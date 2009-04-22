%%
%%  wings_pref.erl --
%%
%%     Preference management.
%%
%%  Copyright (c) 2001-2008 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_pref).
-export([init/0,finish/0,load/0,
	 lowpass/2,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 delete_value/1,
	 get_scene_value/0,get_scene_value/1,get_scene_value/2,
	 set_scene_value/2,set_scene_default/2,
	 delete_scene_value/0,delete_scene_value/1]).

-define(NEED_ESDL, 1).	  %% Some keybindings
-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3,reverse/1,sort/1,last/1,foldl/3]).

-define(MAC_PREFS, "Library/Preferences/Wings 3D Preferences.txt").
-define(WIN32_OLD_PREFS, "Preferences").
-define(WIN32_PREFS, "Wings3D/Preferences.txt").

init() ->
    ets:new(wings_state, [named_table,public,ordered_set]),
    ets:new(wings_delayed_update, [named_table,public,ordered_set]),
    ets:new(wings_scene_prefs, [named_table,public,ordered_set]),
    ets:insert(wings_state, defaults()).

load() ->
    case old_pref_file() of
	none ->
	    %% No preference file found. We must turn
	    %% on the advanced menus.
	    set_value(advanced_menus, true),
	    ok;
	PrefFile ->
	    io:format("Reading preferences from: ~s\n", [PrefFile]),
	    case file:consult(PrefFile) of
		{ok,List0} ->
		    List = clean(List0),
		    catch ets:insert(wings_state, List),
		    no_more_basic_menus(),
		    start_in_tweak();
		{error,_Reason} ->
		    ok
	    end
    end.

start_in_tweak() ->
    case get_value(start_in_tweak) of
      true ->
        self() ! {external, launch_tweak},
        ok;
      false ->
        ok
    end.

no_more_basic_menus() ->
    %% If Wings is launched the first time, the setting of
    %% advanced_menus will determine whether an information
    %% dialog should be shown. At subsequent launch, the
    %% value of no_basic_menu_info decides.
    case get_value(advanced_menus) andalso get_value(no_basic_menu_info) of
	true ->
	    %% The user either already used advanced menus or
	    %% (s)he has chosen to turn off the informtional dialog.
	    ok;
	false ->
	    %% Either this is the first launch and the user used
	    %% the basic menus, or (s)he has still not turned off
	    %% the informational dialog.
	    set_value(no_basic_menu_info, false),
	    self() ! {external,no_more_basic_menus},
	    ok
    end,

    %% Advanced menus are now always on. We must force this value
    %% to true until we have cleaned away all code that test it.
    set_value(advanced_menus, true).

finish() ->
    win32_save_maximized(),
    foreach(fun({Key,Val}) ->
		    set_value(Key, Val)
	    end, ets:tab2list(wings_delayed_update)),
    PrefFile = new_pref_file(),
    List0 = ets:tab2list(wings_state),
    List = prune_defaults(List0),
    Format = "~p. \n",
    PostProcess = case os:type() of
		      {win32,_} -> fun insert_crs/1;
		      _ -> fun(L) -> L end
		  end,
    Write = fun({{bindkey,_},_,default}) -> [];
	       ({{bindkey,_},_,plugin}) -> [];
	       ({{bindkey,_,_},_,default}) -> [];
	       ({{bindkey,_,_},_,plugin}) -> [];
	       (Else) -> PostProcess(io_lib:format(Format, [Else]))
	    end,
    Str = lists:map(Write, List),
    catch file:write_file(PrefFile, Str),
    ok.

lowpass(X, Y) ->
    case get_value(jumpy_camera) of
	false -> {X,Y};
	true -> {lowpass(X),lowpass(Y)}
    end.

lowpass(N) when N > 0 -> lowpass_1(N);
lowpass(N) -> -lowpass_1(-N).

lowpass_1(N) when N =< 7 -> N;
lowpass_1(N) when N =< 15 -> (N-7)*0.6 + lowpass_1(7);
lowpass_1(N) when N =< 30 -> (N-15)*0.5 + lowpass_1(15);
lowpass_1(N) when N =< 50 -> (N-30)*0.05 + lowpass_1(30);
lowpass_1(_) -> lowpass_1(50).

%% Insert CRs into a deep list to produce a correct Windows
%% text file.
insert_crs([H|T]) -> [insert_crs(H)|insert_crs(T)];
insert_crs([]) -> [];
insert_crs($\n) -> "\r\n";
insert_crs(C) when is_integer(C) -> C.

prune_defaults(List) ->
    List -- defaults().

win32_save_maximized() ->
    case os:type() of
	{win32,_} ->
	    set_value(win32_start_maximized, sdl_video:wm_isMaximized());
	_ ->
	    ok
    end.

%%%
%%% Search for a pre-existing preference file.
%%%

old_pref_file() ->
    case os:type() of
	{unix,darwin} ->
	    case try_location(os:getenv("HOME"), ?MAC_PREFS) of
		none -> unix_pref();
		File -> File
	    end;
	{unix,_} -> unix_pref();
	{win32,_} -> win32_pref()
    end.

unix_pref() ->
    try_location(os:getenv("HOME"), ".wings").

win32_pref() ->
    {ok,R} = win32reg:open([read]),
    Res = win32_pref_1(R, ["AppData","Personal"]),
    ok = win32reg:close(R),
    Res.

%% Search for a preference file in "special folders", such as "AppData"
%% and "My Documents".
win32_pref_1(R, [FolderType|T]) ->
    case win32_special_folder(R, FolderType) of
	none -> win32_pref_1(R, T);
	Path ->
	    case try_location(Path, ?WIN32_PREFS) of
		none -> win32_pref_1(R, T);
		File -> File
	    end
    end;
win32_pref_1(R, []) ->
    case try_location(wings_util:lib_dir(wings), ?WIN32_PREFS) of
	none -> win32_pref_2(R);
	File -> File
    end.

%% No preferences found so far. Search in old installations of
%% Wings for preference files.
win32_pref_2(R) ->
    case win32_9816(R) of
	none -> win32_pref_pre9816(R);
	File -> File
    end.

win32_9816(R) ->
    %% Search for a preference file in a Wings installation in 0.98.16.
    %% (Too bad... in a special place in this release only.)
    case win32reg:change_key(R, "\\hklm\\SOFTWARE\\Wings 3D") of
	ok ->
	    case win32reg:sub_keys(R) of
		{ok,SubKeys0} ->
		    SubKeys = reverse(sort(SubKeys0)),
		    {ok,Curr} = win32reg:current_key(R),
		    win32_9816_1(R, SubKeys, Curr);
		{error,_Error} ->
		    %% Can't read sub keys - strange.
		    none
	    end;
	{error,_Error} ->
	    %% No 'Wings 3D' key (this is STRANGE)
	    none
    end.

win32_9816_1(R, [K|Keys], Curr) ->
    ok = win32reg:change_key(R, K),
    Dir = reg_get_default(R),
    ok = win32reg:change_key(R, Curr),
    WingsDirs = filelib:wildcard(Dir++"/lib/wings-*"),
    case try_locations(WingsDirs, ?WIN32_OLD_PREFS) of
	none -> win32_9816_1(R, Keys, Curr);
	File -> File
    end;
win32_9816_1(_, [], _) -> none.

win32_pref_pre9816(R) ->
    %% Search for a preference file in a Wings installation older than 0.98.16
    %% using the uninstall string.
    case win32reg:change_key(R, "\\hklm\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Wings 3D") of
	ok ->
	    case win32reg:value(R, "UninstallString") of
		{ok,Str0} ->
		    Str = strip_quotes(Str0),
		    try_location(filename:dirname(Str), ?WIN32_OLD_PREFS);
		{error,_} -> none
	    end;
	{error,_} -> none
    end.

win32_special_folder(R, FolderType) ->
    Key = "\\hkcu\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",
    case win32reg:change_key(R, Key) of
	ok ->
	    case win32reg:value(R, FolderType) of
		{error,_} -> none;
		{ok,Value} -> Value
	    end;
	_ -> error
    end.

reg_get_default(R) ->
    %% There seems to be a bug in win32reg:value/2 preventing
    %% us from retrieving the default value. Workaround follows.
    case win32reg:values(R) of
	{ok,Values} ->
	    case keysearch(default, 1, Values) of
		{value,{default,Val}} -> Val;
		false -> ""
	    end;
	{error,_} -> ""
    end.

strip_quotes([$"|T0]) ->
    case reverse(T0) of
	[$"|T] -> reverse(T);
	_ -> T0
    end;
strip_quotes(S) -> S.

%%%
%%% Return a suitable path for a new preference file.
%%%

new_pref_file() ->
    case os:type() of
	{unix,darwin} ->
	    filename:join(os:getenv("HOME"), ?MAC_PREFS);
	{unix,_} ->
	    filename:join(os:getenv("HOME"), ".wings");
	{win32,_} ->
	    win32_new_pref()
    end.

win32_new_pref() ->
    {ok,R} = win32reg:open([read]),
    Res = win32_new_pref_1(R, ["AppData","Personal"]),
    ok = win32reg:close(R),
    Res.

win32_new_pref_1(R, [FolderType|T]) ->
    case win32_special_folder(R, FolderType) of
	none -> win32_new_pref_1(R, T);
	Path ->
	    File = filename:join(Path, ?WIN32_PREFS),
	    filelib:ensure_dir(File),
	    File
    end;
win32_new_pref_1(_, []) ->
    %% Desperate fallback... (no standard folders found - should not happen).
    filename:join(wings_util:lib_dir(wings), ?WIN32_PREFS).

%%%
%%% Utilities.
%%%

try_locations([D|Ds], File) ->
    case try_location(D, File) of
	none -> try_locations(Ds, File);
	Name -> Name
    end;
try_locations([], _) -> none.

try_location(Dir, File) ->
    Name = filename:join(Dir, File),
    case filelib:is_file(Name) of
	true -> Name;
	false -> none
    end.

get_value(Key) ->
    get_value(Key, undefined).

get_value(Key, Default) ->
    case ets:lookup(wings_state, Key) of
	[] -> Default;
	[{Key,Val}] -> Val
    end.

set_value(Key, Value) ->
    ets:insert(wings_state, {Key,Value}),
    ok.

set_default(Key, Value) ->
    case ets:member(wings_state, Key) of
	true -> ok;
	false ->
	    ets:insert(wings_state, {Key,Value}),
	    ok
    end.

delete_value(Key) ->
    ets:delete(wings_state, Key),
    ok.


%%% Scene prefs

get_scene_value() ->
    ets:tab2list(wings_scene_prefs).

get_scene_value(Key) ->
    get_value(Key, undefined).

get_scene_value(Key, Default) ->
    case ets:lookup(wings_scene_prefs, Key) of
	[] -> Default;
	[{Key,Val}] -> Val
    end.

set_scene_value(Key, Value) ->
    true = ets:insert(wings_scene_prefs, {Key,Value}),
    ok.

set_scene_default(Key, Value) ->
    case ets:member(wings_scene_prefs, Key) of
	true -> ok;
	false ->
	    ets:insert(wings_scene_prefs, {Key,Value}),
	    ok
    end.

delete_scene_value() ->
    true = ets:delete_all_objects(wings_scene_prefs),
    ok.

delete_scene_value(Key) ->
    true = ets:delete(wings_scene_prefs, Key),
    ok.

%%% End of scene prefs


defaults() ->
    [
     %% Put any non-constant preferences here.
     {jumpy_camera,os:type() =:= {unix,darwin}},

     %% Advanced menus are always turned on now.
     %% The default must still be false for compatibility
     %% with older Wings versions. (We force it to true
     %% later after laoding the user's preference file.)
     {advanced_menus,false},
     {no_basic_menu_info,true},

     %% The remaining items are constants. The generated code
     %% will be much more compact.
     {background_color,{0.8,0.8,0.8}},
     {info_color,{0.0,0.0,0.0}},
     {info_background_color,{0.8,0.8,0.8,0.5}},
     {info_enhanced_text,false},
     {info_text_on_hilite,false},
     {grid_color,{0.7,0.7,0.7}},
     {edge_color,{0.0,0.0,0.0}},
     {hard_edge_color,{1.0,0.5,0.0}},
     {selected_color,{0.65,0.0,0.0}},
     {unselected_hlite,{0.0,0.65,0.0}},
     {selected_hlite,{0.70,0.70,0.0}},
     {x_color,{0.6,0.0,0.0}},
     {y_color,{0.0,0.6,0.0}},
     {z_color,{0.0,0.0,0.6}},
     {neg_x_color,{0.6,0.6,0.6}},
     {neg_y_color,{0.6,0.6,0.6}},
     {neg_z_color,{0.6,0.6,0.6}},

     {vertex_size,4.0},
     {selected_vertex_size,5.0},
     {vertex_color,{0.0,0.0,0.0}},
     {masked_vertex_size,8.0},
     {masked_vertex_color,{0.5,1.0,0.0,0.8}},
     {edge_width,2.0},
     {selected_edge_width,2.0},
     {hard_edge_width,2.0},
     {show_axis_letters,true},
     {constrain_axes,true},
     {mini_axis,true},
     {force_show_along_grid,false},
     {force_ortho_along_axis,false},
     {vertex_hilite,true},
     {edge_hilite,true},
     {face_hilite,true},
     {body_hilite,true},
     {auto_rotate_angle,1.0},
     {auto_rotate_delay,60},
     {autosave,true},
     {autosave_time,10},
     {active_vector_size,1.0},
     {active_vector_width,2.0},
     {active_vector_color,{0.0,0.0,0.65}},
     {normal_vector_size,0.3},
     {normal_vector_width,2.0},
     {normal_vector_color,{0.0,0.0,0.65}},
     {smart_highlighting,false},
     {material_default,{1.0,1.0,1.0}},
     {material_hole,{0.0,0.0,0.9}},

     {selection_style,solid},
     {hide_sel_while_dragging,false},
     {hide_sel_in_camera_moves,false},
     {negative_height,24},
     {negative_width,36},

     %% Compatibility preferences.
     {text_display_lists,true},
     {dummy_axis_letter,false},
     {polygon_offset_f,1.0},
     {polygon_offset_r,1.0},

     %% Advanced features.
     {default_commands,false},
     {use_temp_sel,false},
     {use_super_temp_sel,false},
     {use_mirror_for_sels,true},
     {clip_plane_color,{0.8,0.3,0.0}},
     {clip_plane_size,1.5},
     {highlight_aim_at_selected,false},
     {highlight_aim_at_unselected,true},
	 {hilite_select,false},
     {start_in_tweak, false},

     %% Constraints preferences.
     {con_dist_alt,10.0},
     {con_dist_shift,1.0},
     {con_dist_ctrl,0.10},
     {con_dist_ctrl_shift,0.010},
     {con_dist_ctrl_alt,0.0010},
     {con_dist_shift_alt,0.250},
     {con_dist_ctrl_shift_alt,0.0250},

     {con_dist_a_alt,1/64},
     {con_dist_a_shift,1/4},
     {con_dist_a_ctrl,1/8},
     {con_dist_a_ctrl_shift,1/32},
     {con_dist_a_ctrl_alt,5/8},
     {con_dist_a_shift_alt,1/16},
     {con_dist_a_ctrl_shift_alt,3/64},

     {con_rot_alt,180.0},
     {con_rot_shift,15.0},
     {con_rot_ctrl,1.0},
     {con_rot_ctrl_shift,0.10},
     {con_rot_ctrl_alt,22.50},
     {con_rot_shift_alt,135.0},
     {con_rot_ctrl_shift_alt,144.0},

     {con_scale_alt,10.0},
     {con_scale_shift,1.0},
     {con_scale_ctrl,0.10},
     {con_scale_ctrl_shift,0.010},
     {con_scale_ctrl_alt,0.0010},
     {con_scale_shift_alt,0.250},
     {con_scale_ctrl_shift_alt,0.0250},

     {con_alternate,false},
     {con_dist_set,con_dist_},

     {con_dist_default,alt},
     {con_rot_default,alt},
     {con_scale_default,alt},

     %% Drag preferences.
     {drag_resets,false},
     {drag_custom,false},
     {drag_cam_dist_abs,true},
     {drag_speed_abs,5.0},
     {drag_cam_dist_relative,true},
     {drag_speed_relative,5.0},
     {drag_speed_rotate,5.0},

     %% Proxy preferences.
     {proxy_shaded_edge_style,some},
     {proxy_static_opacity,1.0},
     {proxy_moving_opacity,1.0},

     %% User interface preferences.
     {menu_text,{0.0,0.0,0.0}},
     {menu_hilite,{0.0,0.0,0.5}},
     {menu_hilited_text,{1.0,1.0,1.0}},
     {menu_color,{0.75,0.75,0.75,0.99}},
     {dialog_text,{0.0,0.0,0.0}},
     {dialog_disabled,{0.5,0.5,0.5}},
     {dialog_color,{0.75,0.75,0.75,0.99}},
     {title_active_color,{0.41,0.55,0.41,1.0}},
     {title_passive_color,{0.325,0.4,0.325,1.0}},
     {title_text_color,{1.0,1.0,1.0}},
     {menu_bar_bg,{0.52,0.52,0.52}},
     {info_line_bg,{0.52,0.52,0.52}},
     {no_progress_bar,false},
     {interface_icons,bluecube},
     {objects_in_outliner,true},
     {aa_edges,false},
     {extended_toolbar,true},

     %% Console
     {console_width,80},
     {console_height,12},
     {console_save_lines,100},
     {console_color,{0.0,0.0,0.0}},
     {console_text_color,{0.0,1.0,0.0}},
     {console_cursor_color,{1.0,1.0,1.0}},

     %% Undos.
     {num_undo_levels,32},

     %% Languages.
     {lang,en}
    ].

clean(List) ->
    clean(List, []).

clean([{Key,Val}=Pair|T], Acc) ->
    case not_bad(Key, Val) of
	true -> clean(T, [Pair|Acc]);
	false ->
	    io:format("Removed pref: ~p\n", [Pair]),
	    clean(T, Acc)
    end;
clean([{{bindkey,Key},{vector,{pick,[],Res0,Ns}},Type}|T], Acc) ->
    Res = list_to_tuple(reverse(Res0)),
    Mode = last(Ns),
    Bk = {{bindkey,Mode,Key},build_command(Res, Ns),Type},
    clean(T, [Bk|Acc]);
clean([{{bindkey,_}=Bk,{view,{virtual_mirror,Cmd}},user}|T], Acc) ->
    clean(T, [{Bk,{tools,{virtual_mirror,Cmd}},user}|Acc]);
clean([{{bindkey,_}=Bk,{tools,tweak},user}|T], Acc) ->
    clean(T, [{Bk,{tools,{tweak,false}},user}|Acc]);
clean([{{bindkey,_},Cmd,user}=Bk|T], Acc) ->
    case bad_command(Cmd) of
	false -> clean(T, [Bk|Acc]);
	true ->
	    io:format("Removed pref: ~p\n", [Bk]),
	    clean(T, Acc)
    end;
clean([{{bindkey,_,_},Cmd,user}=Bk|T], Acc) ->
    case bad_command(Cmd) of
	false -> clean(T, [Bk|Acc]);
	true ->
	    io:format("Removed pref: ~p\n", [Bk]),
	    clean(T, Acc)
    end;
clean([H|T], Acc) ->
    clean(T, [H|Acc]);
clean([], Acc) -> Acc.

%% First, get rid of obsolete stuff.
not_bad(last_point, _) -> false;
not_bad(default_point, _) -> false;
not_bad(smooth_preview, _) -> false;
not_bad(wire_mode, _) -> false;
not_bad(none, _) -> false;
not_bad(use_front_buffer, _) -> false;
not_bad(one_button_mouse, _) -> false;
not_bad(face_color, _) -> false;
not_bad(workmode, _) -> false;
not_bad(orthogonal_view, _) -> false;
not_bad(show_memory_used, _) -> false;
not_bad(show_axes, _) -> false;
not_bad(show_groundplane, _) -> false;
not_bad(current_view, _) -> false;
not_bad(camera_fov, _) -> false;
not_bad(camera_hither, _) -> false;
not_bad(camera_yon, _) -> false;
not_bad(right_click_sel_in_ss, _) -> false;
not_bad(right_click_sel_in_geom, _) -> false;
not_bad(wire_edge_color, _) -> false;
not_bad(show_wire_backfaces, _) -> false;	%Now a window property.
not_bad(smoothed_preview_cage, _) -> false;
not_bad(smoothed_preview_edges, _) -> false;
not_bad(contour,_) -> false;

%% Crashes have occurred.
not_bad(last_axis, Val) -> is_wings_vector(Val);
not_bad(default_axis, Val) -> is_wings_vector(Val);
not_bad(magnet_radius, Val) -> is_number(Val);
not_bad(_, _) -> true.

is_wings_vector({{Px,Py,Pz},{Vx,Vy,Vz}})
  when is_number(Px), is_number(Py), is_number(Pz),
       is_number(Vx), is_number(Vy), is_number(Vz) ->
    true;
is_wings_vector(_) -> false.

bad_command({_,{rotate,Atom}}) when is_atom(Atom) -> true;
bad_command({view,virtual_mirror}) -> true;
bad_command({select,edge_loop}) -> true;
bad_command({select,select_region}) -> true;
bad_command({select,edge_ring}) -> true;
bad_command({select,prev_edge_loop}) -> true;
bad_command({select,next_edge_loop}) -> true;
bad_command(_) -> false.

build_command(Name, Names) ->
    foldl(fun(N, A) -> {N,A} end, Name, Names).
