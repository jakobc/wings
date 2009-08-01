%%
%%  wings_tweak.erl --
%%
%%     A rewrite of wpc_tweak.erl to add Tweak into the Wings core.
%%
%%  Copyright (c) 2009 Richard Jones
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_tweak).

-export([init/0,command/2]).
-export([tweak_event/2,menu/2]).

-import(lists,[member/2,foldl/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-define(L_ALT, 307).
-define(R_ALT, 308).

-include_lib("wings.hrl").
-include_lib("e3d.hrl").

-record(tweak,
    {mode,					% current tweak tool
     magnet,				% true/false
     mag_type=dome,			% magnet type: Type
     mag_rad=1.0,			% magnet influence radius
     cam,					% camera mode
     id,                    % {Id,Elem} mouse was over when tweak began
     ox,oy,					% original X,Y
     cx,cy,					% current X,Y
     st}).					% wings st record (working)

-record(drag,
    {vs,
     pos0,				%Original position.
     pos,				%Current position.
     mag,				%mag record
     mm}).				%original|mirror

-record(mag,
    {orig,			%Orig pos of vertex being moved.
     vs,			%[{V,Pos,Distance,Influence}](not changed while dragging)
     vtab=[]}).		%[{V,Pos}] (latest)


init() ->
    set_default_tweak_prefs(wings_pref:get_value(camera_mode)),
    TweakMagnet = {true, dome, 1.0},  %{magnet on, magnet type, magnet radius}
    wings_pref:set_default(tweak_magnet, TweakMagnet),
    wings_pref:set_default(tweak_help, true),
    wings_pref:set_default(tweak_xyz, [false,false,false]),
    true.

tweak_event(Ev, St) ->
    Prefs = wings_pref:get_value(tweak_prefs),
    case Prefs of
      {inactive,_} ->
        wings:info_line(),
        wings_wm:message_right([]),
        next;
      {active,TweakModes} ->
          Cam = wings_pref:get_value(camera_mode),
          {Mag, MagType, MagR} = wings_pref:get_value(tweak_magnet),
          T = #tweak{mode=TweakModes, magnet=Mag, mag_type=MagType,
                   mag_rad=MagR, cam=Cam, st=St},
          case orddict:find(Cam,TweakModes) of
            {ok,D} ->
                tweak_info_line(Cam, D),
                tweak_magnet_help(),
                handle_tweak_event_0(Ev, T);
            error ->
                set_tweak_prefs(Cam, TweakModes),
                next
          end;
      _ ->
          Cam = wings_pref:get_value(camera_mode),
          set_tweak_prefs(Cam, Prefs),
          next
    end.

handle_tweak_event_0(#mousebutton{button=B, x=X,y=Y, mod=Mod, state=?SDL_PRESSED},
  #tweak{mode=Tm, cam=Cam}=T) when B =< 3 ->
    Ctrl = Mod band ?CTRL_BITS =/= 0,
    Shift = Mod band ?SHIFT_BITS =/= 0,
    Alt = Mod band ?ALT_BITS =/= 0,
    case orddict:find(Cam,Tm) of
      {ok,D} ->
        case orddict:find({B,{Ctrl,Shift,Alt}}, D) of
          {ok, Mode} ->
              handle_tweak_event_1(T#tweak{mode=Mode, ox=X, oy=Y});
          error -> next
        end;
      error -> next
    end;

%% Tweak Adjust Magnet Radius: Alt + Mouse Motion
handle_tweak_event_0(#keyboard{sym=Sym,mod=0},
  #tweak{st=#st{selmode=Selmode,sel=Sel0}=St0}=T0)
  when Sym =:= ?L_ALT, Selmode =/= body; Sym =:= ?R_ALT, Selmode =/= body->
    {_,X,Y} = wings_wm:local_mouse_state(),
    {GX,GY} = wings_wm:local2global(X, Y),
    case wings_pick:do_pick(X,Y,St0) of
      {add, MM, #st{sel=Sel}=St} ->
          Id = get_id(Sel0, Sel),
          wings_wm:grab_focus(),
          wings_io:grab(),
          begin_magnet_adjustment(MM, St),
          T = T0#tweak{id=Id,ox=GX,oy=GY,cx=0,cy=0},
          {seq,push,update_magnet_handler(T)};
      {delete, MM, #st{sel=Sel}} ->
          Id = get_id(Sel0, Sel),
          wings_wm:grab_focus(),
          wings_io:grab(),
          begin_magnet_adjustment(MM, St0),
          T = T0#tweak{id=Id,ox=GX,oy=GY,cx=0,cy=0},
          {seq,push,update_magnet_handler(T)};
      none -> next
    end;
handle_tweak_event_0(_,_) ->
    next.


%% Tweak Select
handle_tweak_event_1(#tweak{mode=select, ox=X, oy=Y, st=St}) ->
    wings_wm:message(mode(select),[]),
    wings_pick:paint_pick(X, Y, St);

%% Tweak Drag
handle_tweak_event_1(#tweak{ox=X, oy=Y, st=#st{sel=Sel0}=St0}=T0) ->
    {GX,GY} = wings_wm:local2global(X, Y),
    case wings_pick:do_pick(X,Y,St0) of
      {add, MM, #st{sel=Sel}=St} ->
        Id = get_id(Sel0, Sel),
        wings_wm:grab_focus(),
        wings_io:grab(),
        begin_drag(MM, St, T0),
        do_tweak(0.0, 0.0, 0.0, 0.0, screen),
        T = T0#tweak{id=Id,ox=GX,oy=GY,cx=0,cy=0},
        {seq,push,update_tweak_handler(T)};
      {delete, MM, #st{sel=Sel}} ->
        Id = get_id(Sel0, Sel),
        wings_wm:grab_focus(),
        wings_io:grab(),
        begin_drag(MM, St0, T0),
        do_tweak(0.0, 0.0, 0.0, 0.0, screen),
        T = T0#tweak{id=Id,ox=GX,oy=GY,cx=0,cy=0},
        {seq,push,update_tweak_handler(T)};
      none -> next
    end.

get_id([],[{Id,El}]) ->  {Id,El};
get_id([{Id,El1}],[{Id,El2}]) ->
    S1 = gb_sets:size(El1),
    S2 = gb_sets:size(El2),
    El = if
      S1 > S2 -> gb_sets:subtract(El1, El2);
      true -> gb_sets:subtract(El2, El1)
    end,
    {Id,El};
get_id([{Id,El}],[]) ->  {Id,El};
get_id(Sel0, Sel) ->
    L1 = length(Sel0),
    L2 = length(Sel),
    [Res] = if
            L1 =:= L2 ->
              [{Id,El1}] = lists:subtract(Sel0, Sel),
              [{Id,El2}] = lists:subtract(Sel, Sel0),
               S1 = gb_sets:size(El1),
               S2 = gb_sets:size(El2),
               El = if
                 S1 > S2 -> gb_sets:subtract(El1, El2);
                 true -> gb_sets:subtract(El2, El1)
               end,
               [{Id,El}];
            L1 < L2 ->
              ordsets:subtract(Sel, Sel0);
            L1 > L2 ->
              ordsets:subtract(Sel0, Sel)
    end,
    Res.

%% Event handler for active tweak tools
update_tweak_handler(T) ->
    wings_draw:update_sel_dlist(),
    wings_wm:dirty(),
    tweak_drag_no_redraw(T).

tweak_drag_no_redraw(T) ->
    {replace,fun(Ev) ->
        handle_tweak_drag_event(Ev, T) end}.

handle_tweak_drag_event(redraw, #tweak{mode=Mode,st=St}=T) ->
    redraw(St),
    mode_message(Mode),
    info_box(help_msg()),
    tweak_drag_no_redraw(T);
handle_tweak_drag_event(#mousemotion{x=X, y=Y},
            #tweak{mode=screen, cx=CX, cy=CY, ox=OX, oy=OY}=T0) ->
    {GX,GY} = wings_wm:local2global(X, Y),
    DX = GX-OX, %since last move X
    DY = GY-OY, %since last move Y
    DxOrg = DX+CX, %total X
    DyOrg = DY+CY, %total Y
    Cs = constraints(),
    Mode = if
      Cs =:= [true,true,false] -> xymove;
      Cs =:= [false,true,true] -> yzmove;
      Cs =:= [true,false,true] -> zxmove;
      Cs =:= [true,false,false]  -> xmove;
      Cs =:= [false,true,false]  -> ymove;
      Cs =:= [false,false,true]  -> zmove;
      true -> screen
    end,
    wings_io:warp(OX,OY),
    do_tweak(DX,DY,DxOrg,DyOrg,Mode),
    T = T0#tweak{cx=DxOrg,cy=DyOrg},
    update_tweak_handler(T);

% Tweak Modes without constraints
handle_tweak_drag_event(#mousemotion{x=X, y=Y},
            #tweak{mode=Mode, cx=CX, cy=CY, ox=OX, oy=OY}=T0) ->
    {GX,GY} = wings_wm:local2global(X, Y),
    DX = GX-OX, %since last move X
    DY = GY-OY, %since last move Y
    DxOrg = DX+CX, %total X
    DyOrg = DY+CY, %total Y

    wings_io:warp(OX,OY),
    do_tweak(DX,DY,DxOrg,DyOrg,Mode),
    T = T0#tweak{cx=DxOrg,cy=DyOrg},
    update_tweak_handler(T);

handle_tweak_drag_event(#keyboard{sym=Sym, mod=0}, #tweak{mode=Mode,
  magnet=true, cx=CX, cy=CY, ox=OX, oy=OY, st=#st{selmode=Selmode}}=T0)
  when Sym =:= ?L_ALT, Selmode =/= body; Sym =:= ?R_ALT, Selmode =/= body->
    {_,X,Y} = wings_wm:local_mouse_state(),
    {GX,GY} = wings_wm:local2global(X, Y),
    DX = GX-OX, %since last move X
    DY = GY-OY, %since last move Y
    DxOrg = DX+CX, %total X
    DyOrg = DY+CY, %total Y

    wings_io:warp(OX,OY),
    do_tweak(DX,DY,DxOrg,DyOrg,Mode),
    T = T0#tweak{cx=DxOrg,cy=DyOrg},
    update_in_drag_radius_handler(T);

%% Catch hotkeys for toggling xyz constraints and magnet attributes
handle_tweak_drag_event(#keyboard{}=Ev, #tweak{mode=Mode, st=St}=T0) ->
    case wings_hotkey:event(Ev,St) of
      next -> keep;
      Action ->
        is_tweak_constraint_hk(Action),
        case is_tweak_magnet_hk(Action) of
          ok ->
            mode_message(Mode),
            keep;
          {Mag,MagType} ->
            mode_message(Mode),
            tweak_magnet_help(),
            T = T0#tweak{magnet=Mag, mag_type=MagType},
            setup_magnet(T),
            update_tweak_handler(T)
        end
    end;

%% Mouse Button released, so we end the drag sequence.
handle_tweak_drag_event(#mousebutton{button=B,state=?SDL_RELEASED}, T) when B =< 3->
    end_drag(T);
handle_tweak_drag_event(_,_) ->
    keep.

%% Event handler for magnet resize
update_magnet_handler(T) ->
    wings_draw:update_sel_dlist(),
    wings_wm:dirty(),
    magnet_event_no_redraw(T).

magnet_event_no_redraw(T) ->
    {replace,fun(Ev) ->
        handle_magnet_event(Ev, T)end}.

handle_magnet_event(redraw, #tweak{magnet=Mag,st=St}=T) ->
    redraw(St),
    tweak_magnet_radius_help(Mag),
    draw_magnet(T),
    magnet_event_no_redraw(T);
handle_magnet_event({new_state,St}, T) ->
    end_magnet_event(T#tweak{st=St});
handle_magnet_event(#mousemotion{mod=Mod, x=X, y=Y},
  #tweak{magnet=true, ox=OX, oy=OY}=T)
  when Mod band ?ALT_BITS =/= 0 ->
    {GX,_} = wings_wm:local2global(X, Y),
    DX = GX-OX, %since last move X
    wings_io:warp(OX,OY),
    update_magnet_handler(adjust_magnet_radius(DX, T));

%% If something is pressed during magnet radius adjustment, save changes
%% and begin new event.
handle_magnet_event(#keyboard{}=Ev,T) ->
    end_magnet_event(Ev,T);
handle_magnet_event(#mousebutton{button=B}=Ev,#tweak{ox=X,oy=Y}=T) when B =< 3 ->
    end_magnet_event(Ev#mousebutton{x=X,y=Y},T);
handle_magnet_event(#mousemotion{},T) ->
    end_magnet_event(T);
handle_magnet_event(_,_) -> keep.

%% In-drag Event handler for magnet resize
update_in_drag_radius_handler(T) ->
    wings_draw:update_sel_dlist(),
    wings_wm:dirty(),
    in_drag_radius_no_redraw(T).

in_drag_radius_no_redraw(T) ->
    {replace,fun(Ev) ->
        handle_in_drag_magnet_ev(Ev, T)end}.

handle_in_drag_magnet_ev(redraw, #tweak{magnet=Mag,st=St}=T) ->
    redraw(St),
    tweak_magnet_radius_help(Mag),
    draw_magnet(T),
    in_drag_radius_no_redraw(T);
handle_in_drag_magnet_ev(#mousemotion{mod=Mod, x=X, y=Y},
  #tweak{magnet=true, ox=OX, oy=OY}=T)
  when Mod band ?ALT_BITS =/= 0 ->
    {GX,_} = wings_wm:local2global(X, Y),
    DX = GX-OX, %since last move X
    wings_io:warp(OX,OY),
    update_in_drag_radius_handler(in_drag_adjust_magnet_radius(DX, T));
handle_in_drag_magnet_ev(Ev, T) ->
    end_in_drag_mag_event(Ev, T).

end_in_drag_mag_event(Ev,#tweak{magnet=Mag, mag_type=MagType, mag_rad=MagR}=T) ->
    wings_pref:set_value(tweak_magnet, {Mag, MagType, MagR}),
    handle_tweak_drag_event(Ev, T).

%% End Magnet adjustment event
end_magnet_event(#tweak{id=Id,st=St}=T) ->
    save_magnet_prefs(T),
    end_magnet_adjust(Id),
    wings_wm:later({new_state,St}),
    pop.
end_magnet_event(Ev,#tweak{id=Id}=T) ->
    save_magnet_prefs(T),
    end_magnet_adjust(Id),
    wings_wm:later(Ev),
    pop.

%% End of event handlers


%% Adjust Magnet Radius
begin_magnet_adjustment(MM, St) ->
    wings_draw:refresh_dlists(St),
    wings_dl:map(fun(D, _) ->
             begin_magnet_adjustment_fun(D, MM)
         end, []).

begin_magnet_adjustment_fun(#dlo{src_sel={Mode,Els},src_we=We}=D, MM) ->
    Vs0 = sel_to_vs(Mode, gb_sets:to_list(Els), We),
    Center = wings_vertex:center(Vs0, We),
    D#dlo{drag=#drag{pos=Center,mm=MM}};
begin_magnet_adjustment_fun(D, _) -> D.

adjust_magnet_radius(MouseMovement, #tweak{mag_rad=Falloff0,st=St}=T0) ->
    case Falloff0 + MouseMovement * 0.01 of
    Falloff when Falloff > 0 ->
        T0#tweak{mag_rad=Falloff,st=St};
    _otherwise -> T0#tweak{st=St}
    end.

in_drag_adjust_magnet_radius(MouseMovement, #tweak{mag_rad=Falloff0,st=St}=T0) ->
    case Falloff0 + MouseMovement * 0.01 of
    Falloff when Falloff > 0 ->
        setup_magnet(T0#tweak{mag_rad=Falloff,st=St});
    _otherwise -> T0#tweak{st=St}
    end.

end_magnet_adjust({OrigId,El}) ->
    wings_dl:map(fun(#dlo{src_we=#we{id=Id}}=D, _) ->
             if OrigId =:= Id -> show_cursor(El,D); true -> ok end,
             D#dlo{vs=none,sel=none,drag=none}
         end, []).

%%%%
begin_drag(MM, St, T) ->
    wings_draw:refresh_dlists(St),
    wings_dl:map(fun(D, _) ->
             begin_drag_fun(D, MM, St, T)
         end, []).

begin_drag_fun(#dlo{src_sel={body,_},src_we=#we{vp=Vtab}=We}=D, _MM, _St, _T) ->
    Vs = wings_util:array_keys(Vtab),
    Center = wings_vertex:center(Vs, We),
    Id = e3d_mat:identity(),
    D#dlo{drag={matrix,Center,Id,e3d_mat:expand(Id)}};

begin_drag_fun(#dlo{src_sel={Mode,Els},src_we=We}=D0, MM, St, T) ->
    Vs0 = sel_to_vs(Mode, gb_sets:to_list(Els), We),
    Center = wings_vertex:center(Vs0, We),
    {Vs,Magnet} = begin_magnet(T, Vs0, Center, We),
    D = wings_draw:split(D0, Vs, St),
    D#dlo{drag=#drag{vs=Vs0,pos0=Center,pos=Center,mag=Magnet,mm=MM}};
begin_drag_fun(D, _, _, _) -> D.

end_drag(#tweak{mode=Mode, id={OrigId,El}, st=St0}) ->
    St = wings_dl:map(fun (#dlo{src_we=#we{id=Id}}=D, St1) ->
                  if OrigId =:= Id -> show_cursor(El,D); true -> ok end,
                  end_drag(Mode, D, St1)
                end, St0),
    wings_wm:later({new_state,St}),
    pop.

end_drag(_, #dlo{src_we=#we{id=Id},drag={matrix,_,Matrix,_}}=D,
        #st{shapes=Shs0}=St0) ->
    We0 = gb_trees:get(Id, Shs0),
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    D1 = D#dlo{src_we=We},
    D2 = wings_draw:changed_we(D1, D),
    {D2#dlo{vs=none,sel=none,drag=none},St};
end_drag(slide_collapse, #dlo{src_sel={_,_},src_we=#we{id=Id}}=D0, #st{shapes=Shs0}=St0) ->
    #dlo{src_we=We} = D = wings_draw:join(D0),
    {Nc,We1} = collapse_short_edges(0.0001,We),
    Shs = gb_trees:update(Id,We1, Shs0),
    St = if
      Nc -> St0#st{shapes=Shs};
      true -> St0#st{shapes=Shs,sel=[]}
    end,
    {D#dlo{vs=none,sel=none,drag=none},St};
end_drag(_, #dlo{src_sel={_,_},src_we=#we{id=Id}}=D0, #st{shapes=Shs0}=St0) ->
    #dlo{src_we=We}=D = wings_draw:join(D0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    {D#dlo{vs=none,sel=none,drag=none},St};
end_drag(_, D, St) -> {D, St}.


%% Setup magnet in the middle of a tweak op
setup_magnet(#tweak{mode=Mode, cx=X, cy=Y}=T) ->
    wings_dl:map(fun(D, _) ->
             setup_magnet_fun(D, T)
         end, []),
    do_tweak(0.0, 0.0, X, Y,Mode),
    T.

setup_magnet_fun(#dlo{src_sel={_,_},drag=#drag{vs=Vs0,pos0=Center}=Drag}=Dl0,
         #tweak{st=St}=T) ->
    We = wings_draw:original_we(Dl0),
    {Vs,Mag} = begin_magnet(T, Vs0, Center, We),
    Dl = wings_draw:split(Dl0, Vs, St),
    Dl#dlo{drag=Drag#drag{mag=Mag}};
setup_magnet_fun(Dl, _) -> Dl.


%%  Additional functions for lookup around vertex
collect_neib_verts_coor(V,We)->
    VertList = wings_vertex:fold(fun(_,_,ERec,Acc) ->
                   [wings_vertex:other(V,ERec)|Acc]
                   end,[],V,We),
    foldl(fun(E,B) -> [wings_vertex:pos(E,We)|B] end,[],VertList).

get_orig_pos(V,We,Vs)->
    Pos=foldl(
      fun({Vert,Coor,_,_,_},P) ->
          if V =:= Vert -> Coor; true-> P end
      end,none,Vs),
    case Pos of
    none -> wings_vertex:pos(V,We);
    _ -> Pos
    end.

collect_neib_verts_coor_vs(V,We,Vs)->
    VertList = wings_vertex:fold(fun(_,_,ERec,Acc) ->
                   [wings_vertex:other(V,ERec)|Acc]
                   end,[],V,We),
    foldl(fun(E,B) -> [get_orig_pos(E,We,Vs)|B] end,[],VertList).

sub_pos_from_list(List,Pos) ->
    foldl(fun
        (E,B) -> [e3d_vec:sub(E,Pos)|B] end,[],List).

relax_vec(V, We) ->
    case collect_neib_verts_coor(V, We) of
    [] ->
        %% Because of hidden faces there may be no neighbouring vertices,
        %% so we default to the position of the vertex itself.
        wings_vertex:pos(V, We);
    Cs ->
        e3d_vec:average(Cs)
    end.

slide_one_vec(Vpos, TweakPos, PosList) ->
    Dpos=e3d_vec:sub(TweakPos,Vpos),
    {Dp,_} = foldl(fun
        ({0.0,0.0,0.0},VPW) -> VPW;
        (Vec, {VP,W}) ->
              Vn = e3d_vec:norm(Vec),
              Dotp = e3d_vec:dot(Vn,Dpos),
              if
                  Dotp > W, Dotp > 0 ->
                      Len = e3d_vec:len(Vec),
                      Dotp2 = if
                          Dotp > Len -> Len;
                          true -> Dotp
                      end,
                      {e3d_vec:mul(Vn, Dotp2),Dotp};
                  true -> {VP,W}
              end
     end,{{0,0,0},0},PosList),
    e3d_vec:add(Vpos,Dp).

slide_vec_w(V, Vpos, VposS, TweakPosS, We, W,Vs) ->
    Dv = e3d_vec:sub(VposS,Vpos),
    TweakPos = e3d_vec:sub(TweakPosS, Dv),
    Cs = sub_pos_from_list(collect_neib_verts_coor_vs(V, We, Vs), Vpos),
    TweakPos2=e3d_vec:add(Vpos, e3d_vec:mul(e3d_vec:sub(TweakPos, Vpos), W)),
    slide_one_vec(Vpos, TweakPos2, Cs).

relax_vec(V, #we{}=We,Pos0,Pos,Weight) ->
    Vec = relax_vec(V,We),
    Len = e3d_vec:dist(Pos0,Pos),
    Len1 = if
        Len > 1 -> 1.0;
        true -> Len
    end,
    D = e3d_vec:sub(Vec,Pos0),
    e3d_vec:add_prod(Pos0, D, Len1 * Weight).

relax_vec_fn(V, #we{}=We,Pos0,Weight) ->
    Vec = relax_vec(V,We),
    D = e3d_vec:sub(Vec,Pos0),
    e3d_vec:add_prod(Pos0, D, Weight).


%% scanning over the mesh to collapse short edges
collapse_short_edges(Tolerance, #we{es=Etab,vp=Vtab}=We) ->
    Short = array:sparse_foldl(
          fun(Edge, #edge{vs=Va,ve=Vb}, A) ->
              case array:get(Va,Vtab) of
          undefined -> A;
              VaPos ->
                  case array:get(Vb,Vtab) of
                  undefined -> A;
                  VbPos ->
                      case abs(e3d_vec:dist(VaPos, VbPos)) of
                      Dist when Dist < Tolerance -> [Edge|A];
                      _Dist -> A
                      end
                  end
              end
          end, [], Etab),
    NothingCollapsed = Short =:= [],
    We1 = wings_collapse:collapse_edges(Short,We),
    {NothingCollapsed, We1}.

%% end of additional geo-functions block

do_tweak(DX, DY, DxOrg, DyOrg, Mode) ->
    wings_dl:map(fun
        (#dlo{src_we=We}=D, _) when ?IS_LIGHT(We) ->
            Allowed = [screen, xmove, ymove, zmove, xymove, yzmove, zxmove],
            case lists:member(Mode,Allowed) of
              true ->
                do_tweak(D, DX, DY, DxOrg, DyOrg, Mode);
              false ->
                do_tweak(D, DX, DY, DxOrg, DyOrg, screen)
            end;
        (D, _) ->
             do_tweak(D, DX, DY, DxOrg, DyOrg, Mode)
         end, []).

do_tweak(#dlo{drag={matrix,Pos0,Matrix0,_},src_we=#we{id=Id}}=D0,
     DX,DY,_,_,Mode) ->
    Matrices = wings_u:get_matrices(Id, original),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    {Tx,Ty,Tz} = TweakPos,
    {Px,Py,Pz} = Pos0,
    Pos = case Mode of
        xmove -> {Tx,Py,Pz};
        ymove -> {Px,Ty,Pz};
        zmove -> {Px,Py,Tz};
        xymove -> {Tx,Ty,Pz};
        yzmove -> {Px,Ty,Tz};
        zxmove -> {Tx,Py,Tz};
        _Other -> TweakPos
    end,
    Move = e3d_vec:sub(Pos, Pos0),
    Matrix = e3d_mat:mul(e3d_mat:translate(Move), Matrix0),
    D0#dlo{drag={matrix,Pos,Matrix,e3d_mat:expand(Matrix)}};
do_tweak(#dlo{drag=#drag{vs=Vs,pos=Pos0,pos0=Orig,mag=Mag0,mm=MM}=Drag,
          src_we=#we{id=Id}=We}=D0, DX, DY, DxOrg, DyOrg, Mode) ->
    Matrices = wings_u:get_matrices(Id, MM),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    [V2|_]=Vs,
    {Tx,Ty,Tz} = TweakPos,
    {Px,Py,Pz} = Pos0,
    {Vtab,Mag} =
    case Mode of
        xmove -> Pos = {Tx,Py,Pz},
             magnet_tweak(Mag0, Pos);
        ymove -> Pos = {Px,Ty,Pz},
             magnet_tweak(Mag0, Pos);
        zmove -> Pos = {Px,Py,Tz},
             magnet_tweak(Mag0, Pos);
        xymove -> Pos = {Tx,Ty,Pz},
              magnet_tweak(Mag0, Pos);
        yzmove -> Pos = {Px,Ty,Tz},
              magnet_tweak(Mag0, Pos);
        zxmove -> Pos = {Tx,Py,Tz},
              magnet_tweak(Mag0, Pos);
        relax -> Pos = relax_vec(V2, We, Pos0, TweakPos, 1.0),
              Len = (abs(DxOrg) + abs(DyOrg)) / 200.0,
              Len1 = case Len > 1 of
                   true -> 1.0;
                   false -> Len
              end,
              relax_magnet_tweak_fn(Mag0, Pos,We,Len1);
        slide -> Pos = TweakPos,
              magnet_tweak_slide_fn(Mag0, We,Orig,TweakPos);
        slide_collapse -> Pos = TweakPos,
              magnet_tweak_slide_fn(Mag0, We,Orig,TweakPos);
        normal -> Pos = tweak_normal(Vs, Pos0, TweakPos, D0),
              magnet_tweak(Mag0, Pos);
        tangent -> Pos = tweak_tangent(Vs, Pos0, TweakPos, D0),
              magnet_tweak(Mag0, Pos);
        _ 	-> Pos = TweakPos,
              magnet_tweak(Mag0, Pos)
    end,
    D = D0#dlo{sel=none,drag=Drag#drag{pos=Pos,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);
do_tweak(D, _, _, _, _, _) -> D.

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    wings_gl:project(X, Y, Z, MVM, PM, VP).

screen_to_obj({MVM,PM,VP}, {Xs,Ys,Zs}) ->
    wings_gl:unProject(Xs, Ys, Zs, MVM, PM, VP).

tweak_tangent( _, Pos0, TweakPos, #dlo{src_we=#we{}=We,src_sel={face,Sel0}}) ->
    Faces = gb_sets:to_list(Sel0),
    Normals = face_normals(Faces,We,[]),
    case Normals of
    [[]] -> TweakPos;
    _Otherwise ->
        N = e3d_vec:average(Normals),
        %% constraining by the plane
        Dot = e3d_vec:dot(N, N),
        if
        Dot =:= 0.0 -> Pos0;
        true ->
            T = -e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
            e3d_vec:add_prod(TweakPos, N, T)
        end
    end;

tweak_tangent(Vs, Pos0, TweakPos, D) ->
    Normals = [vertex_normal(V, D) || V <- Vs],
    N = e3d_vec:average(Normals),
    %% constraining by the plane
    Dot = e3d_vec:dot(N, N),
    if
    Dot =:= 0.0 -> Pos0;
    true ->
        T = -e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
        e3d_vec:add_prod(TweakPos, N, T)
    end.

%%%% Along Average Normal
tweak_normal( _, Pos0, TweakPos, #dlo{src_we=#we{}=We,src_sel={face,Sel0}}) ->
    Faces = gb_sets:to_list(Sel0),
    Normals = face_normals(Faces,We,[]),
    case Normals of
    [[]] -> TweakPos;
    _Otherwise ->
        N = e3d_vec:norm(e3d_vec:add(Normals)),
        %% Return the point along the normal closest to TweakPos.
        Dot = e3d_vec:dot(N, N),
        if
        Dot =:= 0.0 -> Pos0;
        true ->
            T = e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
            e3d_vec:add_prod(Pos0, N, T)
        end
    end;

tweak_normal(Vs, Pos0, TweakPos, D) ->
    Normals = [vertex_normal(V, D) || V <- Vs],
    N = e3d_vec:norm(e3d_vec:add(Normals)),
    %% Return the point along the normal closest to TweakPos.
    Dot = e3d_vec:dot(N, N),
    if
    Dot =:= 0.0 -> Pos0;
    true ->
        T = e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
        e3d_vec:add_prod(Pos0, N, T)
    end.

%% vertex_normal(Vertex, DLO) -> UnormalizedNormal
%%  Calculate the vertex normal. Will also work for vertices surrounded
%%  by one or more hidden faces.
vertex_normal(V, D) ->
    OrigWe = wings_draw:original_we(D),
    FaceNs = [face_normal(F, D) || F <- wings_face:from_vs([V], OrigWe)],
    e3d_vec:add(FaceNs).

%% face_normal(Face, DLO) -> Normal
%%  Calculate the face normal. Will also work for faces that
%%  are hidden (including the virtual mirror face).
face_normal(Face, #dlo{src_we=#we{vp=Vtab}}=D) ->
    #we{vp=OrigVtab} = OrigWe = wings_draw:original_we(D),
    Vs = wings_face:vertices_ccw(Face, OrigWe),
    VsPos = [vertex_pos(V, Vtab, OrigVtab) || V <- Vs],
    e3d_vec:normal(VsPos).

face_normals([Face|Fs], We, Normals) ->
    N = wings_face:normal(Face, We),
    face_normals(Fs, We, [N|Normals]);
face_normals([], _We, Normals) ->
    Normals.

vertex_pos(V, Vtab, OrigVtab) ->
    case array:get(V, Vtab) of
      undefined -> array:get(V, OrigVtab);
      Pos -> Pos
    end.

%% Magnet
begin_magnet(#tweak{magnet=false}=T, Vs, Center, We) ->
    Mirror = mirror_info(We),
    Near = near(Center, Vs, [], Mirror, T, We),
    Mag = #mag{orig=Center,vs=Near},
    {[Va || {Va,_,_,_,_} <- Near],Mag};
begin_magnet(#tweak{magnet=true}=T, Vs, Center, #we{vp=Vtab0}=We) ->
    Mirror = mirror_info(We),
    Vtab1 = sofs:from_external(array:sparse_to_orddict(Vtab0), [{vertex,info}]),
    Vtab2 = sofs:drestriction(Vtab1, sofs:set(Vs, [vertex])),
    Vtab = sofs:to_external(Vtab2),
    Near = near(Center, Vs, Vtab, Mirror, T, We),
    Mag = #mag{orig=Center,vs=Near},
    {[Va || {Va,_,_,_,_} <- Near],Mag}.

near(Center, Vs, MagVs, Mirror, #tweak{mag_rad=R,mag_type=Type}, We) ->
    RSqr = R*R,
    M0 = foldl(fun({V,Pos}, A) ->
              case e3d_vec:dist_sqr(Pos, Center) of
              DSqr when DSqr =< RSqr ->
                  D = math:sqrt(DSqr),
                  Inf = mf(Type, D, R),
                  Matrix = mirror_matrix(V, Mirror),
                  [{V,Pos,Matrix,D,Inf}|A];
              _ -> A
              end;
         (_, A) -> A
          end, [], MagVs),
    M = minus_locked_vs(M0,We),
    foldl(fun(V, A) ->
          Matrix = mirror_matrix(V, Mirror),
          Pos = wpa:vertex_pos(V, We),
          [{V,Pos,Matrix,0.0,1.0}|A]
      end, M, Vs).

minus_locked_vs(MagVs, #we{pst=Pst}) ->
    Mask = wings_pref:get_value(magnet_mask_on),
    case gb_trees:is_defined(wpc_magnet_mask,Pst) of
      true when Mask ->
        LockedVs = gb_sets:to_list(wpc_magnet_mask:get_locked_vs(Pst)),
        remove_masked(LockedVs, MagVs);
      _otherwise ->
        MagVs
    end.

remove_masked([V|LockedVs],MagVs) ->
    remove_masked(LockedVs,lists:keydelete(V,1,MagVs));
remove_masked([],MagVs) -> MagVs.

mf(dome, D, R) when is_float(R) ->
    math:sin((R-D)/R*math:pi()/2);
mf(straight, D, R) when is_float(R) ->
    (R-D)/R;
mf(spike, D0, R) when is_float(R) ->
    D = (R-D0)/R,
    D*D.

magnet_tweak(#mag{orig=Orig,vs=Vs}=Mag, Pos) ->
    Vec = e3d_vec:sub(Pos, Orig),
    Vtab = foldl(fun({V,P0,Plane,_,1.0}, A) ->
               P1 = e3d_vec:add(P0, Vec),
               P = mirror_constrain(Plane, P1),
               [{V,P}|A];
            ({V,P0,Plane,_,Inf}, A) ->
               P1 = e3d_vec:add_prod(P0, Vec, Inf),
               P = mirror_constrain(Plane, P1),
               [{V,P}|A]
         end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

relax_magnet_tweak_fn(#mag{vs=Vs}=Mag, _,We,Weight) ->
    Vtab = foldl(fun({V,P0,Plane,_,1.0}, A) ->
               P1=relax_vec_fn(V,We,P0,Weight),
               P = mirror_constrain(Plane, P1),
               [{V,P}|A];
            ({V,P0,Plane,_,Inf}, A) ->
               P1=relax_vec_fn(V,We,P0,Weight*Inf),
               P = mirror_constrain(Plane, P1),
               [{V,P}|A]
         end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

magnet_tweak_slide_fn(#mag{vs=Vs}=Mag, We,Orig,TweakPos) ->
    Vtab = foldl(fun({V,P0,Plane,_,Inf}, A) ->
             P1=slide_vec_w(V,P0,Orig,TweakPos,We,Inf,Vs),
             P = mirror_constrain(Plane, P1),
             [{V,P}|A]
         end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

draw_magnet(#tweak{st=#st{selmode=body}}) -> ok;
draw_magnet(#tweak{magnet=true, mag_rad=R, mag_type=Mt}) ->
    R2=[get_inv_magnet_value(Mt,X/10.0)||X<-lists:seq(1,9)],
    wings_dl:fold(fun(D, _) ->
        gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
        gl:disable(?GL_DEPTH_TEST),
        gl:enable(?GL_BLEND),
        gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
        wings_view:load_matrices(false),
        gl:color4f(0, 0, 1, 0.06),
        draw_magnet_1(D, R,R2),
        gl:popAttrib()
    end, []);
draw_magnet(_) -> ok.

draw_magnet_1(#dlo{mirror=Mtx,drag=#drag{mm=Side,pos={X,Y,Z}}}, R,R2) ->
    case Side of
    mirror -> gl:multMatrixf(Mtx);
    original -> ok
    end,
    gl:translatef(X, Y, Z),

    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, R, 20, 20),
    glu:deleteQuadric(Obj),
    gl:color4f(0, 0, 1, 0.03),
    lists:foreach(
      fun(R3) ->
          Obj2 = glu:newQuadric(),
          glu:quadricDrawStyle(Obj2, ?GLU_FILL),
          glu:quadricNormals(Obj2, ?GLU_SMOOTH),
          glu:sphere(Obj, R3*R, 20, 20),
          glu:deleteQuadric(Obj2)
      end,R2);

draw_magnet_1(_, _,_) -> [].

get_inv_magnet_value(MagType,Value) ->
    get_inv_magnet_value2(MagType,Value,0.0).

get_inv_magnet_value2(MagType,Value,Pos) ->
    Step=0.1,
    V1=mf(MagType,Pos,1.0),
    V2=mf(MagType,Pos+Step,1.0),
    case Value<V1 andalso Value>=V2 of
      true -> Pos+(V1-Value)/(V1-V2)*Step;
      false -> get_inv_magnet_value2(MagType,Value,Pos+Step)
    end.

mirror_info(#we{mirror=none}) -> {[],none};
mirror_info(#we{mirror=Face}=We) ->
    PlaneNormal = wings_face:normal(Face, We),
    FaceVs = wpa:face_vertices(Face, We),
    Origin = wings_vertex:center(FaceVs, We),
    M0 = e3d_mat:translate(Origin),
    M = e3d_mat:mul(M0, e3d_mat:project_to_plane(PlaneNormal)),
    Flatten = e3d_mat:mul(M, e3d_mat:translate(e3d_vec:neg(Origin))),
    {FaceVs,Flatten}.

mirror_matrix(V, {MirrorVs,Flatten}) ->
    case member(V, MirrorVs) of
    false -> none;
    true -> Flatten
    end.

mirror_constrain(none, Pos) -> Pos;
mirror_constrain(Matrix, Pos) -> e3d_mat:mul_point(Matrix, Pos).

redraw(St) ->
    wings:redraw(St),
    keep.

sel_to_vs(vertex, Vs, _) -> Vs;
sel_to_vs(edge, Es, We) -> wings_vertex:from_edges(Es, We);
sel_to_vs(face, Fs, We) -> wings_face:to_vertices(Fs, We).

save_magnet_prefs(#tweak{magnet=Mag, mag_type=MT, mag_rad=MagR}) ->
    wings_pref:set_value(tweak_magnet, {Mag, MT, MagR}).
%%
%%%% XYZ Tweak Constraints
%%

constraints() ->
    FKeys = fkey_combo(), % held xyz constraints
    TKeys = wings_pref:get_value(tweak_xyz), % Toggled xyz constraints
    add_constraints(FKeys,TKeys,[]).

%% Check for pressed Fkeys
fkey_combo() ->
    F1 = wings_io:is_key_pressed(?SDLK_F1),
    F2 = wings_io:is_key_pressed(?SDLK_F2),
    F3 = wings_io:is_key_pressed(?SDLK_F3),
    [F1,F2,F3].

%% Add or subtract xyz Held constraints from Toggled constraints
add_constraints([true|Fkeys],[false|Tkeys],Constraints) ->
    add_constraints(Fkeys,Tkeys,[true|Constraints]);
add_constraints([true|Fkeys],[true|Tkeys],Constraints) ->
    add_constraints(Fkeys,Tkeys,[false|Constraints]);
add_constraints([_|Fkeys],[Key|Tkeys],Constraints) ->
    add_constraints(Fkeys,Tkeys,[Key|Constraints]);
add_constraints([],[],Constraints) ->
    lists:reverse(Constraints).

%% After releasing lmb to conclude drag, unhide the cursor and make sure its
%% inside the window at the centre of the selection if possible.

show_cursor(_, #dlo{src_we=#we{id=Id}, drag={matrix,Pos,_,_}}) ->
    Matrices = wings_u:get_matrices(Id, original),
    show_cursor_1(Matrices, Pos);
show_cursor(El, #dlo{src_sel={Mode,_},src_we=#we{id=Id}=We,drag=#drag{mm=MM}}) ->
    Vs0 = sel_to_vs(Mode, gb_sets:to_list(El), We),
    Center = wings_vertex:center(Vs0, We),
    Matrices = wings_u:get_matrices(Id, MM),
    show_cursor_1(Matrices, Center).

show_cursor_1(Matrices, Pos) ->
    {W,H} = wings_wm:win_size(),
    {X0,Y0,_} = obj_to_screen(Matrices, Pos),
    {X1,Y1} = {trunc(X0), H - trunc(Y0)},
    X2 = if X1 < 0 -> 20;
            X1 > W -> W-20;
            true -> X1
        end,
    Y2 = if Y1 < 0 -> 20;
            Y1 > H -> H-20;
            true -> Y1
        end,
    {X,Y} = wings_wm:local2global(X2,Y2),
    wings_wm:release_focus(),
    wings_io:ungrab(X,Y).

%% Tweak Menu
menu(X, Y) ->
    Owner = wings_wm:this(),
    Set = [?__(1,"Set Key Binding: Hold modifiers and/or press a mouse button.")],
    Swap = [?__(2,"Swap Bindings: Press another key binding to swap keys with that tool.")],
    Del = [button(3), ?__(3,"Remove binding.")],
    Mode = wings_pref:get_value(tweak_prefs),
    HelpBox = wings_pref:get_value(tweak_help),
    {Toggle, TogHelp} = case Mode of
      {inactive,_} -> {?__(4,"Enable Tweak"),
                       ?__(5,"Tweak is currently disabled; click to enter Tweak mode.")};
      {active,_}   -> {?__(6,"Disable Tweak"),
                       ?__(7,"Tweak is currently enabled; click to exit Tweak mode.")}
    end,
    Menu =
      [{Toggle, toggle_tweak, TogHelp}, separator,
       {?__(17,"Magnets"),{tweak_magnet, tweak_magnet_menu()}},
       {?__(18,"XYZ Constraints"),{constrainXYZ, xyz_constraints_menu()}},
       separator,
       {mode(screen), tweak_menu_fun(screen),
        wings_msg:join([?__(8,"Drag elements relative to the screen."),
        Set, Swap, Del]), crossmark(screen)},
       {mode(normal), tweak_menu_fun(normal),
        wings_msg:join([?__(9,"Drag selection along its average normal."),
        Set, Swap, Del]), crossmark(normal)},
       {mode(relax),tweak_menu_fun(relax),
        wings_msg:join([?__(10,"Relax geometry."),
        Set, Swap, Del]), crossmark(relax)},
       {mode(slide),tweak_menu_fun(slide),
        wings_msg:join([?__(11,"Slide elements along adjacent edges."),
        Set, Swap, Del]), crossmark(slide)},
       {mode(slide_collapse),tweak_menu_fun(slide_collapse),
        wings_msg:join([?__(12,"Slide element and collapse short edges."),
        Set, Swap, Del]), crossmark(slide_collapse)},
       {mode(tangent),tweak_menu_fun(tangent),
        wings_msg:join([?__(13,"Constrain drag to selection's average plane."),
        Set, Swap, Del]), crossmark(tangent)},
       {mode(select),tweak_menu_fun(select),
        wings_msg:join([?__(14,"Paint select."),
        Set, Swap, Del]), crossmark(select)},
       separator,
       {?__(15,"Show Help Box"),show_help_box,
        ?__(16,"The help box appears when any Tweak tool is activated."),
        crossmark(HelpBox)}],
    wings_menu:menu(X, Y, Owner, tweak, Menu).

mode(screen) -> ?__(1,"Move");
mode(normal) -> ?__(2,"Normal");
mode(relax) -> ?__(3,"Relax");
mode(slide) -> ?__(4,"Slide");
mode(slide_collapse) -> ?__(5,"Slide Collapse");
mode(tangent) -> ?__(6,"Tangent Plane");
mode(select) -> ?__(7,"Select").

tweak_menu_fun(Mode) ->
    fun
      (B,_) when B =< 3 -> {tweak,{Mode,B}};
      (_,_) -> ignore
    end.

xyz_constraints_menu() ->
    [Fx,Fy,Fz] = wings_pref:get_value(tweak_xyz),
    F1 = case Fx of
        true -> ?__(1,"Unlock X");
        false -> ?__(2,"Lock X")
    end,
    F2 = case Fy of
        true -> ?__(3,"Unlock Y");
        false -> ?__(4,"Lock Y")
    end,
    F3 = case Fz of
        true -> ?__(5,"Unlock Z");
        false -> ?__(6,"Lock Z")
    end,
    Help = ?__(7,"If assigned, hotkeys can be used to toggle axis locking while Tweak is active."),
    [{F1,x,Help}, {F2,y,Help}, {F3,z,Help}, separator,
     {?__(8,"XYZ Panel"),panel}].

tweak_magnet_menu() ->
    {Mag, MagType, _} = wings_pref:get_value(tweak_magnet),
    {Toggle,Help} = if
      Mag  -> {?__(1,"Disable Magnet"),
               ?__(3,"Magnet soft selection is currently enabled; click to disable Magnets.")};
      true -> {?__(2,"Enable Magnet"),
               ?__(4,"Magnet soft selection is currently disabled; click to enble Magnets.")}
    end,
    Dome = {magnet_type(dome), dome, mag_thelp(dome),
            crossmark({dome, MagType})},
    Straight = {magnet_type(straight), straight, mag_thelp(straight),
                crossmark({straight, MagType})},
    Spike = {magnet_type(spike), spike, mag_thelp(spike),
      crossmark({spike, MagType})},
    [{Toggle, toggle_magnet, Help}, separator, Dome, Straight, Spike].

magnet_type(dome) -> ?__(1,"Dome");
magnet_type(straight) -> ?__(2,"Straight");
magnet_type(spike) -> ?__(3,"Spike").

mag_thelp(dome) -> ?__(1,"This magnet pulls and pushes geometry with an even and rounded effect.");
mag_thelp(straight) -> ?__(2,"This magnet pulls and pushes geometry with a straight effect.");
mag_thelp(spike) -> ?__(3,"This magnet pulls and pushes geometry out to a sharp point.").

magnet_radius() -> ?__(1,"Magnet Radius").

crossmark(true) -> [crossmark];
crossmark({MagType, MagType}) -> [crossmark];
crossmark({_, MagType}) when is_atom(MagType)-> [];
crossmark(Key) ->
    Cam = wings_pref:get_value(camera_mode),
    P = case wings_pref:get_value(tweak_prefs) of
      {inactive, Prefs} -> Prefs;
      {active, Prefs} -> Prefs
    end,
    case orddict:find(Cam,P) of
      {ok,Modes} ->
        case lists:keyfind(Key,2,Modes) of
          false -> [];
          _other -> [crossmark]
        end;
      error -> []
    end.

%% Tweak Command Response
command(toggle_tweak, St) ->
    Prefs = wings_pref:get_value(tweak_prefs),
    NewPrefs = case Prefs of
      {inactive, Modes} -> {active,Modes};
      {active,Modes} -> {inactive, Modes}
    end,
    wings_pref:set_value(tweak_prefs, NewPrefs),
    St;
command({tweak_magnet, toggle_magnet}, St) ->
    magnet_toggle(),
    St;
command({tweak_magnet, MagType}, St) ->
    set_magnet_type(MagType),
    St;
command(show_help_box, St) ->
    wings_pref:set_value(tweak_help, not wings_pref:get_value(tweak_help)),
    St;
command({constrainXYZ, panel}, St) ->
    constraints_menu(St);
command({constrainXYZ, Axis}, St) ->
    set_axis_lock(Axis),
    St;
command({Mode,B}, St) when B =< 3->
    Mod = sdl_keyboard:getModState(),
    Ctrl = Mod band ?CTRL_BITS =/= 0,
    Shift = Mod band ?SHIFT_BITS =/= 0,
    Alt = Mod band ?ALT_BITS =/= 0,
    set_tweak_pref(Mode, B, {Ctrl, Shift, Alt}),
    St;
command(Mode, St) when Mode =:= screen; Mode =:= select; Mode =:= slide;
  Mode =:= slide_collapse; Mode =:= relax; Mode =:= tangent ->
    set_tweak_pref(Mode, 1, {false, false, false}),
    St;
command(_,_) -> next.

%% Delete Tweak bind key
set_tweak_pref(Mode, 3, {false,false,false}) ->
    Cam = wings_pref:get_value(camera_mode),
    {Toggle, Prefs} = wings_pref:get_value(tweak_prefs),
    D0 = case orddict:find(Cam, Prefs) of
      {ok,CamSpecific} -> CamSpecific;
      error -> default_prefs(Cam)
    end,
    D = lists:keydelete(Mode,2,D0),
    NewPrefs = orddict:store(Cam, D, Prefs),
    wings_pref:set_value(tweak_prefs, {Toggle, NewPrefs});
%% Set new Tweak bind key or swap functions if the bind keys already exist
set_tweak_pref(Mode, B, Mods) ->
    Cam = wings_pref:get_value(camera_mode),
    exceptions(Cam,B,Mods),
    {Toggle, Prefs} = wings_pref:get_value(tweak_prefs),
    D0 = case orddict:find(Cam, Prefs) of
      {ok,CamSpecific} -> CamSpecific;
      error -> default_prefs(Cam)
    end,
    NewPrefs = case lists:keyfind(Mode,2,D0) of
      false ->
        orddict:store(Cam, orddict:store({B,Mods}, Mode, D0), Prefs);
      {OldKey,Mode} ->
        D1 = case orddict:find({B,Mods},D0) of
          {ok, OldMode} -> orddict:store(OldKey,OldMode,D0);
          error -> lists:keydelete(Mode,2,D0)
        end,
        D = orddict:store({B,Mods}, Mode, D1),
        orddict:store(Cam, D, Prefs)
    end,
    wings_pref:set_value(tweak_prefs, {Toggle, NewPrefs}).

%% A bind keys that conflict with either menus or camera buttons are listed here
exceptions(wings_cam,2,{false,false,false}) -> cam_conflict();
exceptions(mirai,2,{false,false,false}) -> cam_conflict();
exceptions(nendo,2,{false,false,false}) -> cam_conflict();
exceptions(maya,3,{true,false,false}) -> menu_conflict();
exceptions(maya,_,{false,false,true}) -> cam_conflict();
exceptions(tds,2,{false,false,false}) -> cam_conflict();
exceptions(tds,2,{false,false,true}) -> cam_conflict();
exceptions(tds,2,{true,false,true}) -> cam_conflict();
exceptions(blender,2,{false,false,false}) -> cam_conflict();
exceptions(blender,2,{false,true,false}) -> cam_conflict();
exceptions(blender,2,{true,false,false}) -> cam_conflict();
exceptions(mb,1,{true,true,false}) -> cam_conflict();
exceptions(mb,1,{true,false,false}) -> cam_conflict();
exceptions(mb,1,{false,true,false}) -> cam_conflict();
exceptions(sketchup,2,{false,false,false}) -> cam_conflict();
exceptions(sketchup,2,{false,true,false}) -> cam_conflict();
exceptions(_,_,_) -> ok.


menu_conflict() ->
    wings_u:error(?__(1,"Key combo was not assigned.\n
    Those keys would conflict with the right click Tweak menu.")).

cam_conflict() ->
    wings_u:error(?__(1,"Key combo was not assigned.\n
    That key combination would conflict with the Camera buttons")).


constraints_menu(St) ->
    [TX, TY, TZ] = wings_pref:get_value(tweak_xyz),
    X = wings_s:dir(x),
    Y = wings_s:dir(y),
    Z = wings_s:dir(z),
    Menu =
       [{X,TX},
        {Y,TY},
        {Z,TZ}],
    wings_ask:dialog(?__(1,"Lock..."),Menu,
    fun(Res) -> set_constraint(Res,St) end).

set_constraint(Res, St) ->
    wings_pref:set_value(tweak_xyz,Res),
    St.

set_default_tweak_prefs(Cam) ->
    Prefs = default_prefs(Cam),
    D = orddict:store(Cam, Prefs, orddict:new()),
    wings_pref:set_default(tweak_prefs, {inactive,D}).

%% A bug fix function
set_tweak_prefs(Cam, TweakModes) ->
    D = default_prefs(Cam),
    Prefs = orddict:store(Cam,D,TweakModes),
    wings_pref:set_value(tweak_prefs,{active,Prefs}),
    tweak_info_line(Cam, D),
    tweak_magnet_help().

%% {Bool, Bool, Bool} == {Crtl, Shift, Alt}
default_prefs(maya) ->
    F = false,
    T = true,
    D = [{{1,{F,F,F}}, screen},
         {{1,{T,F,F}}, normal},
         {{2,{F,F,F}}, select}],
    orddict:from_list(D);
default_prefs(mb) ->
    F = false,
    T = true,
    D = [{{1,{F,F,F}}, screen},
         {{2,{T,F,F}}, normal},
         {{2,{F,F,F}}, select}],
    orddict:from_list(D);
default_prefs(_) -> % when wings_cam, blender, sketchup, nendo, mirai, tds
    F = false,
    T = true,
    D = [{{1,{F,F,F}}, screen},
         {{1,{T,F,F}}, normal},
         {{1,{F,F,T}}, select}],
    orddict:from_list(D).

%% Tweak Mode info line
tweak_info_line(Cam, D) ->
    M1 = compose_info_line(D),
    Rmb = button(3),
    TMenu = ?__(2,"Tweak menu"),
    M3 = [Rmb,?__(1,"Menu")],
    M4 = case Cam of
        maya -> [modifier({true,f,f}), Rmb, TMenu];
        _ -> [modifier({f,f,true}), Rmb, TMenu]
    end,
    Message = wings_msg:join([M1,M3,M4]),
    wings_wm:message(Message).

%% Go through list of user defined keys and tweak tool, and build an info line
compose_info_line([{{Button,Modifiers},Mode}|D]) ->
    B = button(Button),
    Mod = modifier(Modifiers),
    M = mode(Mode),
    Message = [Mod,B,M],
    wings_msg:join([Message,compose_info_line(D)]);
compose_info_line([]) -> [].

button(1) -> wings_s:lmb() ++ ": ";
button(2) -> wings_s:mmb() ++ ": ";
button(3) -> wings_s:rmb() ++ ": ".

%% Info box
-record(tb,		% text box
    {str=[],	% string
     ln=1,		% number of lines in string (carriage returns)
     chr=0,		% length of current line
     br=60,		% max line length allowed (break)
     wrd=[], 	% accumulated letters from the current word
     res=[]}).	% Result

-define(SPACE, $ ). % unicode space
-define(NL, $\n). 	% unicode new line
info_box(Text0) ->
    case wings_pref:get_value(tweak_help) of
      true ->
        {W,H} = wings_wm:win_size(),
        CW = ?CHAR_WIDTH,
        LH = ?LINE_HEIGHT,
        Win = W div CW,

        LW = if Win < 40 -> Win; true -> 40 end,

        #tb{ln=Lines,res=Text1} = string_to_text_box(Text0, LW - 2),
        Text = lists:reverse(Text1),

        Bottom = H - LH,
        TextHeight = LH * Lines,
        X = W - LW*CW,

        wings_io:ortho_setup(),
        wings_io:blend(wings_pref:get_value(info_background_color),
          fun(Color) ->
              wings_io:set_color(Color),
              gl:recti(X, Bottom, W, Bottom - (TextHeight + LH div 2))
          end),
        wings_io:set_color(wings_pref:get_value(info_color)),
        wings_io:text_at(X + CW, Bottom - (TextHeight - LH div 2), Text);
      false -> ok
    end.

help_msg() ->
    [?__(1,"Before using a Magnet or Constraint hotkey, first release any modifier keys that are pressed."),
     ?__(2,"As long as the current mouse button is held down, the current tool will remain active.\n\n"),
     ?__(3,"For instance, to change the Magnet Radius, first release any modifier keys, and then press [Alt].")].


%% Formats strings to fit the width of a line length given in characters
string_to_text_box(Info, MaxChar) ->
    str_to_tb(#tb{str=Info, br=MaxChar}).

%% String parsing for Text Box
str_to_tb(#tb{str=[?SPACE|Str], chr=Ch0, br=Ch0, wrd=[]}=Tb) ->
    str_to_tb(Tb#tb{str=Str});

str_to_tb(#tb{str=[?SPACE|Str], chr=Ch0, br=Ch0, wrd=W0, res=R}=Tb) ->
    Word = lists:reverse(W0),
    str_to_tb(Tb#tb{str=Str, wrd=[], res=[Word|R]});

str_to_tb(#tb{str=[?SPACE|Str], chr=Ch, wrd=W0, res=R}=Tb) ->
    Word = lists:reverse(W0),
    str_to_tb(Tb#tb{str=Str, wrd=[], chr=Ch+1, res=[?SPACE,Word|R]});

str_to_tb(#tb{str=[?NL|Str], ln=Ln, chr=Ch0, br=Ch0, wrd=[], res=R}=Tb) ->
    str_to_tb(Tb#tb{str=Str, ln=Ln+1, chr=0, res=[?NL|R]});

str_to_tb(#tb{str=[?NL|Str], ln=Ln, wrd=W0, res=R}=Tb) ->
    Word = lists:reverse(W0),
    str_to_tb(Tb#tb{str=Str, ln=Ln+1, wrd=[], chr=0, res=[?NL, Word|R]});

str_to_tb(#tb{str=[H|T]}=Tb0) when is_list(H) ->
    Tb = str_to_tb(Tb0#tb{str=H}),
    str_to_tb(Tb#tb{str=T});

str_to_tb(#tb{str=[C|Str], ln=Ln, chr=Ch0, br=Ch0, wrd=[], res=R}=Tb) ->
    str_to_tb(Tb#tb{str=Str, ln=Ln+1, chr=1, wrd=[C], res=[?NL|R]});

str_to_tb(#tb{str=[C|Str], ln=Ln, chr=Ch0, br=Ch0, wrd=W, res=R}=Tb) ->
    Word0 = [C|W],
    Ch = length(Word0),
    case Ch >= Ch0 of
      true ->
        [C1|Word1] = W,
        Word = lists:reverse(Word1),
        str_to_tb(Tb#tb{str=Str, ln=Ln+1, wrd=[C,C1], chr=2, res=[?NL, $-, Word|R]});
      false ->
        str_to_tb(Tb#tb{str=Str, ln=Ln+1, wrd=Word0, chr=Ch, res=[?NL|R]})
    end;

str_to_tb(#tb{str=[C|Str], chr=Ch, wrd=W}=Tb) ->
    str_to_tb(Tb#tb{str=Str, wrd=[C|W], chr=Ch+1});

str_to_tb(#tb{str=[], wrd=[]}=Tb) ->
    Tb;
str_to_tb(#tb{str=[], ln=Ln, wrd=W0, chr=Ch0, br=Ch0, res=R}=Tb) ->
    Word = lists:reverse(W0),
    Ch = length(Word),
    case Ch =:= Ch0 of
      true -> Tb#tb{wrd=[], res=[Word|R]};
      false -> Tb#tb{ln=Ln+1, wrd=[], chr=Ch+1, res=[?SPACE,Word,?NL|R]}
    end;
str_to_tb(#tb{str=[], wrd=W0, chr=Ch, res=R}=Tb) ->
    Word = lists:reverse(W0),
    Tb#tb{wrd=[], chr=Ch+1, res=[?SPACE,Word|R]}.


%% XYZ Constraints info line
mode_message(screen) ->
    Help1 = ?__(1,"Constrain drag to X, Y, or Z by holding one or more Fkeys."),
    Help2 = ?__(2,"Bold Fkeys represent Locked Axes (Tweak menu)."),
    Help3 = ?__(3,"Held Fkeys cancel out Locked Axes."),
    Help = wings_msg:join([fkey_help(),Help1,Help2,Help3]),
    wings_wm:message(Help);
mode_message(Mode) -> wings_wm:message(mode(Mode)).

fkey_help() ->
    [Fx,Fy,Fz] = wings_pref:get_value(tweak_xyz),
    F1 = if Fx -> [{bold,"F1"}]; true -> "F1" end,
    F2 = if Fy -> [{bold,"F2"}]; true -> "F2" end,
    F3 = if Fz -> [{bold,"F3"}]; true -> "F3" end,
    ["[",F1,",",F2,",",F3,"]:"].

modifier({Ctrl,Shift,Alt}) ->
    C = if Ctrl -> [wings_s:key(ctrl),"+"]; true -> [] end,
    S = if Shift -> [wings_s:key(shift),"+"]; true -> [] end,
    A = if Alt -> [wings_s:key(alt),"+"]; true -> [] end,
    [C,S,A].

set_axis_lock(Axis) ->
    [X,Y,Z] = wings_pref:get_value(tweak_xyz),
    NewPref = case Axis of
      x -> [not X, Y, Z];
      y -> [X, not Y, Z];
      z -> [X, Y, not Z]
    end,
    wings_pref:set_value(tweak_xyz, NewPref).

%% Info line for tweak magnet
tweak_magnet_help() ->
    {Mag, MagType, _} = wings_pref:get_value(tweak_magnet),
    Message = if
      Mag ->
        M1 = ?__(1,"Magnet: On"),
        M2 = [?__(3,"Type: "),magnet_type(MagType)],
        M3 = [wings_s:key(alt), ": ", magnet_radius()],
        wings_msg:join([M1,M2,M3]);
      true -> ?__(2,"Magnet: Off")
    end,
    wings_wm:message_right(Message).

tweak_magnet_radius_help(true) ->
    wings_wm:message(?__(1,"Drag left or right to adjust the magnet radius."));
tweak_magnet_radius_help(false) ->
    wings_wm:message(?__(2,"Magnet is currently off.")).

magnet_toggle() ->
    {Mag, MagType, MagRad} = wings_pref:get_value(tweak_magnet),
    wings_pref:set_value(tweak_magnet,{not Mag, MagType, MagRad}).

set_magnet_type(MagType) ->
    {Mag, _, MagRad} = wings_pref:get_value(tweak_magnet),
    wings_pref:set_value(tweak_magnet,{Mag, MagType, MagRad}).

% Checking for xyz constraints during screen relative drag
is_tweak_constraint_hk({tweak,{constrainXYZ, panel}}) -> ok;
is_tweak_constraint_hk({tweak,{constrainXYZ, Axis}}) -> set_axis_lock(Axis);
is_tweak_constraint_hk(_) -> ok.

is_tweak_magnet_hk({tweak,{tweak_magnet, toggle_magnet}}) ->
    magnet_toggle(),
    {Mag, MagType, _} = wings_pref:get_value(tweak_magnet),
    {Mag, MagType};
is_tweak_magnet_hk({tweak,{tweak_magnet, MagType}}) ->
    set_magnet_type(MagType),
    {Mag, MagType, _} = wings_pref:get_value(tweak_magnet),
    {Mag, MagType};
is_tweak_magnet_hk(_) -> ok.
