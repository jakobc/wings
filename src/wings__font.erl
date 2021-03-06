%%
%%  wings__font.erl --
%%
%%     Generic font operations.
%%
%%  Copyright (c) 2005-2009 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings__font, [Key,Desc,Width,Height,GlyphTab,Bitmaps]).
-export([key/0,desc/0,width/0,width_fun/0,height/0,draw/1,char/1,bold/1]).

draw([C|T]) ->
    char(C),
    draw(T);
draw([]) -> ok.

bold([C|T]) ->
    bold_char(C),
    bold(T);
bold([]) -> ok.

key() -> Key.
desc() -> Desc.

width_fun() -> fun cw/1.

cw(C) ->
    try
	ets:lookup_element(GlyphTab, C, 6)
    catch
	error:badarg ->
	    Width
    end.

width() -> Width.
height() -> Height.

bold_char(C) ->
    Glyph = glyph_info(C),
    draw_glyph(Glyph),
    Cw = glyph_width(Glyph),
    gl:bitmap(1, 1, 0, 0, -Cw+1, 0, <<0>>),
    draw_glyph(Glyph).

char(C) ->
    draw_glyph(glyph_info(C)).

draw_glyph({W,H,Xorig,Yorig,Xmove,B}) ->
    gl:bitmap(W, H, Xorig, Yorig, Xmove, 0, B).

glyph_info(C) ->
    case ets:lookup(GlyphTab, C) of
	[] when is_integer(C), C > 0 ->
	    %% Undefined character. Return a filled box.
	    NumBytes = ((Width+7) div 8) * Height,
	    B = <<(-1):NumBytes/unit:8>>,
	    {Width,Height,0,0,Width+1,B};
	[{_,W,H,Xorig,Yorig,Xmove,Offset}] ->
	    %% Valid character.
	    NumBytes = ((W+7) div 8)*H,
	    <<_:Offset/binary,B:NumBytes/binary,_/binary>> = Bitmaps,
	    {W,H,Xorig,Yorig,Xmove,B}
    end.

glyph_width({_,_,_,_,Xmove,_}) -> Xmove.

	    

    
