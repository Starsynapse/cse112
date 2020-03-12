% $Id: graphpaths.pl,v 1.5 2020-02-26 16:38:39-08 - - $ */

%
% Define the links in the graph.
%

link( a, b).
link( a, d).
link( b, c).
link( d, e).
link( e, c).
link( e, f).
link( f, a).
link( f, g).
link( f, j).
link( g, h).
link( h, i).
link( i, j).

%
% Prolog version of not.
%

not( X) :- X, !, fail.
not( _).

%
% Is there a path from one node to another?
%

ispath( L, L, _).
ispath( L, M, Path) :-
   link( L, X),
   not( member( X, Path)),
   ispath( X, M, [L|Path]).


%
% Find a path from one node to another.
%

writeallpaths( Node, Node) :-
   write( Node), write( ' is '), write( Node), nl.
writeallpaths( Node, Next) :-
   listpath( Node, Next, [Node], List),
   write( Node), write( ' to '), write( Next), write( ' is '),
   writepath( List),
   fail.

writepath( []) :-
   nl.
writepath( [Head|Tail]) :-
   write( ' '), write( Head), writepath( Tail).

listpath( Node, End, Outlist) :-
   listpath( Node, End, [Node], Outlist).

listpath( Node, Node, _, [Node]).
listpath( Node, End, Tried, [Node|List]) :-
   link( Node, Next),
   not( member( Next, Tried)),
   listpath( Next, End, [Next|Tried], List).


% TEST: writeallpaths(a,e).
% TEST: writeallpaths(a,j).
