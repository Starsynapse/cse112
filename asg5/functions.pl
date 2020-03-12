
earthRadius(X) :- X = 3961.

show(Code) :-
    airport( Code, City, Nlat, Wlong),
    write( Code), write( ' '), write( City),
    write( ' Nlat='), write( Nlat),
    write( ' Wlong='), write( Wlong), nl.

link(bos, nyc).
link(dfw, den).
link(atl, lax).
link(chi, den).
link(mia, atl).
link(sfo, lax).
link(sea, den).
link(nyc, chi).
link(sea, lax).
link(den, dfw).
link(sjc, lax).
link(atl, lax).
link(atl, mia).
link(chi, nyc).
link(lax, atl).
link(lax, sfo).
link(lax, sjc).
link(nyc, bos).
link(bos, nyc).
link(den, chi).
link(dfw, den).
link(mia, atl).
link(sjc, lax).
link(lax, sea).
link(chi, den).
link(lax, nyc).
link(sfo, lax).
link(atl, lax).
link(lax, atl).
link(nyc, chi).
link(nyc, lax).
link(den, dfw).
link(lax, sjc).
link(chi, nyc).
link(lax, atl).
link(lax, sfo).
link(nyc, bos).
link(sfo, lax).
link(sjc, lax).
link(atl, mia).
link(den, chi).
link(lax, sjc).
link(lax, sfo).
link(lax, sea).

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

fly(Code1, Code2) :- 
    airport( Code1, City1, Nlat1, Wlong1),
    arg(1, Nlat1, Nlat1X),
    arg(2, Nlat1, Nlat1Y),
    arg(1, Wlong1, Wlong1X),
    arg(2, Wlong1, Wlong1Y),

    airport( Code2, City2, Nlat2, Wlong2),
    arg(1, Nlat2, Nlat2X),
    arg(2, Nlat2, Nlat2Y),
    arg(1, Wlong2, Wlong2X),
    arg(2, Wlong2, Wlong2Y),

    writeln(Nlat1X),
    writeln(Nlat1Y),
    writeln(Wlong1X),
    writeln(Wlong1Y),
    earthRadius(X),
    writeln(X).
    
