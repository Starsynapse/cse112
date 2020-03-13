
earthRadius(3961).
pi(3.141592653589793).
planeSpeed(500).

show(Code) :-
    airport( Code, City, Nlat, Wlong),
    write( Code), write( ' '), write( City),
    write( ' Nlat='), write( Nlat),
    write( ' Wlong='), write( Wlong), nl.

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
   fail. % Replace fail with ! for a single path and vise versa for many paths

writepath( []) :-
   nl.
writepath( [Head|Tail]) :-
   write( ' '), write( Head), writepath( Tail).

listpath( Node, End, Outlist) :-
   listpath( Node, End, [Node], Outlist).

listpath( Node, Node, _, [Node]).
listpath( Node, End, Tried, [Node|List]) :-
    flight( Node, Next, Time),
    distance(Node, Next, Distance),
    writef("%w %w %w\n", [Node, Next, Distance]),
    writeln(Tried),
    % checkTime(Time),
    not( member( Next, Tried)),
    writeln("Fiesta!"),
    listpath( Next, End, [Next|Tried], List).

checkTime(Time) :-
    writeln(Time),
    arg(1, Time, Hour),
    arg(2, Time, Minute),
    append([[Hour, Minute]], ListA, ListB),
    writeln(ListB).

% on(Item,[Item|Rest]).
% on(Item,[DisregardHead|Tail]):-
%     on(Item,Tail).

nope(N, L) :-
    memberchk(N, L) -> false; true.
    

append([],List,List).
append([Head|Tail],List2,[Head|Result]):-
    append(Tail,List2,Result).

haversine(Lat1, Lat2, Lon1, Lon2, D) :-
    earthRadius(R),
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    A = (sin(Dlat / 2))^2 + cos(Lat1) * cos(Lat2) * (sin(Dlon / 2))^2,
    C = 2 * atan2(sqrt(A), sqrt(1 - A)),
    D is R * C.
    
radians(Location, Rad) :-
    pi(Pi),
    arg(1, Location, Degrees),
    arg(2, Location, Minutes),
    DecimalDegree is Degrees + Minutes / 60,
    Rad is DecimalDegree * Pi / 180.

distance(Code1, Code2, Distance) :-
    airport( Code1, City1, Nlat1, Wlong1),
    airport(Code2, City2, Nlat2, Wlong2),

    radians(Nlat1, LatRad1),
    radians(Nlat2, LatRad2),
    radians(Wlong1, LongRad1),
    radians(Wlong2, LongRad2),
    
    haversine(LatRad1, LatRad2, LongRad1, LongRad2, Distance).

fly(Code1, Code2) :- 
    listpath(sfo, mia, Out),
    writeln(Out),
    distance(Code1, Code2, Distance).
    % writeln(Distance).
