
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
   listpath( Node, End, [Node], Outlist, [time(0,0)]).

listpath( Node, Node, _, [f(Node,na(0,0),na(0,0))], _).
listpath( Node, End, Tried, [f(Node,Time,Next,Arrival)|List], Times) :-
    flight( Node, Next, Time),
    not( member( Next, Tried)),
    % writef("%w %w %w %w\n", [Node, Next, Time, Distance]),
    % writeln(Tried),
    checkDeparture(Time, Times),
    distance(Node, Next, Distance),
    computeArrival(Time, Distance, Arrival),
    % writeln(Arrival),
    % Soup = [Time|Times],
    % writeln(Soup),
    % writeln(Node),
    listpath( Next, End, [Next|Tried], List, [Time|Times]).

checkDeparture(Time, [Last|Times]) :-
    % writef("%w %w %w\n", [Time, Last, Times]),
    % writeln(Times),
    arg(1, Time, HourNow),
    arg(2, Time, MinuteNow),
    arg(1, Last, HourLast),
    arg(2, Last, MinuteLast),
    compareTime(HourNow, HourLast).

compareTime(T1, T2) :-
    T1 >= T2.

computeArrival(Departure, Distance, Arrival) :-
    arg(1, Departure, DepHour),
    arg(2, Departure, DepMinute),
    planeSpeed(Speed),
    FlightTime is Distance / Speed,
    AddHours is floor(FlightTime),
    M is FlightTime - AddHours,
    AddMinutes is M * 60,
    ArrivalH is DepHour + AddHours,
    ArrivalM is floor(DepMinute + AddMinutes),
    Arrival = time(ArrivalH, ArrivalM).


% on(Item,[Item|Rest]).
% on(Item,[DisregardHead|Tail]):-
%     on(Item,Tail).
    

% append([],List,List).
% append([Head|Tail],List2,[Head|Result]):-
%     append(Tail,List2,Result).

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

printEverything(R, I) :- I =< 1, !.
printEverything([F|R], Int) :-
    % writeln(Int),
    arg(1, F, Code),
    arg(2, F, Departure),
    arg(3, F, DestinationCode),
    arg(4, F, Arrival),
    % writef("%w %w %w %w\n", [Code, Departure, DestinationCode, Arrival]),
    airport( Code, Name, Nlat, Wlong),
    airport( DestinationCode, Name2, Nlat2, Wlong2),
    % writeln("AAAAAA"),
    arg(1, Departure, Dhour),
    arg(2, Departure, Dmin),
    print_flight('depart', Code, Name, Dhour, Dmin),
    arg(1, Arrival, Ahour),
    arg(2, Arrival, Amin),
    print_flight('arrive', DestinationCode, Name2, Ahour, Amin),
    I is Int - 1,
    printEverything(R, I), !.

pad2( Num, Padded) :- Num < 10, string_concat( '0', Num, Padded).
pad2( Num, Num).

print_flight( Activity, Code, Name, Hour, Min) :-
    write( Activity), write( '  '),
    string_upper( Code, Upper),
    write( Upper), write( ' '), write( Name), write( ' '),
    pad2( Hour, Hour2), write( Hour2), write( ':'),
    pad2( Min, Min2), write( Min2), nl.

test_print :-
    print_flight( 'depart', lax, 'Los Angeles     ', 14, 22),
    print_flight( 'arrive', sfo, 'San Francisco   ', 15, 29),
    print_flight( 'depart', sfo, 'San Francisco   ', 16, 02),
    print_flight( 'arrive', sea, 'Seattle-Tacoma  ', 17, 42).



fly(Code1, Code2) :-
    listpath(Code1, Code2, Out),
    % writeln(Out),
    length(Out, Int),
    % writeln(Int),
    printEverything(Out, Int), !.
