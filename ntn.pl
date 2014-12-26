

son(dhritarashtra, vichitravirya).
son(pandu, vichitravirya).

son(bheem, pandu).
son(arjun, pandu).

brother(X,Y) :- son(X,A), son(Y,A), \==(X,Y).
uncle(X, Y) :- brother(X, A), son(Y, A).
uncle(a,b).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

myappend([],L,L).
myappend([A|L1], L2, [A|L3]) :- myappend(L1,L2,L3).

perm([], []).
perm([X|Y], L) :- perm(Y, L1), remove(X, L, L1).

remove(X, [X|Y], Y).
remove(X, [A|Y], [A|Z]) :- remove(X,Y,Z).


factorial(0, 1) :- !.
factorial(N, M) :- N1 is (N - 1), factorial(N1, M1), M is (N * M1).

choose(List, 1, [X] ) :- !, member(X, List).
choose(List, R , List) :- length(List, R), !.
choose([X|List], R, List1) :- length([X|List], Y), R <  Y,  choose(List, R, List1).
choose([X|List], R, [X|List1]) :- R1 is R - 1, choose(List, R1, List1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Different types of equality

% == two terms are equal
%?- ==(2*2, 2*2).
%true.
%?- ==([1,2], [1,2]).
%true.
%?- ==(S, [1,2,3]). S does not have a value
%false.
%?- =(S,[1,2,3]), ==(S,[1,2,3]). S has been made equal by =, now it has a value
%S = [1, 2, 3].


% = two terms unify, i.e. can be made equal
% ?-=(S, [1,2,3]).
% S = [1, 2, 3].
% ?- =([1,2], [1,2]).
% true.
% ?- =(2*2, 4).
% false.

% =:= Two numeric expressions evaluate to the same value
% ?- =:=(2*2, 4).
% true.
% ?- =:=(S, [1,2,3]).
% ERROR: =:=/2: Arguments are not sufficiently instantiated

%%%% Does not unify%%%%%%%%%%%%%%%
g(X,Y) :-  \=(X,Y).

% g(2,3) is false

% =(f(g(X), h(b, g(h(c, d))), Y), f(g(h(W,Y)), X, g(Z))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


solve(S,E,N,D,M,O,R,Y) :- member(S, [1,2,3,4,5,6,7,8,9]),
	                  member(E, [1,2,3,4,5,6,7,8,9,0]),
			  \==(E,S),
			  member(N, [1,2,3,4,5,6,7,8,9,0]),
			  \==(N,E), \==(N,S),
			  member(D, [1,2,3,4,5,6,7,8,9,0]),
			  \==(D,N), \==(D,E), \==(D,S),
			  member(M, [1,2,3,4,5,6,7,8,9]),
			  \==(M,D), \==(M,N), \==(M,E), \=(M,S),
	                  member(O, [1,2,3,4,5,6,7,8,9,0]),
			  \==(O,M), \==(O,D), \==(O,N), \==(O,E), \=(O,S),
	                  member(R, [1,2,3,4,5,6,7,8,9,0]),
			  \==(R,O), \==(R,M), \==(R,D), \=(R,N), \==(R,E), \=(R,S),
	                  member(Y, [1,2,3,4,5,6,7,8,9,0]),
			  \==(Y,R), \==(Y,O), \==(Y,M), \==(Y,D),\==(Y,N), \==(Y,E), \==(Y,S),
			  =:=((1000 * S) + (100 * E) + (10 * N) + D +
			   (1000 * M) + (100 * O) + (10 * R) + E,
			    (10000 * M) + (1000 * O) + (100 * N) + (10 * E) + Y).


%%%%%%%%%%%%%%%%%%%%% The Einstein puzzle %%%%%%%%%%%%%%%%%%%%%%%%

% There are  five consecutive  houses, each of  a different  color and
% inhabited  by  men  of  different  nationalities. They  each  own  a
% different pet, have a different favorite drink and drive a different
% car.

%   1. The Englishman lives in the red house.
%   2. The Spaniard owns the dog.
%   3. Coffee is drunk in the green house.
%   4. The Ukrainian drinks tea.
%   5. The green house is immediately to the right of the ivory
%      house.
%   6. The Porsche driver owns snails.
%   7. The Masserati is driven by the man who lives in the yellow
%      house.
%   8. Milk is drunk in the middle house.
%   9. The Norwegian lives in the first house on the left.
%  10. The man who drives a Saab lives in the house next to the man
%      with the fox.
%  11. The Masserati is driven by the man in the house next to the
%      house where the horse is kept.
%  12. The Honda driver drinks orange juice.
%  13. The Japanese drives a Jaguar.
%  14. The Norwegian lives next to the blue house.


left_right(L,R,[L,R,_,_,_]).
left_right(L,R,[_,L,R,_,_]).
left_right(L,R,[_,_,L,R,_]).
left_right(L,R,[_,_,_,L,R]).

next_to(X,Y,L) :- left_right(X,Y,L).
next_to(X,Y,L) :- left_right(Y,X,L).

%  each house is structured like this
%   [Color,Nationality,Car,Drink,Pet]
%   S is the list of 5 houses



arrangement(S) :-   S = [[_,norwegian,_,_,_],_,[_,_,_,milk,_],_,_],
	      next_to([_,norwegian,_,_,_],[blue,_,_,_,_],S),
	      member([green,_,_,coffee,_],S),
	      left_right([ivory,_,_,_,_],[green,_,_,_,_],S),
	      member([red,englishman,_,_,_],S),
	      member([_,ukranian,_,tea,_],S),
	      member([yellow,_,masserati,_,_],S),
	      member([_,_,honda,orange_juice,_],S),
	      member([_,japanese,jaguar,_,_],S),
	      member([_,spaniard,_,_,dog],S),
	      next_to([_,_,masserati,_,_],[_,_,_,_,horse],S),
	      member([_,_,porsche,_,snails],S),
	      next_to([_,_,saab,_,_],[_,_,_,_,fox],S),
	      member([_,_,_,_,zebra], S).

goal(Who) :- arrangement(S), member([_,Who,_,_,zebra], S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The bridge crossing problem

time(a, 1).
time(b, 2).
time(c, 5).
time(d, 10).

%state(crossings, pair(people-in-leftbank, people-in-rightbank), where-is-the-torch, totaltime)

% read the predicate  state like this: starting from  a state in which
% the  people are  in  the banks  as  indicated the  torch  is in  the
% position indicated, and the totaltime  spent so far is as indicated,
% Now for  the crossings given,  can you reach  the goal in  which all
% guys are on the right and the total time is <= 17

state([], pair([], RB), r, 17) :- member(a, RB), member(b, RB),
                                  member(c, RB), member(d, RB).

state([pair([X,Y],rl) | Crossing], pair(LB, RB), r, T) :- member(X, RB), delete(RB,X,RB1),
                                                         member(Y, RB1), delete(RB1,Y,RB2),
                                                         =(LB1, [X|[Y|LB]]), time(X, TX),
                                                         time(Y, TY), T1 is T + max(TX,TY),
                                                         17 >= T1,
                                                         state(Crossing, pair(LB1,RB2), l, T1).

state([pair([X],rl) | Crossing], pair(LB, RB), r, T) :- member(X, RB),
                                                        delete(RB, X, RB1),
                                                        =(LB1,[X|LB]),
                                                        time(X, TX),
                                                        T1 is T + TX, 17 >= T1,
                                                        state(Crossing, pair(LB1,RB1), l, T1).

state([pair([X],lr) | Crossing], pair(LB, RB), l, T) :- member(X, LB),
                                                       delete(LB, X, LB1),
                                                        =(RB1, [X|RB]),
                                                       time(X, TX), T1 is T + TX, 17 >= T1,
                                                       state(Crossing, pair(LB1,RB1), r, T1).


state([pair([X,Y],lr) | Crossing], pair(LB, RB), l, T) :- member(X, LB), delete(LB, X,LB1),
                                                         member(Y, LB1), delete(LB1, Y,LB2),
                                                         =(RB1, [X|[Y|RB]]),
                                                         time(X, TX), time(Y, TY),
                                                         T1 is T + max(TX,TY), 17 >= T1,
                                                         state(Crossing, pair(LB2,RB1), r, T1).

goal1(X) :- state(X, pair([a,b,c,d],[]), l ,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%  Examples of cuts %%%%%%%%%%%%%%%%%%%%%%%

q(a).
q(c).
q(d).
r(c) :- !.
r(d).
p(X) :- q(X), !,  r(X).

p(a).
p(b).

z(c) :- !, false.
z(c).



%% q(X) :- t(X), !.
%% q(X) :- s(X).

%% s(d).

%% t(c).
%% t(d).

%% r(d).

%% p(X) :- q(X),  r(X).

%% p(a).
%% p(b).

s(X,Y) :- q(X,Y).
s(0,0).

q(X,Y) :- i(X), i(Y).

i(_).
i(1).
i(2).
j(1).
j(2).
j(3).



