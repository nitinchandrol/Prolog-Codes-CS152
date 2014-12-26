notmem(A,B) :- \+ member(A,B) .
%% Q.1
notadjacent(X,Y) :- \+ =:=(1,abs(X-Y)).

occupancy(X) :- X = [[sanket,A], [ankush,B], [ashwin,C], [umang,D], [krishna,E]],
	List = [1,2,3,4,5],
	member(A,List),member(B,List),\==(B,A),
	member(C,List),\==(C,A),\==(C,B),
	member(D,List),\==(D,A),\==(D,B),\==(D,C),
	member(E,List),\==(E,A),\==(E,B),\==(E,C),\==(E,D),
	\==(C,5),\==(B,1),\==(D,1),\==(D,5),notadjacent(D,B),
	notadjacent(D,E),A > B.

 %%Q.2statement is function which takes color arrangement returns statement by A,B as 1,2,3.1-knows the colour,2-knows colour is not X,3-can't say anything.
statement([_,C2,C3],Ans) :- Ans = [A,B,_],
	 ( ((==([C2,C3],[red,red]));
	  (==([C2,C3],[yellow,yellow]))) -> A = 2,B = 1 ;
	A = 3,((==(C3,red);==(C3,yellow))-> B = 2;
	      B = 3)).

goal(Color,Combinations) :- Combinations = [A,B,Color],
	List = [red,yellow,green],
	member(A,List),
	member(B,List),
	member(Color,List),
	\==(Combinations,[red,red,red]),
	\==(Combinations,[yellow,yellow,yellow]),
	statement(Combinations,[S1,S2,_]),
	==(S1,3),==(S2,3).


%%  Q.3
%Cp-current position Np-next position R-length ans


tour(Res) :- tourh(pair(1,1),Res,[pair(1,1)],1),write(Res).
validpos(A,B) :- A >= 1,A =< 7,B >= 1,B =< 7.
tourh(_,Q,Z,49) :- Q = Z,!.
tourh(pair(X,Y),Ans,Cans,R) :- A1 is X + 2, A2 is X + 1, A3 is X - 2, A4 is X - 1,
			       B1 is Y + 2, B2 is Y + 1, B3 is Y - 2, B4 is Y - 1,
				List = [[A1,B2],[A1,B4],[A3,B2],
				  [A3,B4],[A2,B1],[A2,B3],[A4,B1],[A4,B3]],
				member([B,C],List),
				notmem(pair(B,C),Cans),validpos(B,C),
				R1 is R + 1,reverse(Cans,F),
				reverse([pair(B,C)|F],D),
				tourh(pair(B,C),Ans,D,R1).

%%   Q.4
safe(Ans) :- state(3,3,left,Ans,[[3,3,left]]),write(Ans).
valid(A,B) :-  A >= 0,A =< 3,B >= 0,B =< 3,
	       (==(A,B);(A > B,==(A,3));(A < B,==(A,0))).
state(0,0,right,[[0,0,right]],_) :- !.
state(M,C,left,[[M,C,left]|Ans1],Cans) :- M1 is M - 2,M2 is M - 1,
	                  C1 is C - 2,C2 is C - 1,
			  List = [[M1,C],[M2,C2],[M,C1],[M2,C],[M,C2]],
			  member([A,B],List),
			  valid(A,B),
			  notmem([A,B,right],Cans),
			  state(A,B,right,Ans1,[[A,B,right]|Cans]).
state(M,C,right,[[M,C,right]|Ans1],Cans) :- M1 is M + 2,M2 is M + 1,
	                  C1 is C + 2,C2 is C + 1,
			  List = [[M1,C],[M2,C2],[M,C1],[M2,C],[M,C2]],
			  member([A,B],List),
			  valid(A,B),
		          notmem([A,B,left],Cans),
                          state(A,B,left,Ans1,[[A,B,left]|Cans]).





























