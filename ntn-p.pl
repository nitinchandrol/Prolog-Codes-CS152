per([],[]).
per([A|X],Y) :- per(X,L),remo(A,Y,L).
remo(X,[X|Y],Y).
remo(X,[A|Y],[A|Z]) :- remo(X,Y,Z).
notmemb(X,Y) :- \+ member(X,Y).
adjacent(X,Y) :- List = [1,2,3,4,5],member(X,List),member(Y,List),=:=(1,abs(X - Y)).

oropr(true,true) :-  true.
oropr(true,false) :- true.
oropr(false,true) :- true.
oropr(false,false) :- false.

%%goal(Color,Combinations) :- Combinations = [A,B,Color],List = [r,y,g],
	%%member(A,List),member(B,List),member(Color,List),
	%%\==(Combinations,[r,r,r]),\==(Combinations,[y,y,y]),
	%%Combinations = [_|L],
	%%\==(L,[r,r]),\==(L,[y,y]),\==(Color,r),\==(Color,y).
 s(X,Y) :- ==(X,0),Y =

