%max returns the maximum value of a list.
max([],0).
max(List,M):- List = [M|Tail], max(Tail, N), M>N.
max(List,M):- List =[N|Tail], max(Tail, M), M>N.

% towers of hanoi problem, where N is number of discs, and ML is a list
% of moves where each move is a list in the format: [disc number, pole
% from, pole to].

hanoi(0,[]).
hanoi(1,[[1,1,3]]).
hanoi(1,[[1,3,2]]).
hanoi(1,[[1,1,2]]).
hanoi(1,[[1,2,3]]).
hanoi(1,[[1,2,1]]).
hanoi(1,[[1,3,1]]).

hanoi(N,ML):- N>1, 0 is N mod 2, N1 is N-1, hanoi(N1, ML1), hanoi(N1, ML3),
	ML1 = [M1|ML1Tail], M1 = [1,A,C],
	ML2 = [M|ML3], M = [N,A,B],
	ML3 = [M3|ML3Tail], M3 = [1,C,B],
	A\=B, B\=C, C\=A,
	append(ML1, ML2, ML).


hanoi(N,ML):- N>1, 1 is N mod 2, N1 is N-1, hanoi(N1, ML1), hanoi(N1, ML3),
	ML1 = [M1|ML1Tail], M1 = [1,A,C],
	ML2 = [M|ML3], M = [N,A,C],
	ML3 = [M3|ML3Tail], M3 = [1,B,A],
	A\=B, B\=C, C\=A,
	append(ML1, ML2, ML).

