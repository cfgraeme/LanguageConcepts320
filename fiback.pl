fib(1, 1).
fib(2, 1).
fib(N, FN) :-  N>2,
	Y is N-2, fib(Y, FY),
	X is N-1, fib(X, FX),
	FN is FX+FY.

ack(0,Y,A) :- A is Y+1.
ack(X,0,A) :- X2 is X-1, ack(X2,1,A2),
	A is A2.
ack(X,Y,A) :- X>0, Y>0,
	Y2 is Y-1, ack(X, Y2, A2),
	Y3 is A2,
	X2 is X-1, ack(X2, Y3, A3),
	A is A3.