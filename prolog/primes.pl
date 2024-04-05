del_proc(N, Shift, MAX_N) :- 
  N < MAX_N, !,
  assert(composite(N)),
    N1 is N + Shift,
    del_proc(N1, Shift, MAX_N).
del_proc(N, Shift, MAX_N).

calc(N, MAX_N) :- 
  prime(N), !,
  N1 is N * N,
  del_proc(N1, N, MAX_N).
calc(N, MAX_N).

step(N, MAX_N) :- N =< MAX_N, calc(N, MAX_N), N1 is N + 1, step(N1, MAX_N).

init(MAX_N) :- step(2, MAX_N).

prime(N) :- N > 1, \+ composite(N), !.

next_divisor(1, _, _):-!.
next_divisor(X, D1, D_next) :-
		prime(D1),
		0 is mod(X, D1), 
		D_next is D1, !.
next_divisor(X, D1, D_next) :-
    D2 is D1 + 1,
    next_divisor(X, D2, D_next), !.

cons(1, _, []) :- !.
cons(R, Divisor_last, Divisors) :-
		R1 is div(R, Divisor_last),
		next_divisor(R1, Divisor_last, Divisor_last1),
		cons(R1, Divisor_last1, Divisors1),
		append([Divisor_last], Divisors1, Divisors), !.

prime_divisors(X, Divisors) :-
        number(X), !,
        X > 0,
		next_divisor(X, 2, Divisor_first),
		cons(X, Divisor_first, Divisors).

mul(X, [], _) :- X is 1, !.
mul(X, [Divisor_first | Other], Divisor_prev) :- 
        \+ Divisor_prev > Divisor_first,
        mul(X1, Other, Divisor_first),
        X is X1 * Divisor_first, !.

prime_divisors(X, Divisors) :- mul(X, Divisors, 1), !.
    