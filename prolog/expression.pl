:- load_library('alice.tuprolog.lib.DCGLibrary').

nonvar(V, _) :- var(V).
nonvar(V, T) :- nonvar(V), call(T).

lookup(K, [(K, V) | _], V).
lookup(K, [_ | T], R) :- lookup(K, T, R).

operation(op_add, A, B, R) :- R is A + B, !.
operation(op_subtract, A, B, R) :- R is A - B, !.
operation(op_multiply, A, B, R) :- R is A * B, !.
operation(op_divide, A, B, R) :- R is A / B, !.
operation(op_negate, A, R)  :- R is -A, !.

evaluate(const(Number), Variables, Number) :- number(Number), !.

evaluate(variable(Name), Variables, Result) :-
   atom_chars(Name, [Head | _]),
   lookup(Head, Variables, Result), !.

evaluate(operation(UnaryOp, A), Variables, Result) :-
   evaluate(A, Variables, R),
   operation(UnaryOp, R, Result), !.
   
evaluate(operation(BinaryOp, A, B), Variables, Result) :-
   evaluate(A, Variables, RA),
   evaluate(B, Variables, RB),
   operation(BinaryOp, RA, RB, Result), !.

op_p(op_add) --> ['+'].
op_p(op_subtract) --> ['-'].
op_p(op_multiply) --> ['*'].
op_p(op_divide) --> ['/'].
op_p(op_negate) --> ['n', 'e', 'g', 'a', 't', 'e'].

digits_p([]) --> [].
digits_p([H | T]) -->
    { member(H, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', '-']) },
    [H],
    digits_p(T).

var_p([]) --> [].
var_p([H | T]) -->
    { member(H, ['X', 'Y', 'Z', 'x', 'y', 'z']) },
    [H],
    var_p(T).

is_single_minus(['-']).

infix_expr_p(const(Value)) --> 
	{ nonvar(Value, number_chars(Value, Chars)) },
    digits_p(Chars),
    { Chars = [_ | _], \+ is_single_minus(Chars), number_chars(Value, Chars) }.
    
infix_expr_p(variable(Name)) --> 
    { nonvar(Name, atom_chars(Name, Chars)) },
    var_p(Chars),
    { atom_chars(Name, Chars) }.

infix_expr_p(operation(Op, A, B)) -->
    ['('], infix_expr_p(A), [' '], op_p(Op), [' '], infix_expr_p(B), [')'].

infix_expr_p(operation(Op, A)) -->
    op_p(Op), [' '], infix_expr_p(A).

need_safe_ws_left(A) :- \+ member(A, [' ', '(']).
need_safe_ws_right(A) :- \+ member(A, [' ', ')']).

normalize_form([], _, []).
normalize_form([' '], _, []).
normalize_form([' ' | [A | T]], Prev, RT) :- 
    need_safe_ws_left(Prev), need_safe_ws_right(A), !, 
    T1 = [A | T], 
    normalize_form(T1, Prev, RT1), 
    RT = [' ' | RT1].
normalize_form([' ' | [A | T]], Prev, RT) :- !, T1 = [A | T], normalize_form(T1, Prev, RT).
normalize_form([H | T], Prev, [H | RT]) :- normalize_form(T, H, RT).

infix_str(E, A) :- ground(E), phrase(infix_expr_p(E), C), atom_chars(A, C).
infix_str(E, A) :- atom(A), atom_chars(A, C), normalize_form(C, ' ', C1), phrase(infix_expr_p(E), C1).