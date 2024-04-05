get_dep(nullptr, 0) :- !.
get_dep(node(_, _, _, _, Dep), Dep).

get_dif(node(L, R, _, _, _), Dif) :-
	get_dep(L, DL),
	get_dep(R, DR),
	Dif is DL - DR.

max(A, B, R) :- A > B, !, R = A.
max(_, B, B).
	
make_node(L, R, Key, Val, node(L, R, Key, Val, Dep)) :-
	get_dep(L, D1),
	get_dep(R, D2),
	max(D1, D2, Dmax),
	Dep is Dmax + 1.
	
repair(node(L, R, Key, Val, Dep), New_tree) :-
	get_dif(node(L, R, Key, Val, Dep), Dif), Dif == -2, !,
	get_dif(R, DifR), left_rotate(node(L, R, Key, Val, Dep), DifR, New_tree).
repair(node(L, R, Key, Val, Dep), New_tree) :-
	get_dif(node(L, R, Key, Val, Dep), Dif), Dif == 2, !,
	get_dif(L, DifL), right_rotate(node(L, R, Key, Val, Dep), DifL, New_tree).
repair(T, T).
	
left_rotate(node(L, node(LR, RR, KeyR, ValR, DepR), Key, Val, Dep), DifR, New_tree) :-
	DifR < 1, !,
	make_node(L, LR, Key, Val, L_new),
	make_node(L_new, RR, KeyR, ValR, New_tree).
left_rotate(node(L, R, Key, Val, Dep), 1, New_tree) :-
	right_rotate(R, 0, R_new),
	make_node(L, R_new, Key, Val, Tree),
	left_rotate(Tree, 0, New_tree).

right_rotate(node(node(LL, RL, KeyL, ValL, DepL), R, Key, Val, Dep), DifL, New_tree) :-
	DifL > -1, !,
	make_node(RL, R, Key, Val, R_new),
	make_node(LL, R_new, KeyL, ValL, New_tree).
right_rotate(node(L, R, Key, Val, Dep), -1, New_tree) :-
	left_rotate(L, 0, L_new),
	make_node(L_new, R, Key, Val, Tree),
	right_rotate(Tree, 0, New_tree).

map_getMax(node(_, nullptr, K, V, _), K, V) :- !.
map_getMax(node(_, R, _, _, _), K, V) :- map_getMax(R, K, V).

map_removeMax(node(L, nullptr, _, _, _), L) :- !.
map_removeMax(node(L, R, K, V, _), Tree) :- 
	map_removeMax(R, R_new),
	make_node(L, R_new, K, V, Tree1),
	repair(Tree1, Tree).

map_build([], nullptr) :- !.

map_build([(K, V) | T], Tree) :- map_build(T, Tree_prev), map_put(Tree_prev, K, V, Tree).

map_get(node(_, _, K, V, _), K, V) :- !.
map_get(node(L, _, K1, V1, _), K, V) :- K < K1, !, map_get(L, K, V).
map_get(node(_, R, _, _, _), K, V) :- map_get(R, K, V).

map_put(nullptr, K, V, node(nullptr, nullptr, K, V, 1)) :- !.
map_put(node(L, R, K, V1, Dep), K, V, node(L, R, K, V, Dep)) :- !.
map_put(node(L, R, K1, V1, _), K, V, Res) :- 
	K < K1, !, 
	map_put(L, K, V, L_new),
	make_node(L_new, R, K1, V1, Res1),
	repair(Res1, Res).
map_put(node(L, R, K1, V1, _), K, V, Res) :- 
	map_put(R, K, V, R_new),
	make_node(L, R_new, K1, V1, Res1),
	repair(Res1, Res).

map_remove(nullptr, _, nullptr) :- !.
map_remove(node(nullptr, nullptr, K, _, _), K, nullptr) :- !.
map_remove(node(nullptr, R, K, _, _), K, R) :- !.
map_remove(node(L, R, K, _, _), K, Res) :- 
	!, map_getMax(L, Kmax, Vmax), map_removeMax(L, L_new),
	make_node(L_new, R, Kmax, Vmax, Res1),
	repair(Res1, Res).
map_remove(node(L, R, K1, V, _), K, Res) :- 
	K < K1, !, 
	map_remove(L, K, L_new),
	make_node(L_new, R, K1, V, Res1),
	repair(Res1, Res).
map_remove(node(L, R, K1, V, _), K, Res) :- 
	map_remove(R, K, R_new),
	make_node(L, R_new, K1, V, Res1),
	repair(Res1, Res).

map_getKeyValCeiling(node(_, _, K, V, _), K, K, V) :- !.
map_getKeyValCeiling(node(L, _, K1, _, _), K, K_minmax, V_minmax) :- 
	K < K1, 
	map_getMax(L, KMax, Vmax),
	KMax >= K, !,
	map_getKeyValCeiling(L, K, K_minmax, V_minmax).
map_getKeyValCeiling(node(L, _, K1, V1, _), K, K_minmax, V_minmax) :- 
	K < K1, !,
	K_minmax = K1,
	V_minmax = V1.
map_getKeyValCeiling(node(_, R, _, _, _), K, K_minmax, V_minmax) :- map_getKeyValCeiling(R, K, K_minmax, V_minmax).

map_getCeiling(Tree, K, V) :- map_getKeyValCeiling(Tree, K, K_minmax, V).

map_putCeiling(Tree, K, V, New_tree) :- 
	map_getKeyValCeiling(Tree, K, K_minmax, V_minmax), !,
	map_put(Tree, K_minmax, V, New_tree).
map_putCeiling(Tree, K, V, Tree).