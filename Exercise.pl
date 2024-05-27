% Ex1.1: search2
% search2(Elem, List)
% looks for two consecutive occurrences of Elem
search2(E, [E, E|_]).
search2(E, [_|T]) :- search2(E, T).
% search2(a, [b, c, a, a, d, e]).    -> Yes
% search2(a, [b, c, a, d, a, d, e]). -> No

% Ex1.2: search_two
% search_two(Elem, List)
% looks for two occurrences of Elem with any element in between!
search_two(E, [E, _, E|_]).
search_two(E, [_|T]) :- search_two(E, T).
% search_two(a, [b, c, a, a, d, e]).    -> No
% search_two(a, [b, c, a, d, a, d, e]). -> Yes

% Ex1.3: size
% size(List, Size)
% Size will contain the number of elements in List
size([], 0).
size([_|T], Y) :- size(T, X), Y is X + 1.
% size([], 0).	      -> Yes
% size([a, b, c], 3). -> Yes
% size([a, b], X).    -> Yes {X/2}
% size(X, 2).	      -> Yes {X/[_, _]}

% Ex 1.4: sum
% sum(List, Sum)
sum([], 0).
sum([H|T], Y) :- sum(T, X), Y is X + H.
% sum([1, 2, 3], X). -> Yes {X/6}

% Ex1.5: max and min
% minmax(List, Max, Min)
% Max is the biggest element in List
% Min is the smallest element in List
% Suppose the list has at least one element
minmax([H], H, H).
minmax([H|T], MAX, TMIN) :- minmax(T, TMAX, TMIN), H > TMAX, MAX is H.
minmax([H|T], TMAX, MIN) :- minmax(T, TMAX, TMIN), H < TMIN, MIN is H.
minmax([H|T], TMAX, TMIN) :- minmax(T, TMAX, TMIN), H =< TMAX, H >= TMIN.
% minmax([1], X, Y).       	       -> Yes {X/1, Y/1}
% minmax([2, 3, 2, 1, 2, 3, 4], X, Y). -> Yes {X/4, Y/1}

% Ex1.6: sublist
% split(List1, Elements, SubList1, SubList2)
% Splits a list into two sublists based on a given set of elements
split(L, 0, [], L).
split([H|T], N, [H|L1], L2) :- M is N - 1, split(T, M, L1, L2), append(L1, L2, T).
% split([10, 20, 30, 40, 50], 2, L1, L2). -> Yes {L1/[10, 20], L2/[30, 40, 50]}
% split([20, 30, 40], 0, L1, L2)	  -> Yes {L1/[], L2/[20, 30, 40]}

% Ex1.7: rotate
% rotate(List, RotatedList)
% Rotate a list, namely move the first element to the end of the list
rotate([], []).
rotate([H|T], L) :- append(T, [H], L).
% rotate([], L).               -> Yes {L/[]}
% rotate([10, 20, 30, 40], L). -> Yes {L/[20, 30, 40, 10]}

% Ex 1.8: dice
% dice(X)
% Generates all possible outcomes of throwing a dice
dice(X) :- member(X, [1, 2, 3, 4, 5, 6]).
% dice(X) -> Yes {X/1}; Yes {X/2}; ...; Yes {X/6}

% Ex 1.9: three_dice
% three_dice(N, L)
% Generates all possible outcomes of throwing three dices with a total sum of N
three_dice(N, [X, Y, Z]) :- dice(X), dice(Y), dice(Z), N is X + Y + Z.
% three_dice(5, L). -> Yes {L/[1, 1, 3]}; Yes {L/[1, 2, 2]} ...; Yes {L/[3, 1, 1]}

% Ex 2.1: dropAny
% dropAny(?Elem, ?List, ?OutList)
% Drops any occurrence of Elem in List
dropAny(X, [X|T], T).
dropAny(X, [H|Xs], [H|L]) :- dropAny(X, Xs, L).
% dropAny(10, [10, 20, 10, 30, 10], L). -> Yes {L/[20, 10, 30, 10]}; Yes {L/[10, 20, 30, 10]}; {L/[10, 20, 10, 30]}

% Ex 2.1.1: dropFirst
% dropFirst(?Elem, ?List, ?OutList)
% drops only the first occurrence of Elem in List (showing no alternative results)
dropFirst(X, L, O) :- dropAny(X, L, O), !.
% dropFirst(10, [10, 20, 10, 30, 10], L). -> Yes {L/[20, 10, 30, 10]}

% Ex 2.1.2: dropLast
% dropLast(?Elem, ?List, ?OutList)
% Drops only the last occurrence of Elem in List (showing no alternative results)
dropLast(X, [X], []) :- !.
dropLast(X, [H|Xs], [H|L]) :- dropLast(X, Xs, L).
% dropLast(10, [10, 20, 10, 30, 10], L). -> Yes {L/[10, 20, 10, 30]}

% Ex 2.1.3: dropAll
% dropAll(?Elem, ?List, ?OutList)
% Drop all occurrences of Elem in List, returning a single list as a result
dropAll(X, [], []).
dropAll(X, [Y|T], L) :- copy_term(X, Y), dropAll(X, T, L), !.
dropAll(X, [H|Xs], [H|L]) :- dropAll(X, Xs, L).
% dropAll(10, [10, 20, 10, 30, 10], L). -> Yes {L/[20, 30]}

% Ex3.1: fromList
% fromList(+List, -Graph)
% It creates a graph from a list
fromList([_], []).
fromList([H1, H2|T], [e(H1, H2)|L]) :- fromList([H2|T], L).
% fromList([1, 2, 3], [e(1, 2), e(2, 3)]). -> Yes
% fromList([1, 2], [e(1, 2)]).		   -> Yes
% fromList([1], []).			   -> Yes

% Ex3.2: fromCircList
% fromCircList(+List, -Graph)
% It creates a circular graph from a list
fromCircList([H|T], G) :- append([H|T], [H], L), fromList(L, G).
% fromCircList([1, 2, 3], [e(1, 2), e(2, 3), e(3, 1)]). -> Yes
% fromCircList([1, 2], [e(1, 2), e(2, 1)]).		-> Yes
% fromCircList([1], [e(1, 1)]).				-> Yes

% Ex3.3: outDegree
% outDegree(+Graph, +Node, -Deg)
% Deg is the number of edges which start from Node
outDegree([], N, 0).
outDegree([e(H, _)|L], N, D) :- H \= N, outDegree(L, N, D).
outDegree([e(N, _)|L], N, Y) :- outDegree(L, N, D), Y is D + 1.
% outDegree([e(1, 2), e(1, 3), e(3, 2)], 2, 0). -> Yes
% outDegree([e(1, 2), e(1, 3), e(3, 2)], 3, 1). -> Yes
% outDegree([e(1, 2), e(1, 3), e(3, 2)], 1, 2). -> Yes

% Ex3.4: dropNode
% dropNode(+Graph, +Node, -OutGraph)
% Drop all edges starting and leaving from a Node
% Use dropAll defined in Ex2.2
dropNode(G, N, OG) :- dropAll(e(N, _), G, TG), dropAll(e(_, N), TG, OG).
% dropNode([e(1, 2), e(1, 3), e(2, 3)], 1, [e(2, 3)]). -> Yes

% Ex3.5: reaching
% reaching(+Graph, +Node, -List)
% All the nodes that can be reached in 1 step from Node
% Possibly use findall, looking for e(Node, _) combined with member(?Elem,?List)
reaching(G, N, L) :- findall(X, member(e(N, X), G), L).
% reaching([e(1, 2), e(1, 3), e(2, 3)], 1, L). -> Yes {L/[2, 3]}
% reaching([e(1, 2), e(1, 2), e(2, 3)], 1, L). -> Yes {L/[2, 2]}

% Ex3.6: nodes
% nodes(+Graph, -Nodes)
% Create a list of all nodes (no duplicates) in the graph (inverse of fromList)
nonmember(E, []).
nonmember(E, [H|T]) :- H \= E, nonmember(E, T).

prepend_to_set(E, L, L) :- member(E, L), !.
prepend_to_set(E, L, [E|L]) :- nonmember(E, L).

nodes([], []).
nodes([e(H1, H2)|G], L) :-
	nodes(G, L1),
	prepend_to_set(H2, L1, L2),
	prepend_to_set(H1, L2, L).
% nodes([e(1, 2), e(2, 3), e(3, 4)], L). -> Yes {L/[1, 2, 3, 4]}

% Ex3.7: anypath (advanced!!)
% anypath(+Graph, +Node1, +Node2, -ListPath)
% A path from Node1 to Node2
% If there are many path, they are showed 1-by-1
anypath(G, S, F, [e(S, F)]) :- member(e(S, F), G).
anypath(G, S, F, [e(S, N)|P]) :- member(e(S, N), G), anypath(G, N, F, P).
% anypath([e(1, 2), e(1, 3), e(2, 3)], 1, 3, L). -> Yes {L/[e(1, 3)]}; Yes {L/[e(1, 2), e(2, 3)]}

% Ex3.8: allreaching
% allreaching (+Graph, +Node, -List)
% All the nodes that can be reached from Node
% Suppose the graph is NOT circular!
% Use findall and anyPath!
allreaching(G, N, L) :- findall(X, anypath(G, N, X, _), L).
% allreaching([e(1, 2), e(2, 3), e(3, 5)], 1, [2, 3, 5]). -> Yes

% Ex3.9: grid-like nets
% network(+Graph, -StartNode, -FinalNode, -Path)
% Generate all paths from a node to another
network(G, S, F, P) :-
	nodes(G, L),
	member(S, L),
	member(F, L),
	S \= F,
	anypath(G, S, F, P).
% network([e(1, 2), e(2, 3), e(3, 5)], S, F, P). -> Yes {S/1, F/2, P/[e(1, 2)]};
%						    Yes {S/1, F/3, P/[e(1, 2), e(2, 3)]};
%						    Yes {S/1, F/5, P/[e(1, 2), e(2, 3), e(3, 5)]};
%						    Yes {S/2, F/3, P/[e(2, 3)]};
%						    Yes {S/2, F/5, P/[e(2, 5)]};
%						    Yes {S/3, F/5, P/[e(3, 5)]};

% Ex4.1: next
% next(@Table, @Player, -Result, -NewTable)
result([[P, P, P], [_, _, _], [_, _, _]], win(P)) :- P \= n, !.
result([[_, _, _], [P, P, P], [_, _, _]], win(P)) :- P \= n, !.
result([[_, _, _], [_, _, _], [P, P, P]], win(P)) :- P \= n, !.
result([[P, _, _], [P, _, _], [P, _, _]], win(P)) :- P \= n, !.
result([[_, P, _], [_, P, _], [_, P, _]], win(P)) :- P \= n, !.
result([[_, _, P], [_, _, P], [_, _, P]], win(P)) :- P \= n, !.
result([[P, _, _], [_, P, _], [_, _, P]], win(P)) :- P \= n, !.
result([[_, _, P], [_, P, _], [P, _, _]], win(P)) :- P \= n, !.
result([ROW1, _, _], nothing) :- member(n, ROW1), !.
result([_, ROW2, _], nothing) :- member(n, ROW2), !.
result([_, _, ROW3], nothing) :- member(n, ROW3), !.
result([ROW1, ROW2, ROW3], even) :-
	nonmember(n, ROW1),
	nonmember(n, ROW2),
	nonmember(n, ROW3), 
	!.

mark_cell([n], P, [P]) :- !.
mark_cell([n|T], P, [P|T]).
mark_cell([N|T], P, [N|NT]) :- mark_cell(T, P, NT).

mark_rows([R|T], P, [NR|T]) :- mark_cell(R, P, NR).
mark_rows([R|T], P, [R|NT]) :- mark_rows(T, P, NT).

next(T, P, R, NT) :- mark_rows(T, P, NT), result(NT, R).
% next([[n, n, n], [n, n, n], [n, n, n]], x, R, NT). -> Yes, 9 solutions ...
% next([[o, o, x], [x, x, o], [o, o, x]], x, R, NT). -> No

% Ex4.2: game
% game(@Table, @Player, -Result, -TableList)
other_player(x, o).
other_player(o, x).

game(T, _, R, [T]) :- result(T, R), R \= nothing, !.
game(T, P, R, [T|L]) :- next(T, P, _, NT), other_player(P, NP), game(NT, NP, R, L).
% game([[x, o, n], [n, n, n], [n, n, n]], x, R, L). -> Yes ... 3669 solutions
% game([[x, o, x], [o, x, o], [o, x, o]], x, R, L). -> Yes {R/even, L=[[[x, o, x], [o, x, o], [o, x, o]]]}
