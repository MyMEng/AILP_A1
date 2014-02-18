candidate_number(45684).

%     Part A: Querying prolog

% ailp_start_position(P).
q1(ailp_start_position(P)).

% new_pos(p(1,1), e, P).
q2a(new_pos(p(1,1), e, P)).

% new_pos(p(1,1), n, P).
q2b(19).

% m(A) => m(s). | m(e). | m(w). | m(n).
q3([s,e,w,n]).

%     Part B: Visualising search

% command 'w' gives full list
% [p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(3, 3), p(2, 3), p(2, 2), p(3, 2), p(4, 2), p(4, 1), p(3, 1), p(2, 1), p(1, 1)]
q4a([p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(3, 3), p(2, 3), p(2, 2), p(3, 2), p(4, 2), p(4, 1), p(3, 1), p(2, 1), p(1, 1)]).

% ';'
q4b([p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(4, 2), p(4, 1), p(3, 1), p(3, 2), p(3, 3), p(2, 3), p(2, 2), p(2, 1), p(1, 1)]).

% same as q4a
q4c([p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(3, 3), p(2, 3), p(2, 2), p(3, 2), p(4, 2), p(4, 1), p(3, 1), p(2, 1), p(1, 1)]).

% same as q4b
q4d([p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(4, 2), p(4, 1), p(3, 1), p(3, 2), p(3, 3), p(2, 3), p(2, 2), p(2, 1), p(1, 1)]).

%     Part C: Coding questions

% q5a | q5_corner_move/0
q5_corner_move :-
	ailp_start_position(p(A,B)),
	ailp_show_move(p(A,B),p(1,1)),
	ailp_show_move(p(1,1),p(4,1)),
	ailp_show_move(p(4,1),p(1,4)),
	ailp_show_move(p(1,4),p(4,4)),
	% ??
	assignment1_module:ailp_show_complete.
	%% do_command([mower, colour, 1, 1, lighter]),
	%% do_command([mower, move, 1, 1], _Result),
	%% do_command([mower, move, 4, 1], _Result),
	%% do_command([mower, move, 1, 4], _Result),
	%% do_command([mower, move, 4, 4], _Result).
	% reset?
	%% reset,
	%% ailp_start_position(p(1,1)),
	%% ailp_show_move(p(1,1),p(4,1)),
	%% ailp_show_move(p(4,1),p(1,4)),
	%% ailp_show_move(p(1,4),p(4,4)),
	% show complete?
	%% ailp_show_complete.


% q5_corner_move2/0
q5_corner_move2 :-
	ailp_start_position(p(SX,SY)), % get start position
	ailp_grid_size(Size), % get size of board
	% ???
	% if start point is one of corners --- ignore
	ailp_show_move(p(SX,SY),p(1,1)), % move from start to top left corner
	ailp_show_move(p(1,1),p(Size,1)), % move from start to top right corner
	ailp_show_move(p(Size,1),p(1,Size)), % move from start to bottom left corner
	ailp_show_move(p(1,Size),p(Size,Size)), % move from start to top left corner
	% ??
	assignment1_module:ailp_show_complete.

% q6_spiral/1
q6_spiral(Path) :-
	ailp_start_position(p(SX, SY)), % get start position
	ailp_grid_size(Size), % get size of board
	q6_initiate(p(SX, SY), p(NX, NY), Size), 	% check where you are and move to the border if necessary
	q6_move(p(NX, NY), Path),
	true.

% q6_initiate agent to outer position
q6_initiate(p(SX, SY), p(NX, NY), N) :-
	( SX = 1    -> NX = SX, NY = SY
	; SX = N    -> NX = SX, NY = SY
	; SY = 1    -> NX = SX, NY = SY
	; SY = N    -> NX = SX, NY = SY
	; otherwise -> NX is 1, NY is 1
	).

% decide on direction
%% q6_direction(p(SX, SY), D) :-
%% 	ailp_grid_size(Size), % get size of board
%% 	( SX = 1, SY = 2              -> D = cw
%% 	; SX = 1, SY is Size-1        -> D = acw
%% 	; SX = Size, SY = 2           -> D = acw
%% 	; SX = Size, SY is Size-1     -> D = cw
%% 	; SX = 2, SY = 1              -> D = acw
%% 	; SX is Size-1, SY = 1        -> D = cw
%% 	; SX = 2, SY is Size-1        -> D = acw
%% 	; SX is Size-1, SY is Size-1  -> D = cw
%% 	; otherwise                   -> D = cw
%% 	).

% move agent
q6_move(Position, Path) :-
	%% q6_direction(Position, D), % decide on direction
	%% ailp_grid_size(D), % get outermost radius
	q6_d(D), % choose direction to follow
	q6_move(Position, [Position], RewPath, D, 0),
	reverse(RewPath, Path).

q6_move(_, Path, Path, _, _) :-
	q6_complete(Path).
q6_move(Position, PosList, RewPath, D, L) :-
	%% q6_findmove(Position, NewPosition, D), % execute move

	% outermost circles DO
	q6_m(M),
	q6_new_pos(Position, M, NewPosition),
	%% q6_outermost(NewPosition, PosList, D).
	% outermost circles DO
	outer(L, NewPosition),
	\+ memberchk(NewPosition, PosList),
	ailp_show_move(Position, NewPosition),
	q6_move(NewPosition, [NewPosition|PosList], RewPath, D, L),
	true.

%% q6_findmove(Position, NewPosition, cw) :-
%% 	true.
%% q6_findmove(Position, NewPosition, acw) :-
%% 	true.

% can I use this?
q6_complete(L) :- 
	ailp_grid_size(N),
	N2 is N * N,
	length(L,N2),
	ailp_show_complete.

% outermost circles DO
q6_new_pos(p(X,Y), M, p(X1,Y1)) :-
	( M = s -> X1 =  X,    Y1 is Y+1
	; M = n -> X1 =  X,    Y1 is Y-1
	; M = e -> X1 is X+1,  Y1 =  Y
	; M = w -> X1 is X-1,  Y1 =  Y
	),
	X1 >= 1, Y1 >=1,
	ailp_grid_size(N),
	X1 =< N, Y1 =< N. 

%% q6_outermost(p(OX, OY), PosList, p(NX, NY)) :-
%% 	( D = cw  -> X1 =  X,    Y1 is Y+1
%% 	; D = acw -> X1 =  X,    Y1 is Y-1
%% 	; D = e -> X1 is X+1,  Y1 =  Y
%% 	; D = w -> X1 is X-1,  Y1 =  Y
%% 	),
%% 	X1 >= 1, Y1 >=1,
%% 	ailp_grid_size(N),
%% 	X1 =< N, Y1 =< N,
%% 	true.

q6_m(n).
q6_m(s).
q6_m(w).
q6_m(e).

q6_d(cw).
q6_d(acw).
% outermost circles DO




%% findall(C, outer(Outerness, C), L)

outer(Level, C) :-
	ailp_grid_size(N),
	B is 1+Level,
	H is N-Level,
	(
		((X is 1+Level; X is N-Level), give_range(B, H, Y));
		((Y is 1+Level; Y is N-Level), give_range(B, H, X))
	),
	%% findall(A, give_range(N, A), Ly),
	%% memberchk(Y, Ly),
	%% Y >= 1, Nc is N+0, Y =< Nc,
	%% Y is N-Level,
	C = p(X, Y),
	true.

%% give_range(N, A) :-
%% 	give_range(1, N, A),
%% 	true.

give_range(L, _, L).
give_range(L, U, A) :-
	L < U,
	L1 is L+1,
	give_range(L1, U, A),
	true.
