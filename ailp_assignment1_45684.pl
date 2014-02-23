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
q4b([p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(3, 3), p(2, 3), p(2, 2), p(3, 2), p(3, 1), p(4, 1), p(4,2)]).

% same as q4a
q4c([p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(3, 3), p(2, 3), p(2, 2), p(3, 2), p(4, 2), p(4, 1), p(3, 1), p(2, 1), p(1, 1)]).

% same as q4b
q4d([p(1, 2), p(1, 3), p(1, 4), p(2, 4), p(3, 4), p(4, 4), p(4, 3), p(4, 2), p(4, 1), p(3, 1), p(3, 2), p(3, 3), p(2, 3), p(2, 2), p(2, 1), p(1, 1)]).

%     Part C: Coding questions

% q5a | q5_corner_move/0
q5_corner_move :-
	ailp_start_position(p(A,B)),
	(\+ ((A = 1, B = 1); (A = 1, B = 4); (A = 4, B = 1); (A = 4, B = 4))),
	ailp_show_move(p(A,B),p(1,1)),
	ailp_show_move(p(1,1),p(4,1)),
	ailp_show_move(p(4,1),p(1,4)),
	ailp_show_move(p(1,4),p(4,4)),
	assignment1_module:ailp_show_complete.

%%%%%%%%%%%%%???
q5_corner_move :-
	ailp_start_position(p(1,1)),
	ailp_show_move(p(1,1),p(4,1)),
	ailp_show_move(p(4,1),p(1,4)),
	ailp_show_move(p(1,4),p(4,4)),
	assignment1_module:ailp_show_complete.
q5_corner_move :-
	ailp_start_position(p(1,4)),
	ailp_show_move(p(1,4),p(1,1)),
	ailp_show_move(p(1,1),p(4,1)),
	ailp_show_move(p(4,1),p(4,4)),
	assignment1_module:ailp_show_complete.
q5_corner_move :-
	ailp_start_position(p(4,1)),
	ailp_show_move(p(4,1),p(1,1)),
	ailp_show_move(p(1,1),p(1,4)),
	ailp_show_move(p(1,4),p(4,4)),
	assignment1_module:ailp_show_complete.
q5_corner_move :-
	ailp_start_position(p(4,4)),
	ailp_show_move(p(4,4),p(4,1)),
	ailp_show_move(p(4,1),p(1,1)),
	ailp_show_move(p(1,1),p(1,4)),
	assignment1_module:ailp_show_complete.
%%%%%%%%%%%%%???


% q5_corner_move2/0
q5_corner_move2 :-
	ailp_start_position(p(A,B)), % get start position
	ailp_grid_size(Size), % get size of board
	(\+( (A = 1, B = 1); (A = 1, B = Size); (A = Size, B = 1); (A = Size, B = Size))),
	% if start point is one of corners --- ignore
	ailp_show_move(p(A,B),p(1,1)), % move from start to top left corner
	ailp_show_move(p(1,1),p(Size,1)), % move from start to top right corner
	ailp_show_move(p(Size,1),p(1,Size)), % move from start to bottom left corner
	ailp_show_move(p(1,Size),p(Size,Size)), % move from start to top left corner
	assignment1_module:ailp_show_complete.

%%%%%%%%%%%%%%?????
q5_corner_move2 :-
	ailp_start_position(p(1,1)), % get start position
	ailp_grid_size(Size), % get size of board
	% if start point is one of corners --- ignore
	ailp_show_move(p(1,1),p(Size,1)), % move from start to top right corner
	ailp_show_move(p(Size,1),p(1,Size)), % move from start to bottom left corner
	ailp_show_move(p(1,Size),p(Size,Size)), % move from start to top left corner
	assignment1_module:ailp_show_complete.
q5_corner_move2 :-
	ailp_grid_size(Size), % get size of board
	ailp_start_position(p(1,Size)), % get start position
	% if start point is one of corners --- ignore
	ailp_show_move(p(1,Size),p(1,1)), % move from start to top right corner
	ailp_show_move(p(1,1),p(Size,1)), % move from start to bottom left corner
	ailp_show_move(p(Size,1),p(Size,Size)), % move from start to top left corner
	assignment1_module:ailp_show_complete.
q5_corner_move2 :-
	ailp_grid_size(Size), % get size of board
	ailp_start_position(p(Size,1)), % get start position
	% if start point is one of corners --- ignore
	ailp_show_move(p(Size,1),p(1,1)), % move from start to top right corner
	ailp_show_move(p(1,1),p(1, Size)), % move from start to bottom left corner
	ailp_show_move(p(1,Size),p(Size,Size)), % move from start to top left corner
	assignment1_module:ailp_show_complete.
q5_corner_move2 :-
	ailp_grid_size(Size), % get size of board
	ailp_start_position(p(Size,Size)), % get start position
	% if start point is one of corners --- ignore
	ailp_show_move(p(Size,Size),p(1,Size)), % move from start to top right corner
	ailp_show_move(p(1,Size),p(1,1)), % move from start to bottom left corner
	ailp_show_move(p(1,1),p(Size,1)), % move from start to top left corner
	assignment1_module:ailp_show_complete.
%%%%%%%%%%%%%%?????

%     Q6

% q6_spiral/1
q6_spiral(Path) :-
	ailp_start_position(p(SX, SY)), % get start position
	ailp_grid_size(Size), % get size of board
	q6_initiate(p(SX, SY), p(NX, NY), Size), % check where you are and move to the border if necessary
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

% move agent
q6_move(Position, Path) :-
	q6_d(D), % choose direction to follow
	q6_move(Position, [Position], RewPath, D, 0),
	reverse(RewPath, Path).

q6_move(_, Path, Path, _, _) :-
	q6_complete(Path).
q6_move(Position, PosList, RewPath, D, L) :-
	LN is L+1,
	ailp_grid_size(S),
	SN is S/2,
	LN =< SN,

	q6_m(M),
	q6_new_pos(Position, M, NewPosition),
	\+ memberchk(NewPosition, PosList),

	% update radius?
	% when all positions from current radius are in the step-on list!
	% each element of RadiusList belongs to PosList
	radius_updates(PosList, L, L1),

	outer(L1, NewPosition),

	% force to persist?
	% only needed when changing radius
	% CUT, CUT, CUT, CUT, CUT
	ensure_direction(D, M, Position, PosList),


	ailp_show_move(Position, NewPosition),
	%% term_to_atom([NewPosition|PosList],PsA),
	%% do_command([mower,console,PsA],_R),
	q6_move(NewPosition, [NewPosition|PosList], RewPath, D, L1),
	true.

% q6_initiate agent to outer position
ensure_direction(D, M, p(X, Y), PosList) :-
	ailp_grid_size(S), % get size of board
	S1 is S/2,
	( D = cw,  M = s, X =< S1  -> \+ (
							  (q6_new_pos(p(X, Y), n, NP), \+ memberchk(NP, PosList))
							; (q6_new_pos(p(X, Y), e, NP), \+ memberchk(NP, PosList))%c
							)
	% in CW prefer N i.e. if N is legit don't move
	; D = cw,  M = n, X >= S1  -> \+ (
							   (q6_new_pos(p(X, Y), s, NP), \+ memberchk(NP, PosList))
							 ; (q6_new_pos(p(X, Y), w, NP), \+ memberchk(NP, PosList))%c
							 )
	; D = cw,  M = w, Y =< S1  -> \+ (
							   (q6_new_pos(p(X, Y), e, NP), \+ memberchk(NP, PosList)) % etc.
							;  (q6_new_pos(p(X, Y), s, NP), \+ memberchk(NP, PosList))%c
							)
	; D = cw,  M = e, Y >= S1  -> \+ (
							  (q6_new_pos(p(X, Y), w, NP), \+ memberchk(NP, PosList))
							; (q6_new_pos(p(X, Y), n, NP), \+ memberchk(NP, PosList))%c
							)
	; D = acw, M = n, X =< S1  -> \+ (
							  (q6_new_pos(p(X, Y), s, NP), \+ memberchk(NP, PosList))
							; (q6_new_pos(p(X, Y), e, NP), \+ memberchk(NP, PosList))%c
							)
	; D = acw, M = s, X >= S1  -> \+ (
							  (q6_new_pos(p(X, Y), n, NP), \+ memberchk(NP, PosList))
							; (q6_new_pos(p(X, Y), w, NP), \+ memberchk(NP, PosList))%c
							)
	; D = acw, M = e, Y =< S1  -> \+ (
							  (q6_new_pos(p(X, Y), w, NP), \+ memberchk(NP, PosList))
							; (q6_new_pos(p(X, Y), s, NP), \+ memberchk(NP, PosList))%c
							)
	; D = acw, M = w, Y >= S1  -> \+ (
							  (q6_new_pos(p(X, Y), e, NP), \+ memberchk(NP, PosList))
							; (q6_new_pos(p(X, Y), n, NP), \+ memberchk(NP, PosList))%c
							)
	; otherwise -> true
	).




radius_updates(List, L, L1) :-
	ailp_grid_size(S),
	fields_to_visit_at_level_l(S, L, 0, Vi),
	sup(List, Vi, L, L1).

sup(List, N, L, L) :-
	not( length(List, N) ).
sup(List, N, L, L1) :-
	length(List, N),
	L1 is L+1.

fields_to_visit_at_level_l(S, 0, Acc, Vi) :-
	Vi is 4*S -4+Acc.
fields_to_visit_at_level_l(S, L, Acc, Vi) :-
	CurLev is 4*(S-(L*2)) -4+Acc,
	L1 is L-1,
	L>0,
	fields_to_visit_at_level_l(S, L1, CurLev, Vi),
	true.






% can I use this?
q6_complete(L) :- 
	ailp_grid_size(N),
	N2 is N * N,
	length(L,N2),
	ailp_show_complete.

q6_new_pos(p(X,Y), M, p(X1,Y1)) :-
	( M = s -> X1 =  X,    Y1 is Y+1
	; M = n -> X1 =  X,    Y1 is Y-1
	; M = e -> X1 is X+1,  Y1 =  Y
	; M = w -> X1 is X-1,  Y1 =  Y
	),
	X1 >= 1, Y1 >=1,
	ailp_grid_size(N),
	X1 =< N, Y1 =< N. 

q6_m(n).
q6_m(e).
q6_m(s).
q6_m(w).

q6_d(cw).
q6_d(acw).

%% for testing only | no querying
outer(Level, p(X,Y)) :-
	ailp_grid_size(N),
	B is 1+Level,
	H is N-Level,
	(
		((Y = B; Y = H), is_in_range(B, H, X), !);
		((X = B; X = H), is_in_range(B, H, Y))
	),
	true.
is_in_range(L, U, N) :-
	N =< U,
	N >= L.
