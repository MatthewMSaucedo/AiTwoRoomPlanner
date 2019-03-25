
%%%%%%%%% Simple Prolog Two Room Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% UCF
%%% Artificial Intelligence:
%%% CAP4630 Spring 2019
%%% 
%%% by Matthew Saucedo and Daniel Canas
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module( planner,
	   [
	       plan/4,change_state/3,conditions_met/2,member_state/2,
	       move/3,go/2,test1/0,test2/0
	   ]).

:- [utils].

plan(State, Goal, _, Moves) :-	equal_set(State, Goal),
				write('moves are'), nl,
				reverse_print_stack(Moves).
plan(State, Goal, Been_list, Moves) :-
				move(Name, Preconditions, Actions),
				conditions_met(Preconditions, State),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
			plan(Child_state, Goal, New_been_list, New_moves),!.

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
conditions_met(P, S) :- subset(P, S).

member_state(S, [H|_]) :-	equal_set(S, H).
member_state(S, [_|T]) :-	member_state(S, T).

/* move types */

% goroom1
% goroom2
% handroom1
% handroom2
% room1(x)
% room2(x)

%%%%% GOROOM: Move Robot Arm by itself %%%%%

move(goroom1, [handroom2, holding(X)],
		[del(handroom2), del(room2(X)), add(handroom1), add(room1(X))]).	
		
move(goroom2, [handroom1, holding(X)],
		[del(handroom1), del(room1(X)), add(handroom2), add(room2(X))]).
		
move(goroom1, [handroom2, handempty],
		[del(handroom2), add(handroom1)]).	
		
move(goroom2, [handroom1, handempty],
		[del(handroom1), add(handroom2)]).

% pickup off another block room 1		
move(pickup(X), [handempty, handroom1, room1(X), clear(X), on(X, Y)],
		[del(handempty), del(clear(X)), del(on(X, Y)),
				 add(clear(Y)),	add(holding(X))]).
				 
% pickup off another block room 2		
move(pickup(X), [handempty, handroom1, room1(X), clear(X), on(X, Y)],
		[del(handempty), del(clear(X)), del(on(X, Y)),
				 add(clear(Y)),	add(holding(X))]).

% pickup off table room 1
move(pickup(X), [handempty, handroom1, room1(X), clear(X), ontable(X)],
		[del(handempty), del(clear(X)), del(ontable(X)),
				 add(holding(X))]).
				 
% pickup off table room 2
move(pickup(X), [handempty, handroom2, room2(X), clear(X), ontable(X)],
		[del(handempty), del(clear(X)), del(ontable(X)),
				 add(holding(X))]).

% place block on table room 1
move(putdown(X), [holding(X), handroom1],
		[del(holding(X)), add(ontable(X)), add(clear(X)),
				  add(handempty)]).

% place block on table room 2
move(putdown(X), [holding(X), handroom2],
		[del(holding(X)), add(ontable(X)), add(clear(X)),
				  add(handempty)]).

% place block on block room 1
move(stack(X, Y), [holding(X), handroom1, room1(Y), clear(Y)],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y)),
				  add(clear(X))]).
				  
% place block on block room 2
move(stack(X, Y), [holding(X), handroom2, room2(Y), clear(Y)],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y)),
				  add(clear(X))]).
		
		
/* run commands */

% S is a start-state List.
% G is a goal-state List.

go(S, G) :- plan(S, G, [S], []).

% a 1-room scenario using your 2-room predicates: Start with blocks A, B, and C, and robotic arm all in Room 1,
% with blocks B and C on the table and block A on top of block B. 
% Goal state is all blocks and arm in Room 1, with block C on the table, block B on C, and block A on B.

test1 :- go([room1(a), room1(b), room1(c), handroom1, handempty, ontable(b), ontable(c), on(a, b), clear(a), clear(c)],
	          [room1(a), room1(b), room1(c), handroom1, handempty, ontable(c), on(b,c), on(a, b), clear(a)]).
			  
% a 2-room scenario using your 2-room predicates:  Same start state as for test1. 
% Goal state is: robot arm in Room 1; all blocks in Room 2, with block B on the table, 
% block C on B, and block A on C.

test2 :- go([room1(a), room1(b), room1(c), handroom1, handempty, ontable(b), ontable(c), on(a, b), clear(a), clear(c)],
	          [room2(a), room2(b), room2(c), handroom1, handempty, ontable(b), on(c, b), on(a, c), clear(a)]).
