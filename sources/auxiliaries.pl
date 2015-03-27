% ---------------------------------------------------------------------
% $Id: auxiliaries.pl,v 1.9 2002/12/02 23:22:49 vermaat Exp $
% part of Grail
%
% Grail author:
%    Richard Moot
%
% Refactoring, Cleanup, Fit'n'Finish:
%    Xander Schrijen, Gert Jan Verhoog
%
% <mailto:grail4cki@phil.uu.nl>
% ---------------------------------------------------------------------

:- module(auxiliaries,
          [calculate_tabs/2,
           time/1,
           reverse/3,
           precedes/2,
           member_check/2,
           union/5,
           generate_file_name/3]).

:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3]) ; true).


% calculate the number of tabs necessary to print a list
% of numbers right-justified.

calculate_tabs([X|Xs],Tabs) :-
     max_size(Xs,X,Max),
     num_tabs([X|Xs],Max,Tabs).

num_tabs([],_,[]).

num_tabs([X|Xs],Max,[T|Ts]) :-
     Y is X // 10,
     size(Y,1,N),
     T is Max-N,
     num_tabs(Xs,Max,Ts).

max_size([],X,M) :-
     Y is X // 10, 
     size(Y,1,M).
max_size([X|Xs],Y,M) :-
     Z is max(X,Y),
     max_size(Xs,Z,M).

size(0,N0,N) :-
     !,
     M is (N0-1) // 3,
     N is N0+M.

size(X,N0,M) :-
     X>0,
     Y is X // 10,
     N is N0+1,
     size(Y,N,M).


% = time(+Call)
% print time used to find the first solution (if any) to Call

time(Call) :-
     statistics(runtime,[T0|_]),
     call1(Call),
     statistics(runtime,[T|_]),
     Time is (T-T0)*0.001,
     write('CPU Time used: '),write(Time),nl.

% = call1(Goal) 
% call goal once, succeed always

call1(Call) :- call(Call),!.
call1(_).

% = more list handling

tail([],X,X,[]).
tail([Y|Ys],Z,X,[Z|Zs]) :-
     tail(Ys,Y,X,Zs).

reverse([],L,L).
reverse([X|Xs],Ys,Zs) :-
     reverse(Xs,[X|Ys],Zs).


% precedes(+LPNumber0,+LPNumber)
% true if LPNumber0 precedes LPNumber. LPNumbers can be either a
% number or the constant 'x' which stands for an unknown position

precedes(x,_) :- !.
precedes(X,Y) :- X @=< Y. % also includes precedes(Num,x) 

% =

member_check(X,[X|_]) :- !.
member_check(X,[_|Ys]) :-
     member_check(X,Ys).



union([],AVL,AVL,RN,RN).
union([X|Xs],AVL0,AVL,RN0,RN) :-
       insert(AVL0,X,AVL1,RN0,RN1,_),
       union(Xs,AVL1,AVL,RN1,RN).

insert(empty,         Key, node(Key,0,empty,empty), [Key|RN], RN, 1).
insert(node(K,B,L,R), Key, Result, RN0, RN, Delta) :-
	compare(O, Key, K),
	insert(O, Key, Result, Delta, K, B, L, R, RN0, RN).


insert(=, Key, node(Key,B,L,R), 0, _, B, L, R, RN, RN).
insert(<, Key, Result,          Delta, K, B, L, R, RN0, RN) :-
	insert(L, Key, Lavl, RN0, RN, Ldel),
	Delta is \(B) /\ Ldel,	% this grew iff left grew and was balanced
	B1 is B-Ldel,
	(   B1 =:= -2 ->	% rotation needed
	    Lavl = node(Y,OY,A,CD),	    
	    (   OY =< 0 ->
		NY is OY+1, NK is -NY,
		Result = node(Y,NY,A,node(K,NK,CD,R))
	    ;/* OY = 1, double rotation needed */
		CD = node(X,OX,C,D),
		NY is 0-((1+OX) >> 1),
		NK is (1-OX) >> 1,
		Result = node(X,0,node(Y,NY,A,C),node(K,NK,D,R))
	    )
	;   Result = node(K,B1,Lavl,R)
	).
insert(>, Key, Result,          Delta, K, B, L, R, RN0, RN) :-
	insert(R, Key, Ravl, RN0, RN, Rdel),
	Delta is \(B) /\ Rdel,	% this grew iff right grew and was balanced
	B1 is B+Rdel,
	(   B1 =:= 2 ->		% rotation needed
	    Ravl = node(Y,OY,AC,D),
	    (   OY >= 0 ->
		NY is OY-1, NK is -NY,
		Result = node(Y,NY,node(K,NK,L,AC),D)
	    ;/* OY = -1, double rotation needed */
		AC = node(X,OX,A,C),
		NY is (1-OX) >> 1,
		NK is 0-((1+OX) >> 1),
		Result = node(X,0,node(K,NK,L,A),node(Y,NY,C,D))
	    )
	;   Result = node(K,B1,L,Ravl)
	).


% ============================================================
% File I/O
% ============================================================

generate_file_name(Dir,File,DirFile) :-
	name(Dir,DirS),
	name(File,FileS),
	append(DirS,[47|FileS],DirFileS),
	name(DirFile0,DirFileS),
	canonicalize_path(DirFile0,DirFile).


