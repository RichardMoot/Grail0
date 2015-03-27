% ---------------------------------------------------------------------
% $Id: reduce_sem.pl,v 1.11 2003/06/02 13:42:58 gjv Exp $
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


:- module(reduce_sem,
          [reduce_sem/2,
           replace_sem/4,
           substitute_sem/5]).

:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3]) ; true).

:- use_module(options).


% ---------------------------------------------------------------------
% Lambda term normalization
% ---------------------------------------------------------------------

reduce_sem(Term0,Term) :-
    reduce_sem1(Term0,Term1),
    !,
    reduce_sem(Term1,Term).

reduce_sem(Term,Term).

reduce_sem1(appl(lambda(X,T0),Y),T) :-
     replace_sem(T0,X,Y,T).
reduce_sem1(lambda(X,appl(F,X)),F):-
% toegevoegd uit de originele code (MM)
     \+ subterm(F,X).
reduce_sem1(fst(pair(T,_)),T).
reduce_sem1(snd(pair(_,T)),T).
reduce_sem1(pair(fst(T),snd(T)),T).
reduce_sem1(condia(dedia(T)),T).
reduce_sem1(dedia(condia(T)),T).
reduce_sem1(conbox(debox(T)),T).
reduce_sem1(debox(conbox(T)),T).
reduce_sem1(condia(T),T) :- 
     unary_semantics(inactive).
reduce_sem1(dedia(T),T) :- 
     unary_semantics(inactive).
reduce_sem1(conbox(T),T) :- 
     unary_semantics(inactive).
reduce_sem1(debox(T),T) :- 
     unary_semantics(inactive).

reduce_sem1(merge(drs(X,C),drs(Y,D)),drs(Z,E)) :-
     append(X,Y,Z0),
     sort(Z0,Z),
     append(C,D,E).

% = recursive case

reduce_sem1(T,U) :-
     T =.. [F|Ts],
     reduce_list(Ts,Us),
     U =.. [F|Us].

% zie boven (MM)
subterm(X,X) :- !.
subterm(X,Y) :-
     functor(X,_,N),
     subterm(N,X,Y).

subterm(N0,X,Y) :-
     N0>0,
     arg(N0,X,A),
     subterm(A,Y).

subterm(N0,X,Y) :-
     N0>0,
     N is N0-1,
     subterm(N,X,Y).

% = DRS merge; simply appends contexts and conditions

reduce_list([T|Ts],[U|Ts]) :-
     reduce_sem1(T,U).
reduce_list([T|Ts],[T|Us]) :-
     reduce_list(Ts,Us).

replace_sem(X,X,Y,Y) :- !.
replace_sem(U,X,Y,V) :-
     functor(U,F,N),
     functor(V,F,N),
     replace_sem(N,U,X,Y,V).

replace_sem(0,_,_,_,_) :- !.
replace_sem(N0,U,X,Y,V) :-
     N0>0,
     N is N0-1,
     arg(N0,U,A),
     replace_sem(A,X,Y,B),
     arg(N0,V,B),
     replace_sem(N,U,X,Y,V).

substitute_sem(L,T0,T) :-
     substitute_sem(L,T0,T,0,_).

substitute_sem(_, _, _, 0).

substitute_sem(L, T0, T, NVAR) :-
	substitute_sem(L, T0, T, NVAR, _).

substitute_sem([],T,T,N,N).
substitute_sem([X-U|Rest],T0,T,N0,N) :-
     numbervars(U,N0,N1),
     replace_sem(T0,'$VAR'(X),U,T1),
     substitute_sem(Rest,T1,T,N1,N).




