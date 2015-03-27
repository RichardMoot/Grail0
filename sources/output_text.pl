% ---------------------------------------------------------------------
% $Id: output_text.pl,v 1.5 2002/12/02 23:22:52 vermaat Exp $
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
%
% Generates plaintext output to the console
%
% ---------------------------------------------------------------------


:- module(output_text, []).


% Print information about the total number of solutions.

generate_output(Results) :- write_number_of_solutions(Results),write_results(Results, 1).

write_results([], _).

write_results([result(Meaning, _S0, _S, _, _, _, _)|Results], CurrentResult) :-
	write(CurrentResult),
	write('. '),
	write_semantics(Meaning),nl,
%	write_term_as_text(S0),
%	write(' --> '),
%	nl,
%	write_term_as_text(S),nl,
%	nl,
%	write('==='),nl,
	NextResult is CurrentResult + 1,
	write_results(Results, NextResult).
	
write_semantics(X) :- \+ \+ (numbervars(X,21,_),write_term_as_text(X)).


% = write_goal(+ListOfWordsOrFormulas,+Goal)
%
% Prints a message to the screen.


write_goal_info(X, Goal) :-
	format('===~n',[]),
	format('~w => ~w~n', [X, Goal]),
	format('===~n',[]).



% = write_number_of_solutions(+Results)
%
% print to the screen how many solutions have been found.

write_number_of_solutions([]) :-
	write('No solutions!'),nl.
write_number_of_solutions([_|_]).

%write_number_of_solutions([_]) :-
%	write('1 solution found.').

%write_number_of_solutions(Results) :-
%	length(Results, N),
%    format('~w solutions found.', [N]).



% ---------------------------------------------------------------------
% Pretty-printing logic. Originally, this used the pretty-printing
% mechanism in prolog (portray); changed to improve modularization
% ---------------------------------------------------------------------


%write_term_as_text([rule(A,B,C,D)|Rules]) :-
%       !,
%       nl,
%       portray_list([rule(A,B,C,D)|Rules]).

%write_term_as_text([vertex(A,B,C)|Vs]) :-
%       !,
%       nl,
%       portray_list([vertex(A,B,C)|Vs]).

write_term_as_text(prove(L)) :- !,nl,write('== prove =='),nl,portray_list(L).

write_term_as_text(prove1(Es,E)) :- !,nl,write('== prove =='),nl,write_term_as_text(E),nl,write('==='),nl,portray_list(Es).
 
write_term_as_text(prove(Es,E)) :- !,nl,write('== prove=='),nl,
    my_numbervars([E|Es],41,_,0,_),portray_vertices([E|Es]).

write_term_as_text(prove(Es,E,_,_)) :- !,nl,write('== prove=='),nl,
    my_numbervars([E|Es],41,_,0,_),portray_vertices([E|Es]).

write_term_as_text(select_goal(G,_,_)) :- !,nl,write('== select_goal =='),
    nl,write_term_as_text(G).

write_term_as_text(select_conjugate(_,[C|_],_)) :- !,nl,write('== select_conj =='),
    nl,write_term_as_text(C).

write_term_as_text(select_atom(A,vertex(N,_,_),_,_)) :- !,write('== select_atom
=='),
    nl,write_term_as_text(N),write(':'),write_term_as_text(A),nl.

write_term_as_text(select_conj(A,vertex(N,_,_),_,_)) :- !,write('== select_conj
=='), nl, write_term_as_text(N),write(':'),write_term_as_text(A),nl.
write_term_as_text(ord_union(_,_,_,New)) :- 
        var(New) -> write_term_as_text(New) 
       ;
        nl,write('== new children =='),
	nl,portray_list(New).

write_term_as_text(children(Parent,Children)) :-
        nonvar(Children),!,
        nl,write('== parent =='),nl,write_term_as_text(Parent),nl,
        write('== children =='),nl,portray_list(Children).


write_term_as_text(generate_sequent(Ss,Os,Ts,_,_,_)) :-
        nl,write('== seqents =='),nl,portray_list(Ss),
        nl,write('== ones =='),nl,portray_list(Os),
        nl,write('== twos =='),nl,portray_list(Ts),nl.

write_term_as_text(generate_nd(NDs,Os,Ts,_,_,_,_,_)) :-
        nl,write('== nds =='),nl,portray_list(NDs),
        nl,write('== ones =='),nl,portray_list(Os),
        nl,write('== twos =='),nl,portray_list(Ts),nl.

write_term_as_text(generate_fitch(NDs,Os,Ts,_,_,_,_,_,_,_,_)) :-
        nl,write('== nds =='),nl,portray_list(NDs),
        nl,write('== ones =='),nl,portray_list(Os),
        nl,write('== twos =='),nl,portray_list(Ts),nl.
        
write_term_as_text(rule(N,A,S,_)) :-
        !,write_ant(A),write(' '),write_term_as_text(N),write('=> '),write_term_as_text(S).
write_term_as_text(sign(A,0,B,C,_)) :-
	!,write('+'),write_term_as_text(C),write(':'),write_term_as_text(A),write(':'),
        \+ \+ (numbervars(B,23,_),write_term_as_text(B)).
write_term_as_text(sign(A,1,B,C,_)) :-
	!,write('-'),write_term_as_text(C),write(':'),write_term_as_text(A),write(':'),
        \+ \+ (numbervars(B,23,_),write_term_as_text(B)).

write_term_as_text(not(A,_,S,P0,P1)) :-
        !,write('-'),write_term_as_text(A),write(':'),write_term_as_text(S),write(':'),write_term_as_text(P0),
        write('-'),write_term_as_text(P1).
write_term_as_text(pos(A,_,S,P0,P1)) :-
        !,write('+'),write_term_as_text(A),write(':'),write_term_as_text(S),write(':'),write_term_as_text(P0),
        write('-'),write_term_as_text(P1).

write_term_as_text(not(A,_,_)) :-
        !,write('-'),write_term_as_text(A).
write_term_as_text(pos(A,_,_)) :-
        !,write('+'),write_term_as_text(A).
write_term_as_text(not(A)) :-
        !,write('-'),write_term_as_text(A).
write_term_as_text(pos(A)) :-
        !,write('+'),write_term_as_text(A).
write_term_as_text(lolli(A,B)) :-
        !,write('('),write_term_as_text(A),write(' -0 '),write_term_as_text(B),write(')').
write_term_as_text(tensor(A,B)) :-
        !,write('('),write_term_as_text(A),write(' @ '),write_term_as_text(B),write(')').
write_term_as_text(atom(A,_M,_S)) :-
        !,write_term_as_text(A).

%write_term_as_text(X-Y) :- nonvar(X),X='$VAR'(N),!,write_term_as_text('$VAR'(N)),write_term_as_text('-'),write_term_as_text(Y).
write_term_as_text(X-Y) :- nonvar(X),(X=x;number(X)),\+number(Y),!,write_term_as_text(Y).
%write_term_as_text(X-Y) :- nonvar(X), !, write('L-'),write_term_as_text(Y).

write_term_as_text(lit(A)) :- !, write_term_as_text(A).


write_term_as_text(sp(I,X,Y)) :-
    write('('),write_term_as_text(X),write(','),write_term_as_text(Y),write(')'),write_term_as_text(I),write(' ').
write_term_as_text(sdia(I,X)) :-
    write('<'),write_term_as_text(X),write('>'),write_term_as_text(I),write(' ').
write_term_as_text(dia(I,X)) :-
    write('^'),write(I),write('('),write_term_as_text(X),write(')').
write_term_as_text(box(I,X)) :-
    write('@'),write(I),write('('),write_term_as_text(X),write(')').
write_term_as_text(p(I,X,Y)) :-
    write('('),write_term_as_text(X),write(' *'),write(I),write(' '),write_term_as_text(Y),write(')').
write_term_as_text(dr(I,X,Y)) :-
    write('('),write_term_as_text(X),write(' /'),write(I),write(' '),write_term_as_text(Y),write(')').

write_term_as_text(dl(I,X,Y)) :-
    write('('),write_term_as_text(X),write(' \\'),write(I),write(' '),write_term_as_text(Y),write(')').
write_term_as_text(l(I,X)) :-
    write('<'),write(I),write('('),write_term_as_text(X),write(')').
write_term_as_text(r(I,X)) :-
    write('>'),write(I),write('('),write_term_as_text(X),write(')').
write_term_as_text(zip(I,X)) :-
    write('^'),write(I),write('('),write_term_as_text(X),write(')').
write_term_as_text(unzip(I,X)) :-
    write('#'),write(I),write('('),write_term_as_text(X),write(')').
write_term_as_text(unpack(I,X)) :-
    write('@'),write(I),write('('),write_term_as_text(X),write(')').
write_term_as_text(dom(X)) :-
    write('{'),write_term_as_text(X),write('}').

write_term_as_text(lambda(X,Y)) :- !,write('^'),write(X),write('.'),write_term_as_text(Y).

write_term_as_text(appl(appl(appl(F,Z),Y),X)) :-
     atom(F),
     !,write_term_as_text(F),write('('),write_term_as_text(X),write(','),
     write_term_as_text(Y),write(','),write_term_as_text(Z),write(')').    

write_term_as_text(appl(appl(F,Y),X)) :-
     atom(F),
     !,write_term_as_text(F),write('('),write_term_as_text(X),write(','),
     write_term_as_text(Y),write(')').

write_term_as_text(appl(X,Y)) :- !,write_term_as_text(X),write('('),write_term_as_text(Y),write(')').

write_term_as_text(pair(X,Y)) :- !,write('<'),write_term_as_text(X),write(','),write_term_as_text(Y),write(' >').

write_term_as_text(pi1(X)) :- !,write('pi1('),write_term_as_text(X),write(')').

write_term_as_text(pi2(X)) :- !,write('pi2('),write_term_as_text(X),write(')').

write_term_as_text(bool(X,Y,Z)) :- !,write('('),write_term_as_text(X),write(' '),write(Y),write(' '),write_term_as_text(Z),write(')').

write_term_as_text(quant(Q,X,T)) :- !,write(Q),write(' '),write(X),write('['),write_term_as_text(T),write(']').


% fNf: catch-all write_term_as_text
write_term_as_text(X) :- write(X).





portray_list([]) :- 
     !,
     write('empty'),
     nl.

portray_list([A]) :- 
     !,
     write_term_as_text(A),
     nl,
     write('==='),
     nl.

portray_list([A|As]) :- 
     write_term_as_text(A),
     nl,
     portray_list(As).

print_sentences([]).
print_sentences([X|Xs]) :- write_term_as_text(X),write(' => s'),nl,
     print_sentences(Xs).

portray_vertices([]) :- 
     !,
     write('empty'),
     nl.

portray_vertices([vertex(A,B,C)]) :- 
     !,
     write(A),write(' ['),
     portray_atoms(B),
     write(']-'),
     write_term_as_text(C),
     nl,
     write('==='),
     nl.

portray_vertices([vertex(A,B,C)|As]) :- 
     write(A),write(' ['),
     portray_atoms(B),
     write(']-'),
     write_term_as_text(C),
     nl,
     portray_vertices(As).

portray_atoms([]).

portray_atoms([A]) :- 
     !,
     write_term_as_text(A).

portray_atoms([A|As]) :- 
     write_term_as_text(A),
     nl,
     tab(3),
     portray_atoms(As).

my_numbervars([],Lab,Lab,Pos,Pos).

my_numbervars([vertex(_,As,_)|Rest],Lab0,Lab,Pos0,Pos) :-
    my_numbervars1(As,Lab0,Lab1,Pos0,Pos1),
    my_numbervars(Rest,Lab1,Lab,Pos1,Pos).


my_numbervars1([],Lab,Lab,Pos,Pos).
my_numbervars1([X|Xs],Lab0,Lab,Pos0,Pos) :-
    my_numbervars2(X,Lab0,Lab1,Pos0,Pos1),
    my_numbervars1(Xs,Lab1,Lab,Pos1,Pos).

my_numbervars2(not(_,_,L,P0,P1),Lab0,Lab,Pos0,Pos) :-
    numbervars(L,Lab0,Lab),
    numbervars(P0,Pos0,Pos1),
    numbervars(P1,Pos1,Pos).

my_numbervars2(pos(_,_,L,P0,P1),Lab0,Lab,Pos0,Pos) :-
    numbervars(L,Lab0,Lab),
    numbervars(P0,Pos0,Pos1),
    numbervars(P1,Pos1,Pos).

write_ant(X) :- var(X),!,write(X).

write_ant(sp(I,X,Y)) :-
    !,
    write('('),write_ant(X),write(','),write_ant(Y),write(')'),write_term_as_text(I),write(' ').
write_ant(sdia(I,X)) :-
    !,
    write('<'),write_ant(X),write('>'),write_term_as_text(I),write(' ').
write_ant(Type) :-
    write_term_as_text(Type).


