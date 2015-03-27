% ---------------------------------------------------------------------
% $Id: common_tex.pl,v 1.3 2002/12/04 21:17:43 gjv Exp $
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


% ---------------------------------------------------------------------
% TeX generating code for output_prawitz_tex and output_fitch_tex
% ---------------------------------------------------------------------

:- module(common_tex,[write_label/4,
					  write_type/1,
					  write_sem/1,
					  write_sem/5,
					  rule_name/2,
					  rule_discharges/2]).

:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3]) ; true).

:- use_module([auxiliaries,fragments,reduce_sem,options]).

% = write_label

write_label(L,P1,P2,Con) :-
       (output_labels(yes) ->
        write(P1),
        write_label(L,1,Con),
        write(P2)
       ;
        true).

% =

write_label(p(l,A,B),N,Con) :-
        !,
        write_bo(N),
        binding(A,p(I,A,B),NA),
        write_label(A,NA,Con),
        write('\\lcirc_{}'),
        binding(B,p(I,A,B),NB),
        write_label(B,NB,Con),
        write_bc(N).

write_label(p(r,A,B),N,Con) :-
        !,
        write_bo(N),
        binding(A,p(I,A,B),NA),
        write_label(A,NA,Con),
        write('\\rcirc_{}'),
        binding(B,p(I,A,B),NB),
        write_label(B,NB,Con),
        write_bc(N).

write_label(p(I,A,B),N,Con) :-
        !,
        write_bo(N),
        binding(A,p(I,A,B),NA),
        write_label(A,NA,Con),
        write('\\circ_{'),
	write_mode(I),
	write('}'),
        binding(B,p(I,A,B),NB),
        write_label(B,NB,Con),
        write_bc(N).

write_label(zip(I,A),_,Con) :-
        !,
        write('\\langle '),
        write_label(A,1,Con),
        write('\\rangle^{'),
        write_mode(I),
        write('}').

write_label(l(J,D),_,Con) :-
        !,
        member_check(cf(l(J,D),_,'$VAR'(N)),Con),
        write_pros_var(N).

write_label(r(J,D),_,Con) :-
        !,
        member_check(cf(r(J,D),_,'$VAR'(N)),Con),
        write_pros_var(N).

write_label(unzip(J,D),_,Con) :-
        !,
        member_check(cf(unzip(J,D),_,'$VAR'(N)),Con),
        write_pros_var(N).

write_label(_-'$VAR'(N),_,_) :-
        !,
        write_pros_var(N).

write_label(_-A,_,_) :-
        write('\\mbox{'),
       (atom(A) ->
        write_atom(A)
       ;
        print(A)),
        write('}').

write_pros_var(N) :-
        V is N mod 4,
        I is N // 4,
        pros_var_name(V,Name),
        format('\\mbox{~w}_{~w}',[Name,I]).

pros_var_name(0,p).
pros_var_name(1,q).
pros_var_name(2,r).
pros_var_name(3,s).


% =

write_atom(At) :-
    write_atom(At,'\\_{}').

write_atom(At,Pr) :-
      atom_chars(At,L),
      write_atom_list(L,Pr).

write_atom_list(L,Pr) :-
      split(L,'_',L0,[],L1),
      !,
      atom_chars(At,L0),
      format('~w~w',[At,Pr]),
      write_atom_list(L1,Pr).
write_atom_list(L,_) :-
      atom_chars(At,L),
      write(At).

split([X|Xs],X,Ys,Ys,Xs) :- !.
split([X|Xs],S,[X|Ys0],Ys,Zs) :-
      split(Xs,S,Ys0,Ys,Zs).



write_type(T0) :-
       (macro_reduce(yes) ->
        macro_reduce(T0,T)
       ;
        T=T0),
        write_type(T,1).

% = write_type0(+Type)
% write a type with outer brackets

write_type(lit(X),N) :- !,write_type(X,N).

write_type(dl(r,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dl(I,A,B),NA),
        write_type(A,NA),
        write(' \\rhead_{}'),
        binding(B,dl(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).

write_type(dl(l,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dl(I,A,B),NA),
        write_type(A,NA),
        write(' \\rdep_{}'),
        binding(B,dl(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).

write_type(dr(r,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dl(I,A,B),NA),
        write_type(A,NA),
        write(' \\ldep_{}'),
        binding(B,dl(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).

write_type(dr(l,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dl(I,A,B),NA),
        write_type(A,NA),
        write(' \\lhead_{}'),
        binding(B,dl(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).

write_type(dl(I,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dl(I,A,B),NA),
        write_type(A,NA),
        write(' \\bs_{'),
        write_mode(I),
        write('}'),
        binding(B,dl(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).

write_type(dr(I,A,B),N) :- 
        !, 
        write_bo(N),
        binding(A,dr(I,A,B),NA),
        write_type(A,NA),
        write(' /_{'),
        write_mode(I),
        write('}'),
        binding(B,dr(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).
write_type(p(I,A,B),N) :-
        !,
        write_bo(N),
        binding(A,p(I,A,B),NA),
        write_type(A,NA),
        write(' \\bullet_{'),
        write_mode(I),
        write('}'),
        binding(B,p(I,A,B),NB),
        write_type(B,NB),
        write_bc(N).
write_type(dia(I,A),_) :-
        !,
        write('\\diamondsuit_{'),
        write_mode(I),
        write('}'),
        binding(A,dia(I,A),NA),
        write_type(A,NA).
write_type(box(I,A),_) :-
        !,
        write('\\Box_{'),
        write_mode(I),
        write('}'),
        binding(A,box(I,A),NA),
        write_type(A,NA).

/* extra rules for non-atomic macro definitions */

write_type(bang(I,A),_) :-
        !,
        write('!_{'),
        write_mode(I),
        write('}'),
        write_type(A,0).

write_type(q(A,B,C),_) :-
        !,
        write('q('),
        write_type(A,1),
        write(','),
        write_type(B,1),
        write(','),
        write_type(C,1),
        write(')').

write_type(X,_) :-
        /* X is an atomic type */ 
       (atom(X) ->
        write_atom(X)
       ;
        print(X)).



% ===========================================================

write_sem(Sem0,P1,P2,Subst,NV) :-
       (output_semantics(yes) ->
        (output_subst_lex_sem(yes) ->
         substitute_sem(Subst,Sem0,Sem1,NV,_)
        ;
         Sem1=Sem0),
        (output_reduced_sem(yes) ->
         reduce_sem(Sem1,Sem)
        ;
         Sem=Sem1),
         write(P1),
         write_sem(Sem,1),
         write(P2)
       ;
        true).

write_sem(T) :-
        write_sem(T,1).

write_sem(drs(V,C),_) :-
        !,
        format('\\mbox{\\begin{tabular}{|l|} \\hline~n',[]),
        write_list_of_vars(V),
        format(' \\\\ \\hline~n',[]),
        write_list_of_conds(C),
        format(' \\end{tabular}~n}',[]).

write_sem(merge(A,B),N) :-
        !,
        write_bo(N),
        binding(A,merge(A,B),NA),
        write_sem(A,NA),
        write(' \\circ '),
        binding(B,merge(A,B),NB),
        write_sem(B,NB),
        write_bc(N).

write_sem(bool(A,C,B),N) :-
        !,
        write_bo(N),
        binding(A,bool(A,C,B),NA),
        write_sem(A,NA),
        write_conn(C),
        binding(B,bool(A,C,B),NB),
        write_sem(B,NB),
        write_bc(N).

write_sem(not(X),_) :-
        !,
        write(' \\neg '),
        binding(X,not(X),NX),
        write_sem(X,NX).

write_sem(quant(Q,X,T),N) :-
        !,
        write_bo(N),
        write_quant(Q),
        write_sem(X,1),
        binding(T,quant(Q,X,T),NT),
        write_sem(T,NT),
        write_bc(N).      

write_sem(lambda(X,V),N) :-
        !,
        write_bo(N),
        write('\\lambda '),
        write_sem(X,1),
        write('.'),
        binding(V,lambda(X,V),NV),
        write_sem(V,NV),
        write_bc(N).

write_sem(appl(F,X),_) :-
        output_expl_brackets(no) ->
        write_fun_args(F,[X]),
        !.

write_sem(appl(X,Y),_) :-
        !,
        write('('),
        write_sem(X,1),
        write(' \\  '),
        write_sem(Y,1),
        write(')').

write_sem(pair(X,Y),_) :-
        !,
        write(' \\langle '),
        write_sem(X,1),
        write(' , '),
        write_sem(Y,1),
        write(' \\rangle ').

write_sem(fst(X),_) :-
        !,
        write(' \\pi^1'),
        binding(X,fst(X),NX),
        write_sem(X,NX).

write_sem(snd(X),_) :-
        !,
        write(' \\pi^2'),
        binding(X,snd(X),NX),
        write_sem(X,NX).

write_sem(conbox(X),_) :-
        !,
        write(' {}^{\\wedge} '),
        binding(X,conbox(X),NX),
        write_sem(X,NX).

write_sem(debox(X),_) :-
        !,
        write(' {}^{\\vee} '),
        binding(X,debox(X),NX),
        write_sem(X,NX).

write_sem(condia(X),_) :-
        !,
        write(' {}^{\\cap} '),
        binding(X,condia(X),NX),
        write_sem(X,NX).

write_sem(dedia(X),_) :-
        !,
        write(' {}^{\\cup} '),
        binding(X,dedia(X),NX),
        write_sem(X,NX).

write_sem('$VAR'(N),_) :-
        !,
        V is N mod 3,
        I is N // 3,
        sem_var_name(V,Name),
        format('~w_{~w}',[Name,I]).

write_sem(Const,_) :-
        write('\\mbox{\\bf '),
       (atom(Const) ->
        write_atom(Const)
       ;
        print(Const)),
        write('}').

sem_var_name(0,x).
sem_var_name(1,y).
sem_var_name(2,z).




% =

rule_name(R,N) :-
        rule_name1(R,N),
        !.
rule_name([R|Rs],N) :-
        !,
        rule_list(Rs,R,N).
rule_name(N,N). % N is a structural rule name

rule_name1(lex,'Lex').
rule_name1(uhyp,'Hyp').
rule_name1(hyp(_),'Hyp').
rule_name1(dle(_),'\\bs E').
rule_name1(dre(_),'/ E').
rule_name1(dli(_,_),'\\bs I').
rule_name1(dri(_,_),'/ I').
rule_name1(pe(_,_,_),'\\bullet E').
rule_name1(pi(_),'\\bullet I').
rule_name1(diae(_,_),'\\Diamond E').
rule_name1(diai(_),'\\Diamond I').
rule_name1(boxe(_),'\\Box E').
rule_name1(boxi(_),'\\Box I').

rule_list([],N,N) :- !.
rule_list([R|Rs],R0,(N0,N)) :-
        rule_name(R0,N0),
        rule_list(Rs,R,N).



rule_discharges(dli(_,X),[X]) :- !.
rule_discharges(dri(_,X),[X]) :- !.
rule_discharges(pe(_,X,Y),[X,Y]) :- !.
rule_discharges(diae(_,X),[X]) :- !.
rule_discharges(_,[]).


write_bo(0) :- write('(').
write_bo(1).

write_bc(0) :- write(')').
write_bc(1).



binding(T0,T,N) :-
   (output_expl_brackets(yes) ->
    N=0
   ;
    binds(T0,_,_,Num0),
    binds(T,Ass,Eq,Num),
    bind1(Eq,T0,Ass,Num0,Num,N)
   ),!.

binding(_,_,0).

bind1( _,T,T,M ,M,1) :- !.
bind1(=<,_,_,M0,M,N) :- (M >  M0 -> N = 0 ; N = 1).
bind1( <,_,_,M0,M,N) :- (M >= M0 -> N = 0 ; N = 1).

binds(dia(_,_),n,=<,20).
binds(box(_,_),n,=<,20).
binds(p(I,_,_),p(I,_,_),<,8) :- ignore_brackets(I),!.
binds(p(_,_,_),n,<,8).
binds(dl(_,_,_),n,<,4).
binds(dr(_,_,_),n,<,4).

binds(not(_),n,=<,20).
binds(dedia(_),n,=<,20).
binds(condia(_),n,=<,20).
binds(debox(_),n,=<,20).
binds(conbox(_),n,=<,20).
binds(quant(_,_,_),n,=<,12).
binds(lambda(_,_),n,=<,12).
binds(bool(_,&,_),bool(_,&,_),<,8).
binds(bool(_,\/,_),bool(_,\/,_),<,8).
binds(bool(_,->,_),n,<,4).
binds(merge(_,_),merge(_,_),<,2).


% write_mode(+Mode)

write_mode([]):-!.
write_mode(one(M)):-!,write(M),write('_{1}').
write_mode(two(M)):-!,write(M),write('_{2}').
write_mode(Atom):-write(Atom).


write_fun_args(Fun,As) :- 
        atomic_sem(Fun),
        !,
        write_sem(Fun,1),
        write('('),
        reverse(As,[],[B|Bs]),
        write_args(Bs,B),
        write(')').

write_fun_args(appl(X,Y),As) :-
        write_fun_args(X,[Y|As]).

write_args([],A) :-
        write_sem(A,1).
write_args([A|As],A0) :-
        write_sem(A0,1),
        write(' , '),
        write_args(As,A).

atomic_sem(At) :- atom(At),!.
atomic_sem('$VAR'(_)).



% =

write_quant(exists) :-
     !,
     write(' \\exists ').
write_quant(forall) :-
     !,
     write(' \\forall ').
write_quant(iota) :-
     !,
     write(' \\iota ').
write_quant(X) :-
     write(X).

% =

write_conn(&) :-
     !,
     write(' \\wedge ').
write_conn(\/) :-
     !,
     write(' \\vee ').
write_conn(->) :-
     !,
     write(' \\rightarrow ').
write_conn(neq) :-
     !,
     write(' \\neq ').
write_conn(X) :-
     write(X).

