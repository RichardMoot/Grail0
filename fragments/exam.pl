% ============================================================
% Grail
% ============================================================
% !grail 3.1.1

:- abolish(lazy_unpack/1).
:- abolish(lazy_dr/1).
:- abolish(lazy_dl/1).
:- abolish(transparent_dia/1).
:- abolish(transparent/1).
:- abolish(continuous_dia/1).
:- abolish(continuous/1).
:- abolish(external_dia/1).
:- abolish(external/1).
:- abolish(postulate/3).
:- abolish(postulate1/3).
:- abolish(macro/2).
:- abolish(lex/3).
:- abolish(example/2).

:- dynamic lazy_unpack/1,lazy_dr/1,lazy_dl/1.
:- dynamic transparent_dia/1,transparent/1.
:- dynamic continuous_dia/1,continuous/1.
:- dynamic external_dia/1,external/1.
:- dynamic postulate/3,postulate1/3,special_string/2.
:- dynamic macro/2,lex/3,example/2.

% ============================================================
% = Structural Conversions
% ============================================================

postulate(p(a,p(a,A,B),C),p(a,A,p(a,B,C)),'Ass1'). % ass1
postulate(p(a,A,p(a,B,C)),p(a,p(a,A,B),C),'Ass2'). % ass2

% ============================================================
% = Macros
% ============================================================

atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).

:- dynamic macro/2.

macro(iv, dl(a, np, s)).
macro(vp, dl(a, np, s)).
macro(tv, dr(a, iv, np)).

continuous(a).
continuous(n).

external(a).
external(n).

% ============================================================
% = Lexicon
% ============================================================

:- dynamic lex/3.

lex(every, dr(n,dr(a,s,dl(a,np,s)),n), lambda(P,lambda(Q,quant(forall,X,bool(appl(P,X),->,appl(Q,X)))))).
lex(some,  dr(n,dl(a,dr(a,s,np),s),n), lambda(P,lambda(Q,quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(student, n, student).
lex(exam, n, exam).
lex(was, dr(n,vp,dr(n,n,n)), lambda(Adj, lambda(S, appl(appl(is,appl(Adj,lambda(_,true))),S)))).
lex(difficult, dr(n,n,n), lambda(P,lambda(X,bool(appl(P,X),&,appl(difficult,X))))).
lex(erratic, dr(n,n,n), lambda(P,lambda(X,bool(appl(P,X),&,appl(erratic,X))))).
lex(aced, dr(a,dl(a,np,s),np), ace).
lex(slept, iv, sleep).
lex(during, dr(n,dl(a,iv,iv),np), lambda(X,lambda(P,lambda(Y,appl(appl(during,X),appl(P,Y)))))).
lex(the, dr(n,np,n), iota).
lex(who, dr(n,dl(n,n,n),dl(a,np,s)), lambda(P,lambda(Q,lambda(X,bool(appl(Q,X),&,appl(P,X)))))).
lex(loves, tv, love).
lex(alyssa, np, alyssa).

example("Every student aced some exam.", s).
example("The student who slept during the exam loves Alyssa", s).
example("The exam was difficult erratic.", s).
