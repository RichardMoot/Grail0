% ============================================================
% q.pl
% ============================================================
% !labels v1.0

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
:- dynamic postulate/3,postulate1/3.
:- dynamic macro/2,lex/3,example/2.

% ============================================================
% Postulates
% ============================================================

% = structural postulates

postulate(zip(i,A), p(1,A,x-'I'), 'I').
postulate(p(1,A,x-'I'), zip(i,A), 'I').
postulate(p(0,A,p(1,B,C)), p(1,B,p(0,p(0,x-'B',A),C)), 'B').
postulate(p(0,p(1,A,B),C), p(1,A,p(0,p(0,x-'C',B),C)), 'C').
postulate(p(1,B,p(0,p(0,x-'B',A),C)), p(0,A,p(1,B,C)), 'B').
postulate(p(1,A,p(0,p(0,x-'C',B),C)), p(0,p(1,A,B),C), 'C').

% = lazy evaluation

lazy_dl(_).
lazy_dr(_).


% = transparency

transparent('$NONE').

transparent_dia('$NONE').

% = continuity

continuous('$NONE').

continuous_dia(_).

% = non internal modes

external(_).

external_dia(_).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(q(A,B,C), dia(i,dr(1,C,dl(1,box(i,A),B)))).
macro(bang(A,B), dia(A,box(A,B))).
macro(adj, dr(0,n,n)).
macro(rel, box(i,dl(0,n,n))).
macro(relbody, dl(0,bang(p,np),s)).
macro(relpro, dr(0,rel,relbody)).
macro(relpro(A), q(np,A,dr(0,rel,dl(0,bang(p,A),s)))).
macro(relpropp, q(np,np,relpro)).
macro(det, dr(0,gq,n)).
macro(iv, dl(0,np,s)).
macro(vp, dl(0,np,s)).
macro(tv, dr(0,iv,np)).
macro(gq, q(np,s,s)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(same, q(adj,vp,vp), same).
lex(who, relpro, lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(whom, q(np,dr(0,s,dl(0,np,s)),dr(0,rel,dl(0,bang(p,np),s))), lambda(A,lambda(B,lambda(C,lambda(D,bool(appl(C,D),&,appl(appl(A,D),lambda(E,appl(B,E))))))))).
lex(whom, relpro(pp), lambda(A,lambda(B,lambda(C,lambda(D,bool(appl(C,D),&,appl(B,appl(A,D)))))))).
lex(ehh, bang(p,dr(0,s,s)), lambda(A,A)).
lex(john, np, john).
lex(fred, np, fred).
lex(tlg, np, through_the_looking_glass).
lex(mathematician, n, mathematician).
lex(schoolboy, n, schoolboy).
lex(girl, n, girl).
lex(author, n, author).
lex(waiter, n, waiter).
lex(book, n, book).
lex(authors, n, authors).
lex(books, n, books).
lex(talks, dr(0,iv,pp), talks).
lex(to, dr(0,pp,np), to).
lex(thinks, dr(0,iv,s), thinks).
lex(believes, dr(0,iv,s), believes).
lex(needs, dr(0,iv,dr(0,s,dl(0,np,s))), needs).
lex(likes, tv, likes).
lex(hates, tv, hates).
lex(wrote, tv, wrote).
lex(served, tv, serve).
lex(read, tv, read).
lex(the, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(a, det, lambda(A,lambda(B,quant(exists,C,bool(appl(A,C),&,appl(B,C)))))).
lex(every, det, lambda(A,lambda(B,quant(forall,C,bool(appl(A,C),->,appl(B,C)))))).
lex(everyone, gq, lambda(A,quant(forall,B,appl(A,B)))).
lex(somebody, gq, lambda(A,quant(exists,B,appl(A,B)))).
lex(friend, dr(0,n,pp), friend).
lex(of, dr(0,pp,np), of).
lex(left, iv, left).
lex(today, dl(0,iv,iv), lambda(A,lambda(B,appl(today,appl(A,B))))).
lex(mary, np, marie).
lex(himself, q(np,iv,iv), lambda(A,lambda(B,appl(appl(A,B),B)))).
lex(more, dr(0,q(np,s,dr(0,s,s_than)),n), lambda(A,lambda(B,lambda(C,appl(appl('more-than',lambda(D,bool(appl(A,D),&,appl(B,D)))),C))))).
lex(than, dr(0,s_than,dl(0,bang(p,det),s)), lambda(A,lambda(B,appl(A,lambda(C,lambda(D,bool(appl(C,B),&,appl(D,B)))))))).
lex(someone, dr(0,s,iv), lambda(A,quant(exists,B,appl(A,B)))).
lex(is_missing, dl(0,dr(0,s,iv),s), missing).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("Everyone loves somebody.", s).
example("Everyone read the same book.", s).
example("The same waiter served everyone.", s).
