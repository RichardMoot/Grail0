% -*- Mode: Prolog -*-
% ============================================================
% q.pl
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
:- dynamic postulate/3,postulate1/3.
:- dynamic macro/2,lex/3,example/2.

% ============================================================
% Postulates
% ============================================================

% = structural postulates

postulate(p(0,A,p(2,B,C)), p(2,B,p(0,A,C)), 'MC').
postulate(p(0,p(2,A,B),C), p(2,A,p(0,B,C)), 'MA').
postulate(p(1,A,p(0,B,C)), p(0,B,p(1,A,C)), 'MC').
postulate(p(1,A,p(0,B,C)), p(0,p(1,A,B),C), 'MA').


postulate(p(0,A,zip(p,B)), p(0,zip(p,B),A), 'Cp').
postulate(p(0,p(0,zip(p,A),B),C), p(0,zip(p,A),p(0,B,C)), 'MAp').
postulate(p(0,A,p(0,zip(p,B),C)), p(0,zip(p,B),p(0,A,C)), 'MCp').
%postulate(p(1,A,p(3,B,C)), p(3,p(1,A,B),C), than).


% = lazy evaluation

lazy_dl(1).

% = transparency

transparent(0).
transparent(1).
transparent(2).

transparent_dia(i).
transparent_dia(l).
transparent_dia(p).
transparent_dia(r).
transparent_dia(t).

% = continuity

%continuous(0).

continuous_dia(xxx).

% = non internal modes

external(_).

external_dia(_).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(q(A,B,C),p(1,dr(1,C,dl(2,A,B)),dl(2,A,A))).
macro(bang(A,B), dia(A,box(A,B))).
macro(rel, box(i,dl(0,n,n))).
macro(relbody, dl(0,bang(p,np),s)).
macro(relpro, dr(0,rel,relbody)).
macro(relpro(A), q(np,A,dr(0,rel,dl(0,bang(p,A),s)))).
macro(relpropp, q(np,np,relpro)).
macro(det, dr(0,gq,n)).
macro(iv, dl(0,np,s)).
macro(tv, dr(0,iv,np)).
macro(gq, q(np,s,s)).
% macro(v_ex, dl(0,bang(p,tv),s)).
macro(v_ex, q(tv,s,s)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(who, relpro, lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(whom, q(np,dr(0,s,dl(0,np,s)),dr(0,rel,dl(0,bang(p,np),s))), pair(lambda(A,lambda(B,lambda(C,lambda(D,bool(appl(C,D),&,appl(appl(A,D),lambda(E,appl(B,E)))))))),lambda(F,F))).
lex(whom, relpro(pp), pair(lambda(A,lambda(B,lambda(C,lambda(D,bool(appl(C,D),&,appl(B,appl(A,D))))))),lambda(E,E))).
lex(ehh, bang(p,dr(0,s,s)), lambda(A,A)).
lex(john, np, john).
lex(fred, np, fred).
lex(charles, np, charles).
lex(tlg, np, through_the_looking_glass).
lex(mathematician, n, mathematician).
lex(logic, np, logic).
lex(phonetics, np, phonetics).
lex(schoolboy, n, schoolboy).
lex(girl, n, girl).
lex(author, n, author).
lex(book, n, book).
lex(authors, n, authors).
lex(books, n, books).
lex(representative, dr(0,n,pp), representative_of).
lex(samples, n, samples).
lex(company, n, company).
lex(talks, dr(0,iv,pp), talks).
lex(to, dr(0,pp,np), to).
lex(thinks, dr(0,iv,s), thinks).
lex(believes, dr(0,iv,s), believes).
lex(needs, dr(0,iv,dr(0,s,dl(0,np,s))), needs).
lex(studies, tv, studies).
lex(likes, tv, likes).
lex(loves, tv, loves).
lex(tested, tv, test).
lex(hates, tv, hates).
lex(wrote, tv, wrote).
lex(the, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(most, det, lambda(A,pair(lambda(B,appl(appl(sub(most,C),appl(A,C)),appl(B,C))),lambda(D,D)))).
lex(a, det, lambda(A,pair(lambda(B,quant(exists,C,bool(appl(A,C),&,appl(B,C)))),lambda(D,D)))).
lex(every, det, lambda(A,pair(lambda(B,quant(forall,C,bool(appl(A,C),->,appl(B,C)))),lambda(D,D)))).
lex(everyone, gq, pair(lambda(A,quant(forall,B,appl(A,B))),lambda(C,C))).
lex(somebody, gq, pair(lambda(A,quant(exists,B,appl(A,B))),lambda(C,C))).
lex(friend, dr(0,n,pp), friend_of).
lex(of, dr(0,pp,np), lambda(A,A)).
lex(left, iv, left).
lex(today, dl(0,iv,iv), lambda(A,lambda(B,appl(today,appl(A,B))))).
lex(mary, np, mary).
lex(himself, q(np,iv,iv), pair(lambda(A,lambda(B,appl(appl(A,B),B))),lambda(C,C))).
lex(more, dr(0,q(np,s,dr(0,s,s_than)),n), lambda(A,pair(lambda(B,lambda(C,appl(appl(more_than,lambda(D,bool(appl(A,D),&,appl(B,D)))),C))),lambda(E,E)))).
lex(than, dr(0,s_than,dl(0,bang(p,det),s)), lambda(A,lambda(B,appl(A,lambda(C,pair(lambda(D,bool(appl(C,B),&,appl(D,B))),lambda(E,E))))))).
lex(someone, dr(0,s,iv), lambda(A,quant(exists,B,appl(A,B)))).
lex(is_missing, dl(0,dr(0,s,iv),s), missing).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("Who John talks to.", rel).
example("The friend of whom John talks to.", rel).
example("Whom John talks to a friend of.", rel).
example("Girl whom John talks to a friend of.", n).
example("A friend of whom John talks to.", rel).
example("The girl a friend of whom John talks to left.", s).
example("Whom every girl talks to a friend of.", rel).
example("A friend of whom every girl talks to.", rel).
example("John hates more books than Mary likes authors.", s).
example("Fred thinks every schoolboy believes a mathematician wrote tlg.", s).
example("John likes the author whom a girl hates.", s).
example("Every representative of a company tested most samples.", s).
