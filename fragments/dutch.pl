% -*- Mode: Prolog -*-
% ============================================================
% dutch.pl
% ============================================================
% !grail 3.1.1

% = Dutch verb clusters

% This grammar gives one of Michael Moortgat's solutions to the
% treatment of Dutch verbs in multimodal categorial grammars.
% It allows verb-initial, verb-second and verb-final sentences
% and gives the right interaction with wh extraction phenomena.

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

postulate(p(1,A,zip(e,B)), zip(e,p(1,A,B)), 'P1').
postulate(zip(0,A), zip(e,A), 'P2').
postulate(p(0,zip(0,A),zip(0,B)), zip(0,p(0,B,A)), 'P3').
postulate(p(0,p(1,A,B),zip(0,C)), p(1,A,p(0,B,zip(0,C))), 'P4').
postulate(p(1,A,zip(e1,B)), zip(i,p(1,B,A)), 'P5').
postulate(p(1,A,p(1,B,zip(e1,C))), p(1,p(1,A,B),zip(e1,C)), 'P6').
postulate(p(0,zip(0,A),zip(0,B)), p(1,A,zip(e1,B)), 'P71').
postulate(zip(0,A), zip(e1,A), 'P7').
postulate(p(1,A,zip(i,B)), zip(2,p(1,A,B)), 'P8').
postulate(p(1,A,p(1,zip(w,B),C)), p(1,zip(w,B),p(1,A,C)), 'P9').
postulate(p(1,p(1,zip(w,A),B),C), p(1,zip(w,A),p(1,B,C)), 'P10').

% = lazy evaluation

lazy_unpack(2).
lazy_unpack(i).

% = transparency

transparent(0).
transparent(2).
transparent(x).

transparent_dia(2).
transparent_dia(5).
transparent_dia(e).
transparent_dia(i).

% = continuity

continuous(2).
continuous(x).

continuous_dia(0).
continuous_dia(2).
continuous_dia(5).
continuous_dia(e).
continuous_dia(e1).
continuous_dia(i).
continuous_dia(w).

% = non internal modes

external(0).
external(1).
external(2).
external(x).

external_dia(0).
external_dia(2).
external_dia(5).
external_dia(e).
external_dia(e1).
external_dia(i).
external_dia(w).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(bang(A,B), dia(A,box(A,B))).
macro(iv, dl(1,np,s)).
macro(tv, dl(1,np,iv)).
macro(rel, dl(1,n,n)).
macro(relprosub, dr(1,rel,dl(1,np,sub))).
macro(relpro, dr(1,rel,dl(1,bang(w,np),sub))).
macro(rpro, dr(1,pp,dr(1,pp,np))).
macro(qprom_r, dr(1,qm,box(i,dl(1,bang(w,rpro),s)))).
macro(qpros_r, dr(1,qs,dl(1,bang(w,rpro),box(e,s)))).
macro(relpro_r, dr(1,rel,dl(1,bang(w,rpro),sub))).
macro(qpro_main, dr(1,qm,box(i,dl(1,bang(w,np),s)))).
macro(qpro_sub, dr(1,xcs,dl(1,bang(w,np),box(e,s)))).
macro(dec, box(2,s)).
macro(sub, box(e,s)).
macro(xcs, dl(x,dl(1,bang(w,cs),s),s)).
macro(tepro, bang(5,te)).
macro(xte, dl(xt,dl(1,dia(w,box(5,te)),s),s)).
macro(prep, dr(1,pp,np)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(wie, qpro_main, who).
lex(wat, qpro_main, what).
lex(welk, dr(1,qpro_main,n), which).
lex(wie, qpro_sub, lambda(A,lambda(B,appl(B,appl(who,A))))).
lex(wat, qpro_sub, lambda(A,lambda(B,appl(B,appl(what,A))))).
lex(welk, dr(1,qpro_sub,n), lambda(A,lambda(B,appl(B,appl(which,A))))).
lex(die, relpro, lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(waar, relpro_r, waar).
lex(waar, qprom_r, waar).
lex(waar, qpros_r, waar).
lex(er, rpro, er).
lex(boek, n, boek).
lex(meisje, n, meisje).
lex(taarten, n, tarts).
lex(het, dr(1,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(de, dr(1,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(koningin, n, queen).
lex(soepschildpad, n, lambda(A,appl(mock,appl(turtle,A)))).
lex(toe, dl(1,rpro,pp), lambda(A,appl(A,to))).
lex(mee, dl(1,rpro,pp), lambda(A,appl(A,with))).
lex(tot, prep, to).
lex(met, prep, with).
lex(op, prep, on).
lex(is, box(0,dl(1,ap,iv)), is).
lex(zijn, box(0,dl(1,ap,inf)), be).
lex(bereid, box(0,dl(1,pp,ap)), prepared).
lex(rekent, box(0,dl(1,pp,iv)), counts).
lex(rekenen, box(0,dl(1,pp,inf)), count).
lex(fred, np, fred).
lex(marie, np, mary).
lex(alice, np, alice).
lex(a, np, alice).
lex(jan, np, john).
lex(gek, ap, silly).
lex(vinden, box(0,dl(1,ap,dl(1,np,inf))), find).
lex(vindt, box(0,dl(1,ap,tv)), find).
lex(zal, box(0,dl(0,inf,iv)), will).
lex(wil, box(0,dl(0,inf,iv)), want).
lex(moeten, box(0,dl(0,inf,inf)), must).
lex(kunnen, box(0,dl(0,inf,inf)), can).
lex(willen, box(0,dl(0,inf,inf)), want).
lex(plagen, box(0,dl(1,np,inf)), tease).
lex(stelen, box(0,dl(1,np,inf)), steal).
lex(plaagt, box(0,tv), tease).
lex(slaapt, box(0,iv), sleeps).
lex(slapen, box(0,inf), sleep).
lex(weg, prt, away).
lex(sturen, box(0,dl(2,prt,dl(1,np,inf))), send).
lex(stuurt, box(0,dl(2,prt,tv)), send).
lex(weet, box(0,dl(1,cs,iv)), knows).
lex(weten, box(0,dl(1,cs,inf)), know).
lex(denkt, box(0,dl(1,cs,iv)), thinks).
lex(denken, box(0,dl(1,cs,inf)), think).
lex(probeert, box(0,dl(0,te,iv)), try).
lex(proberen, box(0,dl(0,te,inf)), try).
lex(overtuigt, box(0,dl(0,bang(5,te),tv)), convinces).
lex(overtuigen, box(0,dl(0,bang(5,te),dl(1,np,inf))), convince).
lex(te, box(0,dl(0,inf,te)), lambda(A,A)).
lex(het, cs, it).
lex(het, tepro, it).
lex(ervan, tepro, it).
lex(als, dr(1,als,sub), if).
lex(of, dr(1,of,sub), whether).
lex(dat, dr(1,dat,sub), that).
lex(dan, dr(1,dan,box(i,s)), then).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example(" Als Alice de soepschildpad plaagt.", als).
example(" Of Alice de koningin gek vindt.", of).
example(" Als Alice de Soepschildpad wil plagen.", als).
example(" Als Alice de Soepschildpad wil kunnen plagen.", als).
example(" Dat Alice de Soepschildpad probeert te plagen.", dat).
example(" Dat Alice de Soepschildpad probeert te kunnen plagen.", dat).
example(" Plaagt Alice de Soepschildpad?", box(i,s)).
example(" Vindt Alice de Koningin gek?", box(i,s)).
example(" Probeert Alice de Koningin te plagen?", box(i,s)).
example(" Alice zal de Soepschildpad willen plagen.", dec).
example(" De Soepschildpad die Alice wil plagen.", np).
example(" Wie wil de taarten stelen.", qm).
example(" Het meisje waar de Koningin op rekent.", np).
