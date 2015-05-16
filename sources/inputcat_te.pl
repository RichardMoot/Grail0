% ============================================================
% File header
% ============================================================
% !labels v1.0

?-op(300,xfx,['\\/','*','\\']).
?-op(500,yfx,'@').
?-op(600,xfy,'^').
?-op(250,fy,['<>','[]']).   
?-op(400,xfx,['--->','===>']).
?-op(700,xfy,'#').
?-op(800,xfy,['::',':=']).

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

external(_).
external_dia(_).

	
term_expansion((Name # In--->Out), postulate(Out1,In1,Name)) :-
        !,
	atom(Name),
	i2pp(In,In1),
	i2pp(Out,Out1).

term_expansion(Word::Type1::Sem1, lex(Word,Type,Sem)) :-
	!,
	i2p(Type1,Type),
	t2p(Sem1,Sem).

term_expansion(Word::Type1, lex(Word,Type,Word)) :-
	!,
	i2p(Type1,Type).

term_expansion(Macro:=Type1, macro(Macro,Type)) :-
	!,
	i2p(Type1,Type).
	
term_expansion((Sentence ===> Type1), example(Sentence,Type)) :-
	!,
	i2p(Type1,Type).

	
i2p(Atom,Atom):-atom(Atom).

i2p(A*B,p([],A1,B1)):-
	i2p(A,A1),
	i2p(B,B1).

i2p(A/B,dr([],A1,B1)):-
	i2p(A,A1),
	i2p(B,B1).
	
i2p(A\B,dl([],A1,B1)):-
	i2p(A,A1),
	i2p(B,B1).
	
i2p(<>A,dia([],A1)):-
	i2p(A,A1).

i2p([]A,box([],A1)):-
	i2p(A,A1).
	
i2pp(Structure,Structure1):-
	var(Structure),
	!,
	Structure=Structure1.
	
i2pp(A*B,p([],A1,B1)):-
	!,
	i2pp(A,A1),
	i2pp(B,B1).
	
i2pp(<>A,zip([],A1)):-
	!,
	i2pp(A,A1).

t2p(X,M):-
        (atom(X);var(X)),!,X=M.

t2p(fst(M),fst(M1)):-
        t2p(M,M1).

t2p(snd(M),snd(M1)):-
        t2p(M,M1).
        
t2p((M,N),pair(M1,N1)):-
        t2p(M,M1),
        t2p(N,N1).
        
t2p(M@N,appl(M1,N1)):-
        t2p(M,M1),
        t2p(N,N1).

t2p(X^M,lambda(X,M1)):-
        t2p(M,M1).

t2p(M/\N,bool(M1,'&',N1)):-
        t2p(M,M1),
        t2p(N,N1).

t2p(M\/N,bool(M1,'\\/',N1)):-
        t2p(M,M1),
        t2p(N,N1).
        
t2p(M>>N,bool(M1,'->',N1)):-
        t2p(M,M1),
        t2p(N,N1).

t2p(-(M),not(M1)):-
        t2p(M,M1).


% start
%
% exports the current fragment as an separate "vanilla" Grail file, no longer dependent on any operator definition or translations

start :-
	format(':- abolish(lazy_unpack/1).~n:- abolish(lazy_dr/1).~n:- abolish(lazy_dl/1).~n:- abolish(transparent_dia/1).~n:- abolish(transparent/1).~n',[]),
	format(':- abolish(continuous_dia/1).~n:- abolish(continuous/1).~n:- abolish(external_dia/1).~n:- abolish(external/1).~n', []),
	format(':- abolish(postulate/3).~n:- abolish(postulate1/3).~n:- abolish(macro/2).~n:- abolish(lex/3).~n:- abolish(example/2).~2n', []),
	format('external(_).~nexternal_dia(_).~2n', []),
	print_continuous,
	listing(postulate/3),
	listing(macro/2),
	listing(lex/3),
	listing(example/2).

print_continuous :-
	findall(A-B, postulate(A,B,_), List),
	all_continuous(List).

all_continuous([]) :-
	format('~2ncontinuous(_).~n', []).
all_continuous([A-B|Rest]) :-
	continuous(A, B),
	all_continuous(Rest).

continuous(A, B) :-
	yield(A, ListA, []),
	yield(B, ListB, []),
	ListA == ListB.

yield(A, Ys0, Ys) :-
	var(A),
	!,
	/* workaround needed because '[]' has been designed as an operator :( */
	Ys0 = '.'(A,Ys).
yield(p(_,A,B)) -->
	yield(A),
	yield(B).
yield(zip(_,A)) -->
	yield(A).
