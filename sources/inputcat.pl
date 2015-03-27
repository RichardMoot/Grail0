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

% = lazy evaluation

%lazy_dr(a).

% = transparency

%transparent(a).
%transparent(0).
%transparent_dia(p).

% = continuity

%continuous(0).
%continuous_dia(_).


i2p(Word::Type,lex(Word,Type1,Word)):-
	atom(Word),
	i2p(Type,Type1).
	
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

% lex/3 excised and put into testlex.pl
	
postulate(Out1,In1,Name):-
	Name # In--->Out,
	atom(Name),
	i2pp(In,In1),
	i2pp(Out,Out1).

lex(Word,Type,Word):-
	Word::Type1,
	i2p(Type1,Type).

lex(Word,Type,Sem) :-
	Word::Type1::Sem1,
	i2p(Type1,Type),
	t2p(Sem1,Sem).

macro(Macro,Type):-
	Macro:=Type1,
	i2p(Type1,Type).
	
example(Sentence,Type) :-
    Sentence ===> Type1,
    i2p(Type1,Type).
    
dummy:=dummy.
dummy # dummy ---> dummy.
