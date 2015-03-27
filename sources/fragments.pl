% ---------------------------------------------------------------------
% $Id: fragments.pl,v 1.8 2002/12/04 20:39:11 gjv Exp $
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


:- module(fragments,
          [consult_fragment/1,
          portray_examples/0,
          portray_lexicon/0,
          macro_expand/2,
          macro_reduce/2,
          lazy_unpack/1, 
          lazy_dr/1,
          lazy_dl/1, 
          transparent_dia/1,
          transparent/1,
          continuous_dia/1,
          continuous/1,
          external_dia/1,
          external/1,
          postulate/3,
          postulate1/3,
          macro/2,
          lex/3,
          example/2,
          special_string/2,
          atomic_formula/1]).

:- use_module(compatibility).

:- use_module(auxiliaries).

:- dynamic lazy_unpack/1,lazy_dr/1,lazy_dl/1.
:- dynamic transparent_dia/1,transparent/1.
:- dynamic continuous_dia/1,continuous/1.
:- dynamic external_dia/1,external/1,atomic_formula/1.
:- dynamic postulate/3,postulate1/3,special_string/2.
:- dynamic macro/2,lex/3,example/2.

consult_fragment(FragmentName) :-
	fragment_dir(Dir),
	generate_file_name(Dir, FragmentName, PathToFile),
	abolish(atomic_formula/1),
	abolish(special_string/2),
	[PathToFile],
	assert(atomic_formula('dummy argument')),
	assert(special_string('dummy argument','dummy argument')).

% ============================================================
% Lexicon
% ============================================================

% expand macro definitions

macro_expand(S0,S) :-
     apply_macro(S0,S1),
     !,
     macro_expand(S1,S).

macro_expand(S,S).

% reduce macro definitions

macro_reduce(S0,S) :-
     apply_macro(S1,S0),
     !,
     macro_reduce(S1,S).

macro_reduce(S,S).


apply_macro(dia(I,S0),dia(I,S)) :-
     apply_macro(S0,S).
apply_macro(box(I,S0),box(I,S)) :-
     apply_macro(S0,S).
apply_macro(p(I,R0,S),p(I,R,S)) :-
     apply_macro(R0,R).
apply_macro(p(I,R,S0),p(I,R,S)) :-
     apply_macro(S0,S).
apply_macro(dl(I,R0,S),dl(I,R,S)) :-
     apply_macro(R0,R).
apply_macro(dl(I,R,S0),dl(I,R,S)) :-
     apply_macro(S0,S).
apply_macro(dr(I,R0,S),dr(I,R,S)) :-
     apply_macro(R0,R).
apply_macro(dr(I,R,S0),dr(I,R,S)) :-
     apply_macro(S0,S).
apply_macro(S0,S) :-
     macro(S0,S).
apply_macro(S,lit(S)) :-
     literal(S).


% = literal(+Syn) 
% true if Syn abbreviates lit(Syn), i.e. is a basic syntactic category

literal(X) :- 
     atom(X),
     !.
literal(X) :- 
     atomic_formula(X).


portray_examples :-
	format('~61t~60|~n',[]),
	format('=~t Examples ~t=~60|~n',[]),
	format('~61t~60|~n',[]),
        findall(S-F,example(S,F),List),
        portray_examples(List).

portray_examples([]) :- nl,nl.

portray_examples([S-F|R]) :-
       format('~nexample("~s",~w).',[S,F]),
       portray_examples(R).

portray_lexicon :-
	findall(W,lex(W,_X,_Y),Ws0),
	sort(Ws0,Ws),
	length(Ws,L0),
	L is integer((L0/3)+1),
	split_num(Ws,L,Lex1,Ws1),
	split_num(Ws1,L,Lex2,Ws2),
	split_num(Ws2,L,Lex3,[]),
	format('~61t~60|~n',[]),
	format('=~t Lexicon ~t=~60|~n',[]),
	format('~61t~60|~2n',[]),
	portray_lexicon(Lex1,Lex2,Lex3).

portray_lexicon([],[],[]).

portray_lexicon([X|Xs],[Y|Ys],[Z|Zs]) :-
	format('~w~t~20|~w~t~40|~w~t~60|~n',[X,Y,Z]),
	portray_lexicon(Xs,Ys,Zs).


split_num([],N0,Ys,[]) :-
	( N0 > 0 ->
	    N is N0-1,
	    Ys = [''|Ys0],
	    split_num([],N,Ys0,[])
	;
	    Ys=[]
	).
	
split_num([X|Xs],N0,Ys,Zs) :-
	( N0 =< 0 ->
	    Zs=[X|Xs],
	    Ys=[]
	;
	    N is N0-1,
	    Ys=[X|Ys0],
	    split_num(Xs,N,Ys0,Zs)
	).




init :-
	get_environment_variable('GRAIL_FRAGMENTS_DIR',  '../fragments', RawFragmentDir),
	canonicalize_path(RawFragmentDir, FragmentDir),
	assert(fragment_dir(FragmentDir)).


:- init.
