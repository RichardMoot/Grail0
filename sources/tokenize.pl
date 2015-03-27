% ---------------------------------------------------------------------
% $Id: tokenize.pl,v 1.10 2002/12/02 23:22:52 vermaat Exp $
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

:- module(tokenize,
          [tokenize_string/2]).

:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3]) ; true).

:- use_module(fragments).



tokenize_string([],[]).
tokenize_string([C|Cs],Res) :-
       trailer_char(C) -> tokenize_string1(Cs,Res)
     ;
       tokenize_string1([C|Cs],Res).

tokenize_string1([],[]).
tokenize_string1([C|Cs],Res) :-
        special([C|Cs],Ds,Lex) ->
          Res = [Lex|Res0],
          tokenize_string1(Ds,Res0)
      ; interpunction(C) ->
          tokenize_string(Cs,Res)
      ; tokenize_term([C|Cs],Ds,Word) ->
          Res = [Word|Res0],
          tokenize_string1(Ds,Res0)
      ; tokenize_string1(Cs,Res).

tokenize_word([],[],Ws0,[],Word) :-
       name(Word,Ws0).
tokenize_word([C|Cs],Ds,Ws0,Ws,Word) :-
       special([C|Cs],_,_) ->
         Ws=[],
         name(Word,Ws0),
         Ds=[C|Cs]
     ; alphanum_char(C,C1) ->
          Ws=[C1|Ws1],
          tokenize_word(Cs,Ds,Ws0,Ws1,Word)
     ; Ws=[],
       name(Word,Ws0),
       Ds=[C|Cs].

special([X|Xs],Ys,Res) :-
       special_string([Z|Zs],Res),
       append([Z|Zs],Ys,[X|Xs]).


% ---------------------------------------------------------------------
% = term tokenization
% tokenize_term(+String,-Term).
% tokenize a subset of Prolog syntax.
% Unsupported at this moment are: operator definitions, floating point
% numbers, comments and escape sequences.
% ---------------------------------------------------------------------

tokenize_term(String,Term) :-
       tokenize_term(String,Rest,Term),
     ( Rest = [] ->
       true
     ;
       append(Init,Rest,String),
       format('{Warning: Trailing material deleted.}~2n ~s<here>~s',[Init,Rest])
     ).

tokenize_term([],[],[]).
tokenize_term([C|Cs],Res0,Term) :-
       C=127 ->
         tokenize_term(Cs,Res0,Term)
     ; C<33 ->
         tokenize_term(Cs,Res0,Term)
     ; C=39 ->
       /* quoted term */
         tokenize_quoted_term(Cs,Res,L,[]),
         name(Term,L),
        ( Res = [39|Res1] ->
          trim_spaces(Res1,Res0)
        ;
          trim_spaces(Res,Res0),
          portray_missing_material('''',Res,[C|Cs])
         )
     ; C=40 ->
       /* (Term) */
         tokenize_term(Cs,Res,Term),
        ( Res = [41|Res1] ->
          trim_spaces(Res1,Res0)
        ;
          trim_spaces(Res,Res0),
          portray_missing_material(')',Res,[C|Cs])
        )
     ; C=91 ->
       /* list */
         tokenize_list(Cs,Res,Term),
        ( Res = [93|Res1] ->
          trim_spaces(Res1,Res0)
        ;
          trim_spaces(Res,Res0),
          portray_missing_material(']',Res,[C|Cs])
        )
     ; C=34 ->
       /* string */
         tokenize_string(Cs,Res,Term,[]),
        ( Res = [34|Res1] ->
          trim_spaces(Res1,Res0)
        ;
          trim_spaces(Res,Res0),
          portray_missing_material('"',Res,[C|Cs])
        )
     ; C=43,
       trim_spaces(Cs,[D|Ds]),
       D>47,
       D<58 ->
       /* positive integer */
          tokenize_number([D|Ds],Res,NumS,[]),
          number_chars(Term,NumS),
          trim_spaces(Res,Res0)
     ; C=45,
       trim_spaces(Cs,[D|Ds]),
       D>47,
       D<58 ->
       /* negative integer */
          tokenize_number([D|Ds],Res,NumS,[]),
          number_chars(Term,[45|NumS]),
          trim_spaces(Res,Res0)
     ; (C>47,
        C<58) ->
       /* unsigned integer */
          tokenize_number(Cs,Res,NumS,[]),
          number_chars(Term,[C|NumS]),
          trim_spaces(Res,Res0)
     ; symbol_char(C) ->
       /* symbols */
          tokenize_symbol(Cs,Res,Ss,[]),
          name(Term,[C|Ss]),
          trim_spaces(Res,Res0)
     ; alpha_char(C,C1) ->
          tokenize_word(Cs,Ds,[C1|Ws],Ws,Word),
        ( Ds = [40|Ds0] ->
          trim_spaces(Ds0,Ds1),
          /* functor/arguments */
          tokenize_args(Ds1,Res,TermList,[]),
          Term =.. [Word|TermList],
           (Res=[41|Res1] ->
            trim_spaces(Res1,Res0)
           ;
            trim_spaces(Res,Res0),
            portray_missing_material(')',Res,[C|Cs])
           )
        ;
          /* atom */
          Term=Word,
          trim_spaces(Ds,Res0)
        ).

portray_missing_material(Char,Bs,Cs) :-
       append(As,Bs,Cs),
       format('{Warning: Missing "~w" inserted.}~2n ~s<here>~s',[Char,As,Bs]).

tokenize_symbol([],[],S,S).
tokenize_symbol([S|Rs0],Rs,Ss0,Ss) :-
       symbol_char(S) ->
          Ss0=[S|Ss1],
          tokenize_symbol(Rs0,Rs,Ss1,Ss)
     ; Rs=[S|Rs0],
       Ss0=Ss.

tokenize_number([],[],NumS,NumS).
tokenize_number([C|Cs],Ds,NumS0,NumS) :-
       C>47,
       C<58 ->
         NumS0=[C|NumS1],
         tokenize_number(Cs,Ds,NumS1,NumS)
     ;
       Ds=[C|Cs],
       NumS0=NumS.

tokenize_quoted_term([],[],L,L).
tokenize_quoted_term([C|Cs0],Ds,L0,L) :-
       [C|Cs0] = [39,39|Cs] ->
          L0=[39|L1],
          tokenize_quoted_term(Cs,Ds,L1,L)
     ; C=39 ->
          L0=L,
          Ds=[C|Cs0]
     ; L0=[C|L1],
        tokenize_quoted_term(Cs0,Ds,L1,L).

tokenize_string([],[],Term,Term).
tokenize_string([C|Cs0],Ds,T0,T) :-
       [C|Cs0] = [34,34|Cs] ->
         T0=[34|T1],
         tokenize_string(Cs,Ds,T1,T)
     ; C=34 ->
         T0=T,
         Ds=[C|Cs0]
     ; T0=[C|T1],
       tokenize_string(Cs0,Ds,T1,T).

tokenize_list([],[],[]).
tokenize_list([C|Cs],Ds,L) :-
       C=93 ->
         Ds=[C|Cs],
         L=[]
     ; tokenize_term([C|Cs],Res0,Term),
       ( Res0 = [44|Res1] ->
           trim_spaces(Res1,Res),
           L=[Term|L0],
           tokenize_list(Res,Ds,L0)
       ; Res0 = [124|Res1] ->
           trim_spaces(Res1,Res),
           L=[Term|L0],
           tokenize_term(Res,Ds,L0)
       ; Res0 = [93|Res1] ->
           Ds=[93|Res1],
           L=[Term]
       ; Ds=Res0,
         L=[Term]
        ).

tokenize_args([],[],Args,Args).
tokenize_args([C|Cs0],Es,As0,As) :-
       tokenize_term([C|Cs0],Ds,Arg),
     ( Ds=[44|Ds0] ->
       As0=[Arg|As1],
       trim_spaces(Ds0,Ds1),
       tokenize_args(Ds1,Es,As1,As)
     ;
       Es=Ds,
       As0=[Arg|As]
     ).

trim_spaces([],[]).
trim_spaces([X|Xs],Ys) :-
       X=127 ->
         trim_spaces(Xs,Ys)
     ; X<33 ->
         trim_spaces(Xs,Ys)
     ; Ys=[X|Xs]. 


% The Prolog symbol characters. 

symbol_char( 35). % #
symbol_char( 36). % $
symbol_char( 38). % &
symbol_char( 42). % *
symbol_char( 43). % +
symbol_char( 45). % -
symbol_char( 46). % .
symbol_char( 47). % /
symbol_char( 58). % :
symbol_char( 60). % <
symbol_char( 61). % =
symbol_char( 62). % >
symbol_char( 63). % ?
symbol_char( 64). % @
symbol_char( 92). % \
symbol_char( 94). % ^
symbol_char( 96). % `
symbol_char(126). % ~

% Interpunction characters should not be treated
% as Prolog terms when they occur in a sentence.

interpunction(33). % !
interpunction(34). % "
interpunction(39). % '
interpunction(45). % -
interpunction(46). % .
interpunction(58). % :
interpunction(59). % ;
interpunction(63). % ?
interpunction(96). % `

alpha_char(X,Y) :-
     X>96 ->
     /* lower case letter */
        X<123,
        Y=X
   ; X>64 ->
     /* upper case letter */
        X<91,
        to_lower_offset(Z),
        Y is X+Z.

alphanum_char(X,Y) :-
     X=95 ->
     /* underline */
        Y=95
   ; X>96 ->
     /* lower case letter */
        X<123,
        Y=X
   ; X>64 ->
     /* upper case letter */
        X<91,
        to_lower_offset(Z),
        Y is X+Z
   ; X>47 ->
     /* numerical */
        X<58,
        Y=X.

% =

to_lower_offset(32).

% =

trailer_char(32). % space
trailer_char(42). % *
trailer_char(63). % ?
