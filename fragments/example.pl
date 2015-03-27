% -*- Mode: Prolog -*-
% ============================================================
% example.pl
% ============================================================
% !grail 1.1

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

% Write your structural rules here, the format is
%
% postulate(Left, Right, Name).
%
% where both Left and Right are terms using the predefined
% constructors:
% - p(I,A,B) (product, mode I, subterms A and B)
% - zip(I,A) (diamond, mode J, subterm A).
% - leaves must be Prolog variables, with each variable occurring
%   exactly once in term Left and exactly once in term Right
%
% = structural postulates

postulate(p(1,A,p(0,B,C)), p(0,p(1,A,B),C), 'MA').
postulate(p(0,p(0,zip(p,A),B),C), p(0,zip(p,A),p(0,B,C)), 'MAp').

% WARNING: setting the optimization options below incorrectly may
% cause Grail to miss derivations!

% SAFE SETTINGS:
% - no modes declared transparent
% - no modes declared continuous
% - all modes declared external

% = transparency
%
% modes declared as transparent will require the label to be
% *immediately* rewriteable to the correct word order
% (safest bet: don't declare anything here unless you know
%  what you are doing!).

% = continuity
%
% modes declared as continuous cannot change the order of the
% labels (typical example: associativity, which changes the
% structure but not the order).
% If all postulates where the mode is mentioned keep the order
% of the leaves constant, then you can declare a mode continuous.

% = non internal modes
%
% if a mode I is not declared external, then any labels containing
% mode I will be rejected as possible solutions.
% As an example Morrill's (1994) analysis of wrapping would declare
% mode a and mode n, but not mode w as external.
% (safest bet: declare all modes external).

external(_).

external_dia(_).

% ============================================================
% Macros
% ============================================================
% the macro facility allows you to rewrite a Prolog term into
% another Prolog term. The implementation uses forced determinism
% and doesn't check for termination. However, the main use is
% to allow you to write useful abbreviations, and use these
% abbreviations to create others.

% = macro(Form,Replacement)

macro(bang(A,B), dia(A,box(A,B))).
macro(iv, dl(0,np,s)).    % iv = np\s ("\" of mode 0)
macro(tv, dr(0,iv,np)).   % tv = iv/np = (np\s)/np (both slashes of mode 0)
                          % uses the previously defined iv macro

% ============================================================
% Lexicon
% ============================================================
% the lexicon speficies a Formula (possibly abbreviated using
% the macro declarations above) and a Semantics for each word.

% = lex(Word,Formula,Sem)

lex(john, np, john).
lex(fred, np, fred).
lex(slept, iv, sleep).
lex(charles, np, charles).
lex(mathematician, n, mathematician).
lex(schoolboy, n, schoolboy).
lex(girl, n, girl).
lex(author, n, author).
lex(book, n, book).
lex(someone, dr(0,s,iv), lambda(A,quant(exists,B,appl(A,B)))).
lex(is_missing, dl(0,dr(0,s,iv),s), missing).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("John slept.", s).
