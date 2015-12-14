% ---------------------------------------------------------------------
% $Id: grail.pl,v 1.35 2002/12/02 23:22:50 vermaat Exp $
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

% ============================================================
% Grail
% ============================================================

:- use_module([statistics,
               graphs,
               auxiliaries,
               tokenize,
               reduce_sem,
               fragments,
               options,
               nd]).


% ============================================================
% Help message
% ============================================================

grail_help :-
	format('~n',[]),
	format('load_output_module(Name)               Use output_Name for output.~n',[]),
	format('load_fragment(FileName)                Consult a grammar fragment.~n',[]),
	format('set_output_module(Directory)           Write output files to given directory.~n',[]),
	format('portray_examples                       Show example sentences of current fragment.~n',[]),
	format('portray_lexicon                        Show words in current lexicon.~n',[]),
	format('tokenize(String,ListOfWords)           Tokenize a string.~n',[]),
	format('parseall(ListOfWords,Formula)          Parse ListOfWords as Formula, generating output in selected format~n',[]),
	format('parseall(ListOfWords,Formula,Results)  Parse ListOfWords as Formula, generate Results~n',[]),
	format('generate_output(Results)               Generate output in selected format~n',[]),
	format('~n',[]),
	format('parse(ListOfWords,Formula)             *Deprecated!* use load_output_module(text)~n',[]),
	format('tex(ListOfWords,Formula)               *Deprecated!* use load_output_module(prawitz_tex)~n',[]),
	format('~n',[]),
	format('halt                                   Exit Prolog.~n',[]).



% ============================================================
% Top level
% ============================================================

% ---------------------------------------------------------------------
% tokenize(+String, -ListOfWords).
%
% Skips the first character (if it's defined as trailer_char) and
% passes to tokenize_string.
% ---------------------------------------------------------------------

tokenize(String, ListOfWords) :-
	tokenize_string(String, ListOfWords).


% ---------------------------------------------------------------------
% load_fragment(+Name)
%
% Loads fragment Name from the fragment_dir.
% ---------------------------------------------------------------------

load_fragment(Name) :-
	consult_fragment(Name).


% ---------------------------------------------------------------------
% load_output_module(+Name)
%
% Loads output module output_Name.
% ---------------------------------------------------------------------

load_output_module(NameSuffix) :-
	atom_concat('output_', NameSuffix, ModuleName),
	use_module(ModuleName),
	retractall('active output module'(_)),
	assert('active output module'(ModuleName)).


% ---------------------------------------------------------------------
% set_output_directory(+D)
%
% Write output files to directory D
% ---------------------------------------------------------------------

set_output_directory(Dir) :-
	retractall('latex output directory'(_)),
	atom_concat(Dir,'/',Dir2),
	assert('latex output directory'(Dir2)).


% ---------------------------------------------------------------------
% generate_output(+ResultList)
%
% Generate output using current output module.
% ---------------------------------------------------------------------

generate_output(ResultList) :-
	'active output module'(ModuleName),
	ModuleName:generate_output(ResultList).

% ---------------------------------------------------------------------
% = parseall(+ListOfWords,+GoalFormula,-Results)
%
% Parses ListOfWords as an expression of type GoalFormula,
%  unifying Results with all possible results.
%
% = parseall(+ListOfWords,+GoalFormula)
%
% Variant of parseall/3 where Results are passed directly to
% generate_output.
%  
% ---------------------------------------------------------------------

parseall(Words, Goal, Results) :-
	findall(Result, parse_words(Words, Goal, Result), Results).

parseall(Words, Goal) :-
	findall(Result, parse_words(Words, Goal, Result), Results),
	generate_output(Results).


parseall_syn(ListOfSyn, Goal, Results) :-
	Result = result(_, _, _, _, _, [], 0),
	findall(Result, parse_syn(ListOfSyn, Goal, Result), Results).

parseall_syn(ListOfSyn, Goal) :-
	Result = result(_, _, _, _, _, [], 0),
	findall(Result, parse_syn(ListOfSyn, Goal, Result), Results),
	generate_output(Results).


% ---------------------------------------------------------------------
% parse(+ListOfWords, +GoalType)
%
% Parses ListOfWords as goal type GoalType.
% fNf: DEPRECATED! Use parseall/3
% ---------------------------------------------------------------------

parse(ListOfWords, Goal) :-
	load_output_module(text),
	parseall(ListOfWords, Goal, Results),
	generate_output(Results).
	
% ---------------------------------------------------------------------
% parse_syn(+ListOfSyn, +GoalType)
%
% parse_syn/2 operates like parse/2 only this predicate assumes
% its first argument to be a list of formulas instead of a list
% of words.
% fNf: DEPRECATED! Use parseall/3
% ---------------------------------------------------------------------

parse_syn(ListOfSyn, Goal) :-
	load_output_module(text),
	parseall_syn(ListOfSyn, Goal, Results),
	generate_output(Results).

% ---------------------------------------------------------------------
% tex(+ListOfWords, +GoalType)
%
% Parses ListOfWords as goal type GoalType and creates tex output.
% ---------------------------------------------------------------------

tex(ListOfWords, Goal) :-
	load_output_module(prawitz_tex),
	parseall(ListOfWords, Goal, Results),
	generate_output(Results).

% ---------------------------------------------------------------------
% tex_syn(+ListOfWords,+GoalType)
%
% tex_syn/2 operates like tex/2 only this predicate assumes
% its first argument to be a list of formulas instead of a list
% of words.
% ---------------------------------------------------------------------

tex_syn(ListOfSyn, Goal) :-
	load_output_module(prawitz_tex),
	parseall_syn(ListOfSyn, Goal, Results),
	generate_output(Results).
       
start :-
	assert('latex output directory'('./')),
	load_output_module(null),
	format('~n~nWelcome to Grail!~nType \'grail_help.\' for toplevel predicates.~n~n',[]).

:- start.
