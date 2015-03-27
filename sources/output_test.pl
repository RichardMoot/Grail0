% ---------------------------------------------------------------------
% $Id: output_test.pl,v 1.1 2002/12/04 22:41:55 gjv Exp $
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


% ---------------------------------------------------------------------
% grande test suite -- parses all examples of the present fragments
% with different output modules.
% ---------------------------------------------------------------------


% possible fragments
%
test_fragment(assoc).
test_fragment(hitchhiker).
test_fragment(preverbs2).
test_fragment(soprano).

% possible output modules
%
test_output_module(text).
test_output_module(prawitz_tex).
test_output_module(fitch_tex).
test_output_module(null).

% possible examples
%
test_example(String,Type) :-
	example(String,Type).



perform_test(Results) :-
	format("========================================================================~n",[]),

	test_fragment(Fragment),
	format("======== FRAGMENT: ~w~n",[Fragment]),
	load_fragment(Fragment),

	test_example(StringAsList,Type),
	atom_chars(String,StringAsList),
	format("======== EXAMPLE: ~w (~w)~n",[String,Type]),
	tokenize(StringAsList,Tokens),
	format("======== Parsing..............~n",[]),
	parseall(Tokens,Type,Results),
	format("SUCCEEDED --------------------------------------------------------------~n",[]),
	format("~n~n~n",[]).



perform_all_tests(ResultList) :-
	findall(R,perform_test(R),Results),
	flatten(Results,ResultList).


load_output_module_and_generate_output(Module,Results) :-
	format("======== OUTPUT MODULE: ~w~n",[Module]),
	load_output_module(Module),
	format("======== Generating Output............~n",[]),
	generate_output(Results).




:- ensure_loaded(grail).

:- perform_all_tests(Results),
	load_output_module_and_generate_output(prawitz_tex,Results).



:- halt.
