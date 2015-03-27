% ---------------------------------------------------------------------
% $Id: grandetest.pl,v 1.3 2002/12/04 21:25:48 vermaat Exp $
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
test_fragment(gqd4a).
%test_fragment(hitchhiker).
%test_fragment(preverbs2).
%test_fragment(soprano).

% possible output modules
%
test_output_module(text).
%test_output_module(prawitz_tex).
%test_output_module(fitch_tex).
%test_output_module(null).

% possible examples
%
test_example(String,Type) :-
	example(String,Type).



perform_test(test_result(Fragment,Output_Module,String,Type)) :-
	format("========================================================================~n",[]),

	test_fragment(Fragment),
	format("======== FRAGMENT: ~w~n",[Fragment]),
	load_fragment(Fragment),

	test_output_module(Output_Module),
	format("======== OUTPUT MODULE: ~w~n",[Output_Module]),
	load_output_module(Output_Module),

	test_example(StringAsList,Type),
	atom_chars(String,StringAsList),
	format("======== EXAMPLE: ~w (~w)~n",[String,Type]),
	tokenize(StringAsList,Tokens),
	format("======== Parsing..............~n",[]),
	parseall(Tokens,Type,Result),
	format("======== Generating Output....~n",[]),
	once(generate_output(Result)),
	format("SUCCEEDED --------------------------------------------------------------~n",[]),
	format("~n~n~n",[]).

perform_all_tests :-
	findall(R,perform_test(R),_).

:- ensure_loaded(grail).

:- perform_all_tests.

:- halt.
