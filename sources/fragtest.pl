% ---------------------------------------------------------------------
% fragment test -- parse all examples of a fragment
% ---------------------------------------------------------------------


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

succeeded([],'FAILED').
succeeded([_|_],'SUCCEEDED').

perform_test(Fragment,Result) :-
    load_fragment(Fragment),
    test_output_module(Output_Module),
    load_output_module(Output_Module),
    test_example(StringAsList,Type),
    atom_chars(String,StringAsList),
    format("~nEXAMPLE: ~w (~w)~n",[String,Type]),
    tokenize(StringAsList,Tokens),
    format("Parsing..............~n",[]),
    parseall(Tokens,Type,Result),
    format("Generating Output....~n",[]),
    format("--------------------------------------------------------------~n",[]),
    once(generate_output(Result)),
    format("--------------------------------------------------------------~n",[]),
    format("~n",[]).

perform_test1(Result) :-
    format("~n",[]),
    test_example(StringAsList,Type),
    atom_chars(String,StringAsList),
    format("~w (~w)~n",[String,Type]),
    tokenize(StringAsList,Tokens),
    parseall(Tokens,Type,Result),
    succeeded(Result,Succeeded),
%    generate_output(Result),
    format("~w~n~n",[Succeeded]).

perform_all_tests(Fragment) :-
	findall(R,perform_test(Fragment,R),_).
	
perform_all_tests1(Fragment) :-
    load_fragment(Fragment),
    load_output_module(text),
	findall(R,perform_test1(R),_).

:- ensure_loaded(grail).

