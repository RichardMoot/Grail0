% ---------------------------------------------------------------------
% $Id: compatibility.pl,v 1.8 2002/12/02 15:19:50 xges Exp $
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

% -------------------------------------------------------
% Incompatible predicates
% -------------------------------------------------------

:- module(compatibility,
          [prolog_vendor/1,
           get_environment_variable/3,
           canonicalize_path/2]).

% Check for SICStus Prolog
assert_prolog_vendor :-
	retractall(prolog_vendor(_)),
	current_prolog_flag(version, VersionString),
	sub_atom(VersionString, 0, 7, _, 'SICStus'),
	!,
	assert(prolog_vendor(sics)).

% Default to SWI Prolog.
assert_prolog_vendor :-
	assert(prolog_vendor(swipl)).

init_for_vendor(sics) :-
	use_module(library(system),[environ/2]).

init_for_vendor(_).

get_environment_variable(VarName, Default, Result) :-
	(\+get_environment_variable(VarName, Default) ->
		Result=Default
	).

get_environment_variable(VarName, Result) :-
	prolog_vendor(sics),
	!,
	environ(VarName, Result).

get_environment_variable(VarName, Result) :-
	getenv(VarName, Result).
	
canonicalize_path(InPath, OutPath) :-
	prolog_vendor(sics),
	!,
	absolute_file_name(InPath, OutPath).

canonicalize_path(InPath, OutPath) :-
	expand_file_name(InPath, [OutPath|_]).

init_compatibility :-
	assert_prolog_vendor,
	prolog_vendor(Vendor),
	init_for_vendor(Vendor).

:- init_compatibility.
