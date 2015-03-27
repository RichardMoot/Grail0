% ---------------------------------------------------------------------
% $Id: options.pl,v 1.9 2002/12/02 23:22:51 vermaat Exp $
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


:- module(options,
		  [unary_semantics/1,
		  latex_output_format/1,
		  eta_long_proofs/1,
		  hypo_scope/1,
		  ignore_brackets/1,
		  compact_lex/1,
		  macro_reduce/1,
		  output_expl_brackets/1,
		  output_labels/1,
		  output_semantics/1,
		  output_subst_lex_sem/1,
		  output_reduced_sem/1,
		  output_text_sem/1,
		  output_sr/1,
		  collapse_sr/2,
		  boring_rule/1,
		  logical_rule/1]).


% ---------------------------------------------------------------------
% Options
% ---------------------------------------------------------------------

%%% MM: allows for compact typesetting of lexical assumptions
%%% cf output_prawitz_tex

compact_lex(yes).

unary_semantics(inactive).

% fNf: OBSOLETE -- value of latex_output_format/1 is unused.
% latex_output_format(?Format)
% Format can be one of the atoms 'fitch','nd' or 'none'. This 
% version does not support sequent output

latex_output_format(nd).



% eta_long_proofs(?Flag)
% Setting this flag to 'yes' will cause the eta reduction of the
% natural deduction proof to be skipped.

eta_long_proofs(no).




% hypo_scope(?Flag)
% This flag determines whether to indicate the scope of hypotheses
% in a fitch style natural deduction proof by a vertical bar

hypo_scope(yes).



% ignore_brackets(?Mode)
% Often, too many brackets will make the output unreadable. When
% you are not interested in certain brackets (because they are
% associative, for example, and more readable in list-like notation) 
% use this declaration. This will not remove associativity inferences 
% from the derivation.

ignore_brackets('$MODE'). % never remove this clause
%ignore_brackets(0).
%ignore_brackets(a).




% macro_reduce(?Flag)
% If this flag is set to value 'yes', the macro definitions will
% be applied in order to reduce every formula as far as possible

macro_reduce(no).




% output_expl_brackets(?Flag)
% Setting this flag to 'no' will not output brackets when the
% precedence of the operators allows this.
% For example, this will produce A*B/C instead of (A*B)/C.

output_expl_brackets(yes).




% output_labels(?Flag)
% If this flag is set to value 'yes', the structure label corresponding 
% to each part of the proof will be displayed.

output_labels(yes).




% output_semantics(?Flag)
% If this flag is set to value 'yes', the lambda term corresponding
% to each part of the proof will be displayed.

output_semantics(no).




% output_subst_lex_sem(?Flag)
% If this flag is set to value 'yes', the lambda term meaning recipies
% found in the lexicon will be substituted for the semantic variables
% normally assigned to lexical assumptions.

output_subst_lex_sem(yes).




% output_reduced_sem(?Flag)
% Will display only beta/eta-normal forms of lambda terms when set 
% to 'yes'.

output_reduced_sem(yes).

output_text_sem(yes).




% output_sr(?Flag)
% If this flag is set to 'yes', all structural rules will be displayed
% explicitly. If you are only interested in the logical aspects of the
% proof, set this flag to 'no'.

output_sr(yes).




% collapse_sr(?ListOfNames,?Name)
% sequences (>1) of structural (= non-logical, see logical_rule/1) 
% rules which are all in ListOfNames are abbreviated by a single
% structural rule step labeled Name.
% Examples:
% collapse_sr(['Ass'],'Ass*'). Will print a sequence of 'Ass' inferences
%                              as a single 'Ass*' step.
% collapse_sr(_,'SR').         Will replace any sequence of structural
%                              rules by a single rule named 'SR'.
% collapse_sr(X,X).            Will replace any sequence of structural
%                              rules by the _multiset_ of rules used.

%collapse_sr(X,X).
collapse_sr('dummy argument','dummy arguments').




% boring_rule(?RuleName)
% sequences of rules declared as boring (by rule name) are
% abbreviated by writing a series of dots as premiss of the 
% last 'interesting' rule. Does not make sense when
% latex_output_format is set to 'fitch'

boring_rule(ax).
%boring_rule(rdl(_)).
%boring_rule(ldl(_)).
%boring_rule(rdr(_)).
%boring_rule(ldr(_)).
%boring_rule(rp(_)).
%boring_rule(lp(_)).
%boring_rule(rdia(_)).
%boring_rule(ldia(_)).
%boring_rule(rbox(_)).
%boring_rule(lbox(_)).

boring_rule(lex).
%boring_rule(hyp(_)).
%boring_rule(uhyp).
%boring_rule(dri(_)).
%boring_rule(dre(_)).
%boring_rule(dli(_,_)).
%boring_rule(dle(_,_)).
%boring_rule(pi(_)).
%boring_rule(pe(_,_,_)).
%boring_rule(boxi(_)).
%boring_rule(boxe(_)).
%boring_rule(diai(_)).
%boring_rule(diae(_,_)).





% logical_rule(?RuleName)
% identifies the names of the logical rules.

logical_rule(lex).
logical_rule(hyp(_)).
logical_rule(uhyp).
logical_rule(dri(_,_)).
logical_rule(dre(_)).
logical_rule(dli(_,_)).
logical_rule(dle(_)).
logical_rule(pi(_)).
logical_rule(pe(_,_,_)).
logical_rule(boxi(_)).
logical_rule(boxe(_)).
logical_rule(diai(_)).
logical_rule(diae(_,_)).
