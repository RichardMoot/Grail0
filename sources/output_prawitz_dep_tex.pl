% ---------------------------------------------------------------------
% $Id: output_prawitz_tex.pl,v 1.13 2002/12/04 23:50:34 gjv Exp $
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
%
% Generates LaTeX files for Prawitz natural deductions
%
% ---------------------------------------------------------------------

:- module(output_prawitz_dep_tex, []).
           
:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3]) ; true).

:- use_module([auxiliaries,reduce_sem,options,mm_common_tex]).

% ---------------------------------------------------------------------
% generate_output(+Results)
%
% Generates Prawitz/LaTeX output from parse results.
% ---------------------------------------------------------------------

generate_output([]).
generate_output(Results) :-
	length(Results,N),
	generate_latex_wrapper(N),                 % create proofs1.tex
	latex_output_structures(Results, 1).       % create eg?.tex files

% ---------------------------------------------------------------------
% latex_output_structures(+ListOfResultTerms)
%
% writes a tex file containing the ND for every ResultTerm in the list.
% ---------------------------------------------------------------------

latex_output_structures([], _).
latex_output_structures([result(Meaning, _, _, ND,Con,Subst,NVAR)|OutputStructures], CurrentResult) :-
	latex_output(CurrentResult,Meaning,ND,Con,Subst,NVAR),
	NextResult is CurrentResult + 1,
	latex_output_structures(OutputStructures, NextResult).

% ---------------------------------------------------------------------
% latex_output(+CurrentSolutionIndex,+Meaning,+Proof,+Con,+Subst,+NVAR)
%
% Generates egn.tex, where n = CurrentSolutionIndex, containing
% a prawitz-style ND.
% ---------------------------------------------------------------------

latex_output(CurrentSolutionIndex,Meaning,Proof,Con,Subst,NV) :-
	telling(OldStream),
	texfile_name(NewStream, CurrentSolutionIndex),
	tell(NewStream),
    latex_nd_output(CurrentSolutionIndex,Meaning,Proof,Con,Subst,NV),
   	told,
	tell(OldStream).


% ---------------------------------------------------------------------
% texfile_name -- a SICStus compatible eg?.tex name generator.
% NOTE: returns unexpected results when SolutionIndex > 9 !!
% ---------------------------------------------------------------------
texfile_name(FileName, SolutionIndex) :-
	atom_codes(eg, EgCodes),
	number_codes(SolutionIndex, IndexCodes),
	append(EgCodes, IndexCodes, AtomCodes), atom_codes(ResultWithoutExtension, AtomCodes),
	atom_concat(ResultWithoutExtension,'.tex',FileName).



% ---------------------------------------------------------------------
% generate_latex_wrapper(+NumberOfIncludeStatements)
%
% Generates the file 'proofs1.tex' containing NumberOfIncludeStatements
% \input statements for eg?.tex files.
% ---------------------------------------------------------------------
generate_latex_wrapper(NumberOfIncludeStatements) :-
	telling(Stream),
	tell('proofs1.tex'),
	format('\\documentclass[11pt]{article}~n',[]),
	format('\\usepackage{calc}~n',[]),
	format('\\usepackage{ifthen}~n',[]),
	format('\\usepackage{proof}~n',[]),
	format('\\usepackage{latexsym}~n',[]),
	format('\\usepackage{wasysym}~n',[]),
	format('\\usepackage{color}~n',[]),
	format('\\definecolor{Bgrey}{gray}{.80}~n',[]),
	format('~n',[]),
	format('\\RequirePackage[pdftex,pagebackref,pdfpagemode=none,colorlinks,%~n',[]),
	format('             pdfmenubar=false,%~n',[]),
	format('             pdftoolbar=false,%~n',[]),
	format('             pdffitwindow=true,pdfcenterwindow=true,%~n',[]),
	format('             pdfwindowui=false,menucolor=menucolor,%~n',[]),
	format('             pdfview=Fit,pdfstartview=Fit]{hyperref}~n',[]),
	format('~n',[]),
	format('\\pagestyle{empty}~n',[]),
	format('~n',[]),
	format('\\newcommand{\\lcirc}{\\,\\textcolor{red}{\\LEFTcircle}\\,}~n',[]),
    format('\\newcommand{\\rcirc}{\\,\\textcolor{red}{\\RIGHTcircle}\\,}~n',[]),
	format('\\newcommand{\\lbullet}{\\,\\LEFTcircle\\,}~n',[]),
    format('\\newcommand{\\rbullet}{\\,\\RIGHTcircle\\,}~n',[]),
    format('\\newcommand{\\rhead}{\,\\mbox{$-\\!\\bullet$}\\,}~n',[]),
    format('\\newcommand{\\rdep}{\\,\\mbox{$-\\!\\circ$}\\,}~n',[]),
    format('\\newcommand{\\lhead}{\\,\\mbox{$\\bullet\\!-$}\\,}~n',[]),
    format('\\newcommand{\\ldep}{\\,\\mbox{$\\circ\\!-$}\\,}~n',[]),
	format('\\newcommand{\\Boxd}{\\Box}~n',[]),
	format('\\newcommand{\\bs}{\\backslash}~n',[]),
	format('\\newcommand{\\bo}{[}~n',[]),
	format('\\newcommand{\\bc}{]}~n',[]),
	format('\\newcommand{\\ra}{\\rule{0pt}{7pt} \\Rightarrow}~n',[]),
	format('~n',[]),
	format('\\newlength{\\wdtotal}~n',[]),
	format('\\newlength{\\httotal}~n',[]),
	format('\\setlength{\\wdtotal}{0cm}~n',[]),
	format('\\setlength{\\httotal}{0cm}~n',[]),
	format('~n',[]),
	generate_saveboxes(0,NumberOfIncludeStatements),
	format('~n',[]),
	format('\\setlength{\\paperwidth}{1in+\\wdtotal}~n',[]),
	format('\\setlength{\\paperheight}{2in+\\httotal}~n',[]),
	format('\\textwidth = \\wdtotal~n',[]),
	format('\\textheight = \\httotal~n',[]),
	format('~n',[]),
	format('\\oddsidemargin = 0.0 in~n',[]),
	format('\\evensidemargin = 0.0 in~n',[]),
	format('\\topmargin = 0.0 in~n',[]),
	format('\\headheight = 0.0 in~n',[]),
	format('\\headsep = 0.0 in~n',[]),
	format('\\parskip = 0.2in~n',[]),
	format('\\parindent = 0.0in~n',[]),
	format(' ~n',[]),
	format('\\begin{document}~n',[]),
	generate_useboxes(0,NumberOfIncludeStatements),
	format('\\end{document}~n',[]),
	format('~n',[]),
	told,
	tell(Stream).


% ---------------------------------------------------------------------
% generate_saveboxes(+Current, +Total)
%
% create saveboxes that include the eg?.tex files
% for generate_latex_wrapper/1
% ---------------------------------------------------------------------
generate_saveboxes(Total, Total).
generate_saveboxes(CurrentIncludeStatement, Total) :-
	NextIncludeStatement is CurrentIncludeStatement + 1,
	generate_proofboxname(NextIncludeStatement,ProofBox),
	texfile_name(IncludeFile,NextIncludeStatement),
	format('%%~n',[]),
	format('% box for ~w~n',[IncludeFile]),
	format('%%~n',[]),
	format('\\newsavebox{\\~w}~n',[ProofBox]),
	format('\\sbox{\\~w}{\\input{~w}}~n',[ProofBox,IncludeFile]),
	format('\\ifthenelse{\\wd\\~w > \\wdtotal}{\\setlength{\\wdtotal}{\\wd\\~w}}{}~n',[ProofBox,ProofBox]),
	format('\\ifthenelse{\\ht\\~w > \\httotal}{\\setlength{\\httotal}{\\ht\\~w}}{}~n',[ProofBox,ProofBox]),
	format('~n',[]),
	generate_saveboxes(NextIncludeStatement, Total).

% ---------------------------------------------------------------------
% generate_useboxes(+Current, +Total)
%
% create useboxe statements for generate_latex_wrapper/1
% If there is only one input file, only one \usebox statement is
% generated. Multiple \useboxes are separated by a \newpage statement.
% ---------------------------------------------------------------------
generate_useboxes(Total, Total).
generate_useboxes(CurrentIncludeStatement, Total) :-
	NextIncludeStatement is CurrentIncludeStatement + 1,
	generate_proofboxname(NextIncludeStatement,ProofBox),
	format('\\usebox{\\~w}~n',[ProofBox]),
	( CurrentIncludeStatement is Total - 1 -> true ; format('\\newpage~n',[]) ),
	generate_useboxes(NextIncludeStatement, Total).


% ---------------------------------------------------------------------
% basically, a base10 to base26 converter...
% ---------------------------------------------------------------------
generate_lettercodes(M,Result) :-
	M =< 26,
	ResultCode is M + 96,
	atom_codes(Result,[ResultCode]).

generate_lettercodes(M,Result) :-
	S is M mod 26,
	M1 is M // 26,
	generate_lettercodes(M1,Result0),
	Result1Code is S + 96,
	atom_codes(Result1,[Result1Code]),
	atom_concat(Result1,Result0,Result).

generate_proofboxname(Number,Name) :-
	generate_lettercodes(Number,Letters),
	atom_concat('proofbox',Letters,Name).







% ---------------------------------------------------------------------
% Natural Deduction, Prawitz style
%
% Code originally in "latex.pl". Not refactored (yet).
% ---------------------------------------------------------------------

% =



%latex_nd_output(N,Meaning,rule(Name,A,S,Sem,Rs),Con,Subst,NV) :-
%        format('~n%~n% ~w~n%~n~n{\\samepage~n~n\\ensuremath{~n',[Meaning]),
%        write_nd(Rs,Name,A,S,Sem,Con,Subst,NV,0),
%        format('}~n~n\\vspace{4mm}~n~n',[]),
%        format('{\\bf ~w. }\\mbox{$',[N]),
%        write_sem(Meaning),
%        format('$}~n}~n~n',[]).

%%% MM: semantics *under* the ND derivation 

latex_nd_output(N,Meaning,rule(Name,A,S,Sem,Rs),Con,Subst,NV) :-
        format('~n%~n% ~w~n%~n~n{\\samepage~n~n\\ensuremath{~n\\deduce{',[Meaning]),
        write_sem(Meaning),
        format('}{\\deduce{$\\rule{0pt}{2em}$}{',[]),
        write_nd(Rs,Name,A,S,Sem,Con,Subst,NV,0),
        format('}}}}~n~n',[]).
        
%write_nd([],Name,A,S,Sem,Con,Subst,NV,Tab0) :-
%        Tab is Tab0 + 3,
%        format('~N~*|',[Tab]),
%       (Name=hyp(_) ->
%        write(' \\bo ')
%       ;
%        true),
%        write_label(A,'',' \\vdash ',Con),
%        write_sem(Sem,'',' : ',Subst,NV),
%        write_type(S),
%       (Name=hyp(N) ->
%        format(' \\bc^{~w} ',[N])
%       ;
%        true).

%%% MM: compact representation of lexical assumptions
%%% requires compact_lex/1 option

write_nd([],Name,A,S,Sem,Con,Subst,NV,Tab0) :-
        Tab is Tab0 + 3,
        format('~n~*|',[Tab]),
       ( Name=lex,
         compact_lex(yes) ->
	 write('\\infer{'),
	 write_sem(Sem,'',' : ',Subst,NV),
	 write_type(S),
	 write('}{'),
	 write_label(A,'','',Con),
	 write('}')
       ;
       (Name=hyp(_), 
        hypo_scope(yes) ->
        write(' \\bo ')
       ;
        true),
        write_label(A,'',' \\vdash ',Con),
        write_sem(Sem,'',' : ',Subst,NV),
        write_type(S),
       (Name=hyp(N),
        hypo_scope(yes) ->
        format(' \\bc^{~p} ',[N])
       ;
        true)
       ).

write_nd([R|Rs],N,A,S,Sem,Con,Subst,NV,Tab0) :-
        Tab is Tab0 + 3,
       (boring(N,[R|Rs]) ->
        format('~N~*|\\infer{',[Tab]),
        write_label(A,'',' \\vdash ',Con),
        write_sem(Sem,'',' : ',Subst,NV),
        write_type(S),
        write('}{ \\cdots }')
       ;
        format('~N~*|\\infer[',[Tab]),
        write_nd_rule_name(N),
        write(']{'),
        write_label(A,'',' \\vdash ',Con),
        write_sem(Sem,'',' : ',Subst,NV),
        write_type(S),
        write('}{'),
        write_nds(Rs,R,Con,Subst,NV,Tab),
        format('~N~*|}',[Tab])
       ).

% write_nds(+ListOfNDs)
% write a list of NDs

write_nds([],rule(N,A,S,Sem,Rs0),Con,Subst,NV,Tab) :-
        write_nd(Rs0,N,A,S,Sem,Con,Subst,NV,Tab).
write_nds([R|Rs],rule(N,A,S,Sem,Rs0),Con,Subst,NV,Tab) :-
        write_nd(Rs0,N,A,S,Sem,Con,Subst,NV,Tab),
        format('~N~*|& ',[Tab]),
        write_nds(Rs,R,Con,Subst,NV,Tab).

% =

write_nd_rule_name(Rule) :-
        rule_name(Rule,Name),
        format('\\bo ~w \\bc^{',[Name]),
        rule_discharges(Rule,Xs),
        write_indices(Xs),
        write('}').


write_indices([]).
write_indices([Y|Ys]) :-
        sort([Y|Ys],[Z|Zs]),
        write_indices1(Zs,Z).

write_indices1([],Z) :- 
        print(Z).
write_indices1([X|Xs],Z) :-
        format('~w,',[Z]),
        write_indices1(Xs,X).



% =

boring(N,[R|Rs]) :-
     rule_names([R|Rs],Ns,[]),
     boring([N|Ns]).

boring([]).
boring([N|Ns]) :-
     boring_rule(N),
     boring(Ns).


% =

rule_names([],N,N).
rule_names([rule(Name,_,_,_,Rs1)|Rs],[Name|N0],N) :-
     rule_names(Rs1,N0,N1),
     rule_names(Rs,N1,N).


% ===========================================================
