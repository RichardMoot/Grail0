% ---------------------------------------------------------------------
% $Id: output_fitch_tex.pl,v 1.6 2002/12/04 12:06:43 vermaat Exp $
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
% Generates LaTeX files for Fitch-style natural deductions
%
% fNf Note: needs work.
%
% ---------------------------------------------------------------------

:- module(output_fitch_tex, []).

:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3]) ; true).

:- use_module([auxiliaries,common_tex,options]).      

% ---------------------------------------------------------------------
% generate_output(+Results)
%
% Generates Fitch/LaTeX output from parse results.
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
	nd_to_fitchlist(Proof,1,Max,FH,[],FL,[]),
    latex_nd_output(CurrentSolutionIndex,Meaning,Max,FL,FH,Con,Subst,NV),
   	told,
	tell(OldStream).


% ---------------------------------------------------------------------
% texfile_name -- a SICStus compatible eg?.tex name generator.
% NOTE: returns unexpected results when SolutionIndex > 9 !!
% ---------------------------------------------------------------------
texfile_name(FileName, SolutionIndex) :-
	AsciiNumber is SolutionIndex + 48,
	append("eg",[AsciiNumber],String1),
	append(String1,".tex",String2),
	name(FileName,String2).



% ---------------------------------------------------------------------
% generate_latex_wrapper(+NumberOfInputStatements)
%
% Generates the file 'proofs1.tex' containing NumberOfInputStatements
% \input statements for eg?.tex files.
% ---------------------------------------------------------------------
generate_latex_wrapper(NumberOfInputStatements) :-
	telling(Stream),
	tell('proofs1.tex'),
	format('\\documentclass[11pt]{article}~n',[]),
	format('\\usepackage{calc}~n',[]),
	format('\\usepackage{ifthen}~n',[]),
	format('\\usepackage{proof}~n',[]),
	format('\\usepackage{latexsym}~n',[]),
	format('\\usepackage{palatino}~n',[]),
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
	format('\\newcommand{\\Boxd}{\\Box^{\\downarrow}}~n',[]),
	format('\\newcommand{\\bs}{\\backslash}~n',[]),
	format('\\newcommand{\\bo}{[}~n',[]),
	format('\\newcommand{\\bc}{]}~n',[]),
	format('\\newcommand{\\ra}{\\rule{0pt}{7pt} \\Rightarrow}~n',[]),
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
	generate_input_statements(0,NumberOfInputStatements),
	format('\\end{document}~n',[]),
	format('~n',[]),
	told,
	tell(Stream).


% ---------------------------------------------------------------------
% generate_input_statements(+Current, +Total)
%
% create statements that include the eg?.tex files
% for generate_latex_wrapper/1
% ---------------------------------------------------------------------
generate_input_statements(Total, Total).
generate_input_statements(CurrentIncludeStatement, Total) :-
	NextIncludeStatement is CurrentIncludeStatement + 1,
	texfile_name(IncludeFile,NextIncludeStatement),
	format('%%~n',[]),
	format('\\input{~w}~n',[IncludeFile]),
	format('\\newpage~n',[]),
	generate_input_statements(NextIncludeStatement, Total).









% ---------------------------------------------------------------------
% Natural Deduction, Fitch style
%
% Original code, not refactored (yet).
% ---------------------------------------------------------------------

% =

latex_nd_output(N,Meaning,Max,RFL,FH,Con,Subst,NV) :-
    format('~n% ~w~n%~n~n\\begin{center}~n\\begin{tabular}{rll}~n',[Meaning]),
    reverse(RFL,[],FL),
    write_fitch(FL,[],Max,FH,Con,Subst,NV),
    format('~n\\end{tabular}~n\\end{center}~n\\vspace{4mm}~n{\\bf ~w. }\\mbox{$',[N]),
    write_sem(Meaning),
    format('$~n}~n~n',[]).

nd_to_fitchlist(rule(Name,A,S,Sem,R),N0,N,FH0,FH,[N0-frule(Name,A,S,Sem,Link)|L0],L) :-
       (Name=hyp(_) ->
        FH0=[N0-frule(Name,A,S,Sem,Link)|FH1]
       ;
        FH1=FH0
       ),
        N1 is N0+1,
        nds_to_fitchlist(R,N1,N,FH1,FH,L0,L,[],Link).

nds_to_fitchlist([],N,N,FH,FH,L,L,Ln,Ln).
nds_to_fitchlist([X|Xs],N0,N,FH0,FH,L0,L,Ln0,Ln) :-
        nds_to_fitchlist(Xs,N0,N1,FH0,FH1,L0,L1,[N1|Ln0],Ln),
        nd_to_fitchlist(X,N1,N,FH1,FH,L1,L).

write_fitch([],_,_,_,_,_,_).

write_fitch([N0-frule(Name,A,T,Sem,Rs0)|Rest],Ind0,Max,FH,Con,Subst,NV) :-
        N is Max-N0,
        format(' $ ~w. $ & $ ',[N]),
        update_indent(Name,Ind0,Ind),
        write_indent(Ind),
        write_label(A,'',':',Con),
        write_type(T),
        write_sem(Sem,'-','',Subst,NV),
        write(' $ & $'),
        write_fitch_rule_name(Name,Rs0,Max,FH),
        write('$ \\\\[-2.2ex]'),nl,
        write_fitch(Rest,Ind,Max,FH,Con,Subst,NV).

update_indent(hyp(N)   ,Ind,[N|Ind]) :- !.
update_indent(pe(_,N,M),Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind1),
        update_list(Ind1,M,Ind). 
update_indent(dli(_,N) ,Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind).
update_indent(dri(_,N) ,Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind).
update_indent(diae(_,N),Ind0,Ind) :- 
        !,
        update_list(Ind0,N,Ind).
update_indent(_        ,Ind ,Ind).

write_indent(N0) :-
       (hypo_scope(yes) ->
        reverse(N0,[],N)
       ;
        N=[]),
        write_indent1(N).

write_indent1([]) :- 
        !,
        write('\\rule{0ex}{4ex} ').
write_indent1([X|Xs]) :-
       (X=x -> N = 0 ; N = 3),
        format('\\rule[-1ex]{.1ex}{~wex} \\ ',[N]),
        write_indent1(Xs).

update_list([],_,[]).
update_list([N|Xs],N,Ys) :- 
        !,
        remove_xs(Xs,Ys).
update_list(Xs,N,Zs) :-
        update_list1(Xs,N,Ys),
        remove_xs(Ys,Zs).

update_list1([],_,[]).
update_list1([N|Rest],N,[x|Rest]) :-
        !.
update_list1([N|Rest0],M,[N|Rest]) :-
        update_list1(Rest0,M,Rest).

remove_xs([x|Xs],Ys) :-
        !,
        remove_xs(Xs,Ys).
remove_xs(Xs,Xs).

% =

write_fitch_rule_name(Rule,Rs0,Max,FH) :-
        rule_name(Rule,Name),
        format('~w \\ ',[Name]),
        rule_discharges(Rule,Nums),
        number_hypos(Nums,FH,Rs0,Rs),
        write_numbers(Rs,Max).

number_hypos([],_,Rs,Rs).
number_hypos([X|Xs],FH,Rs0,Rs) :-
        member_check(N-frule(hyp(X),_,_,_,_),FH),
        number_hypos(Xs,FH,[N|Rs0],Rs).

write_numbers(X,Max) :-
        sort(X,Y),
        reverse(Y,[],Z),
        write_numbers1(Z,Max).

write_numbers1([],_).
write_numbers1([X|Xs],Max) :-
        write(' ( '),
        write_numbers2(Xs,X,Max),
        write(' ) ').

write_numbers2([],X,Max) :-
        Y is Max-X,
        print(Y).
write_numbers2([Y|Ys],X,Max) :-
        Z is Max-X,
        format('~w,',[Z]),
        write_numbers2(Ys,Y,Max).


rule_list([],N,N) :- !.
rule_list([R|Rs],R0,(N0,N)) :-
        rule_name(R0,N0),
        rule_list(Rs,R,N).

text_output(Sem,Pros) :-
       (output_text_sem(yes) ->
        format('~N~n%~n% ~w~n%~n~n',[Sem]),
        write_list_of_labels(Pros),
        write_sem(Sem)
       ;
        true).

write_list_of_labels([L|Ls]) :-
        format('~N\\vspace{2mm}~n\\begin{tabular}{l}~n',[]),
        write_list_of_labels1(Ls,L),
        format('~N\\end{tabular}~n~n\\vspace{2mm}~n~n',[]).

write_list_of_labels1([],Label) :-
        write(' $ '),
        write_label(Label,1,[]),
        write(' $ ').

write_list_of_labels1([Label|Labels],Label0) :-
        write(' $ '),
        write_label(Label0,1,[]),
        write(' $ \\\\ '),nl,
        write_list_of_labels1(Labels,Label).


% =

write_list_of_vars([]) :- 
        write('$ \\ $').
write_list_of_vars([V|Vs]) :-
        write_list_of_vars(Vs,V).

write_list_of_vars([],V) :-
        write('$'),
        write_sem(V),
        write('$').
write_list_of_vars([V|Vs],V0) :-
        write('$'),
        write_sem(V0),
        write('\\ $'),
        write_list_of_vars(Vs,V).

% =

write_list_of_conds([]) :-
        write('$ \\ $ \\\\ \\hline ').
write_list_of_conds([C|Cs]) :-
        write_list_of_conds(Cs,C).

write_list_of_conds([],C) :-
        write('$'),
        write_sem(C),
        format('$ \\\\ \\hline ',[]).

write_list_of_conds([C|Cs],C0) :-
        write('$'),
        write_sem(C0),
        format('$ \\\\~n ',[]),
        write_list_of_conds(Cs,C).




