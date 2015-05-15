% ---------------------------------------------------------------------
% $Id: graphs.pl,v 1.7 2002/12/02 23:22:50 vermaat Exp $
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


:- module(graphs,
          [parse_syn/3,
           parse_words/3]).
           
:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3,select/3]) ; true).

:- use_module([fragments,statistics,auxiliaries,reduce_sem,nd]).

parse_syn(ListOfSyn, Goal, result(Meaning, Label, NormalizedLabel, ND, Constants, [], 0)) :-
	parse(ListOfSyn, Goal, result(Meaning, Label, NormalizedLabel, ND, Constants, [], _)).

parse_words(ListOfWords, Goal, Result) :-
	  parse(ListOfWords, Goal, Result).




% ---------------------------------------------------------------------
% parse(+ListOfWordsOrSyn, +GoalType, ResultTerm)
%
% Parses ListOfWordsOrSyn as goal type GoalType; ResultTerm is the data
% structure containing the ND.
% ---------------------------------------------------------------------

parse(ListOfWordsOrSyn, Goal, result(Meaning, Label, NormalizedLabel, ND, Constants, Subst, NVAR)) :-
	init_parser(ListOfWordsOrSyn, Goal, Meaning0, Label1, [E|Es], [], TwoRules0, Hyp, Constants0, Subst, NVAR),
	/* copy the essential graph structure, the first one is for eager reductions */
	/* the second for proof reconstruction */
	copy_term(t([E|Es], Label1, TwoRules0), t([E0|Es0], Label, TwoRules)),
	prove(Es, E, Es0, E0),
	/* first try normalizing the partially reduced label, try reconstructing a proof */
	/* only when this succeeds */
	normalize(Label1, NormalizedLabel),
	substitute_sem(Subst, Meaning0, Meaning1, NVAR, _),
	reduce_sem(Meaning1, Meaning),
	normalize(Label, NormalizedLabel, OneRules, []),
	first_nd(Hyp, OneRules, TwoRules, Constants0, Constants, ND0),
	eta_reduce(ND0, ND1),
	collapse(ND1, ND),
	write_statistics.


% ---------------------------------------------------------------------
% init_parser(ListOfWords, Goal, Semantics, Label,
%         Edges0, Edges, Rules, Hyp, Con, Subst, NVAR)
%
% ---------------------------------------------------------------------

init_parser(ListOfWords, Goal, Semantics, Label, [vertex(0,Bs,Ps)|Edges0], Edges, Rules0, Hyp0, Con0,
            Subst, NVAR) :-
     initial_graph(ListOfWords, 0, M, 1, N, Rules0, Rules1, Hyp0, Hyp1, Con0, Con1, Edges0, Edges1,
                   Subst, []),
     macro_expand(Goal, G),
     link0(G, Semantics, Label, 0, M, N, NVAR, Rules1, [], Hyp1, [], Con1, [], Bs, [], Ps, [], Edges1, Edges).


% ---------------------------------------------------------------------
% initial_graph(ListOfWords, M0, M, N0, N, Rules0, Rules, Hyp0, Hyp , Con0, Con, Edges0, Edges,
%               Subst0, Subst)
%
% ---------------------------------------------------------------------

initial_graph([],M,M,N,N,R,R,H,H,C,C,Es,Es,List, List).

initial_graph([W|Ws],M0,M,N0,N,R0,R,[rule(lex,M0-W,Syn,'$VAR'(N0),[])|H0],H,C0,C,[vertex(N0,As,Ps)|Es0],Es,
              [N0-Sem|S0],S) :-
     catch_fail(lex(W,Syn0,Sem),'Lexical lookup failed: ',W),
     macro_expand(Syn0,Syn),
     M1 is M0+1,
     N1 is N0+1,
     link1(Syn,'$VAR'(N0),M0-W,M0,M1,N1,N2,R0,R1,H0,H1,C0,C1,As,[],Ps,[],Es0,Es1),
     initial_graph(Ws,M1,M,N2,N,R1,R,H1,H,C1,C,Es1,Es,S0,S).

initial_graph([X|Xs],M0,M,N0,N,R0,R,[rule(uhyp,M0-'$VAR'(M0),Syn,'$VAR'(M0),[])|H0],H,C0,C,[vertex(N0,As,Ps)|Es0],Es,
              [],[]) :-
     macro_expand(X,Syn),
     M1 is M0+1,
     N1 is N0+1,
     link1(Syn,'$VAR'(M0),M0-'$VAR'(M0),M0,M1,N1,N2,R0,R1,H0,H1,C0,C1,As,[],Ps,[],Es0,Es1),
     initial_graph(Xs,M1,M,N2,N,R1,R,H1,H,C1,C,Es1,Es,[],[]).


catch_fail(Goal,Message,Indicator) :-
   (
      \+ Goal
     ->
      write(Message),
      write(Indicator),
      nl,
      fail
    ;
      Goal
    ).


% ---------------------------------------------------------------------
% fNf: code below is still uncharted territory.
% ---------------------------------------------------------------------

% = antecedent types
	
link1(lit(A),V,S,Pos0,Pos1,N,N,Rules,Rules,H,H,
      Con,Con,[neg(A,V,S,Pos0,Pos1)|As],As,Ps,Ps,Es,Es).

link1(dia(J,X),V,R,Pos0,Pos1,N0,N,Rules0,Rules,
      [rule(hyp(_),unzip(J,R),X,'$VAR'(N0),[])|H0],H,
      [cf(unzip(J,R),dedia(V),'$VAR'(N0))|Con0],Con,
      As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     N1 is N0+1,
     link1(X,dedia(V),unzip(J,R),Pos2,Pos3,N1,N,Rules0,Rules,
           H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link1(box(J,X),V,R,Pos0,Pos1,N0,N,[box(J,zip(J,R)-X)|Rules0],Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     link1(X,debox(V),zip(J,R),Pos2,Pos3,N0,N,Rules0,Rules,
           H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link1(dr(J,X,Y),U,R,Pos0,Pos1,N0,N,[dr(J,p(J,R,S)-X,S-Y)|Rules0],Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=Pos5 ; true),
     link0(Y,V,S,Pos3,Pos4,N0,N1,Rules0,Rules1,
           H0,H1,Con0,Con1,As0,As1,Ps0,Ps1,Es0,Es1),
     link1(X,appl(U,V),p(J,R,S),Pos2,Pos5,N1,N,Rules1,Rules,
           H1,H,Con1,Con,As1,As,Ps1,Ps,Es1,Es). 

link1(dl(J,Y,X),U,R,Pos0,Pos1,N0,N,[dl(J,S-Y,p(J,S,R)-X)|Rules0],Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=Pos5 ; true),
     link0(Y,V,S,Pos5,Pos2,N0,N1,Rules0,Rules1,
           H0,H1,Con0,Con1,As0,As1,Ps0,Ps1,Es0,Es1), 
     link1(X,appl(U,V),p(J,S,R),Pos4,Pos3,N1,N,
           Rules1,Rules,H1,H,Con1,Con,As1,As,Ps1,Ps,Es1,Es).

link1(p(J,X,Y),V,R,Pos0,Pos1,N0,N,Rules0,Rules,
      [rule(hyp(_),l(J,R),X,'$VAR'(N0),[]),
       rule(hyp(_),r(J,R),Y,'$VAR'(N1),[])|H0],H,
      [cf(l(J,R),fst(V),'$VAR'(N0)),cf(r(J,R),snd(V),'$VAR'(N1))|Con0],Con,
      As,As,[N0-N1|Ps],Ps,
      [vertex(N0,Bs,Qs),vertex(N1,Cs,Rs)|Es0],Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=c(N0),Pos5=c(N0) ; true),
     N1 is N0+1,
     N2 is N1+1,
     link1(X,fst(V),l(J,R),Pos2,Pos4,N2,N3,
           Rules0,Rules1,H0,H1,Con0,Con1,Bs,[],Qs,[],Es0,Es1),
     link1(Y,snd(V),r(J,R),Pos5,Pos3,N3,N,
           Rules1,Rules,H1,H,Con1,Con,Cs,[],Rs,[],Es1,Es).
	
% = succedent types

link0(lit(A),V,S,Pos0,Pos1,N,N,Rules,Rules,H,H,
      Con,Con,[pos(A,V,S,Pos0,Pos1)|As],As,Ps,Ps,Es,Es).

link0(dia(J,X),condia(V),zip(J,R),Pos0,Pos1,N0,N,
      [dia(J,R-X)|Rules0],Rules,H0,H,Con0,Con,
      As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     link0(X,V,R,Pos2,Pos3,N0,N,Rules0,Rules,H0,H,
           Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link0(box(J,X),conbox(V),unpack(J,R),Pos0,Pos1,N0,N,Rules0,Rules,
      H0,H,Con0,Con,As0,As,Ps0,Ps,Es0,Es) :-
    (continuous_dia(J) -> Pos2=Pos0,Pos3=Pos1 ; true),
     link0(X,V,R,Pos2,Pos3,N0,N,Rules0,Rules,H0,H,
           Con0,Con,As0,As,Ps0,Ps,Es0,Es).
	
link0(dr(J,X,Y),lambda('$VAR'(N0),V),dr(J,R,x-'$VAR'(N0)),Pos0,Pos1,N0,N,
      Rules0,Rules,[rule(hyp(_),x-'$VAR'(N0),Y,'$VAR'(N0),[])|H0],H,Con0,Con,
      As,As,[N0-N1|Ps],Ps,
      [vertex(N0,Bs,Qs),vertex(N1,Cs,Rs)|Es0],Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=c(N0),Pos5=c(N0) ; true),
     N1 is N0+1,  
     N2 is N1+1,
     link1(Y,'$VAR'(N0),x-'$VAR'(N0),Pos3,Pos4,N2,N3,
           Rules0,Rules1,H0,H1,Con0,Con1,Bs,[],Qs,[],Es0,Es1),
     link0(X,V,R,Pos2,Pos5,N3,N,Rules1,Rules,
           H1,H,Con1,Con,Cs,[],Rs,[],Es1,Es).

link0(dl(J,Y,X),lambda('$VAR'(N0),V),dl(J,x-'$VAR'(N0),R),Pos0,Pos1,N0,N,
      Rules0,Rules,[rule(hyp(_),x-'$VAR'(N0),Y,'$VAR'(N0),[])|H0],H,Con0,Con,
      As,As,[N0-N1|Ps],Ps,
      [vertex(N0,Bs,Qs),vertex(N1,Cs,Rs)|Es0],Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=c(N0),Pos5=c(N0) ; true),
     N1 is N0+1,
     N2 is N1+1,  
     link0(X,V,R,Pos5,Pos3,N2,N3,Rules0,Rules1,
           H0,H1,Con0,Con1,Bs,[],Qs,[],Es0,Es1), 
     link1(Y,'$VAR'(N0),x-'$VAR'(N0),Pos4,Pos2,N3,N,
           Rules1,Rules,H1,H,Con1,Con,Cs,[],Rs,[],Es1,Es).
	
link0(p(J,X,Y),pair(U,V),p(J,R,S),Pos0,Pos1,N0,N,
      [p(J,R-X,S-Y)|Rules0],Rules,H0,H,Con0,Con,
      As0,As,Ps0,Ps,Es0,Es) :-
    (continuous(J) -> Pos2=Pos0,Pos3=Pos1,Pos4=Pos5 ; true),
     link0(Y,V,S,Pos5,Pos3,N0,N1,Rules0,Rules1,
           H0,H1,Con0,Con1,As0,As1,Ps0,Ps1,Es0,Es1),
     link0(X,U,R,Pos2,Pos4,N1,N,Rules1,Rules,
           H1,H,Con1,Con,As1,As,Ps1,Ps,Es1,Es).

% ============================================================
% Resolution
% ============================================================

prove([],vertex(_,[],[]),_,_).

prove([G0|Gs0],G,[H0|Hs0],H) :-
       select_atom(neg(A,V,S0,P0,P1),vertex(N,As,Ps),
                   neg(A,W,S1,P0,P1),vertex(N,A1s,Ps),
                   [G,G0|Gs0],Gs1,[H,H0|Hs0],Hs1),
       select_conj(neg(A,V,S,P0,P1),vertex(M,Bs,Qs),
                   neg(A,W,S1,P0,P1),vertex(M,B1s,Qs),
                   Gs1,Gs2,Hs1,Hs2),
       \+ cyclic(vertex(N,As,Ps),vertex(M,Bs,Qs),Gs2),
       merge_vertices(vertex(N,As,Ps),vertex(M,Bs,Qs),Gs2,Gs3),
       merge_vertices(vertex(N,A1s,Ps),vertex(M,B1s,Qs),Hs2,Hs3),
       reduce_graph(Gs3,[G4|Gs4]),
       reduce_graph(Hs3,[H4|Hs4]),
       update_statistics('acc links'),
       remove_divisions(S0,S),
       update_statistics('lab links'),
       prove(Gs4,G4,Hs4,H4).

select_atom(neg(B,V,S,P0,P1),vertex(N,[],[]),
            neg(B,W,S1,P0,P1),vertex(N,[],[]),G0,G,H0,H) :-
       duo_select(vertex(N,[neg(B,V,S,P0,P1)],[]),
                  vertex(N,[neg(B,W,S1,P0,P1)],[]),G0,G,H0,H),
       ground(S),
       !.

select_conj(neg(B,V,S,P0,P1),vertex(N,As,Ps),
            neg(B,W,S1,P0,P1),vertex(N,Cs,Ps),G0,G,H0,H) :-
       duo_select(vertex(N,[A|As0],Ps),
                  vertex(N,[C|Cs0],Ps),G0,G,H0,H),
       duo_select(pos(B,V,S,P2,P3),
                  pos(B,W,S1,P2,P3),[A|As0],As,[C|Cs0],Cs),
       update_statistics('total links'),
       P0=P2,
       P1=P3,
       update_statistics('plan links').

% ============================================================
% Graph Reductions
% ============================================================


% = reduce_graph(+OldGraph,-ReducedGraph)
% 
% the graph is reduced according to Danos' graph contraction
% condition. If a vertex N is found from which both destinations
% of a par pair reach the same par link M, then vertices M and N
% are identified. This proceeds until no more reductions can be
% performed. The final graph is checked for connectedness.

reduce_graph(G0,G) :-
        select(vertex(N,As,Ps0),G0,G1),
        select(M-M,Ps0,Ps),
        !,
        select(vertex(M,Bs,Qs),G1,G2),
        merge_vertices(vertex(N,As,Ps),vertex(M,Bs,Qs),G2,G3),
        reduce_graph(G3,G).
reduce_graph(G,G) :-
        connected(G).


% = merge_vertices(+Vertex1,+Vertex2,+OldGraph,-NewGraph)
%
% Vertex1 and Vertex2 are merged into a single vertex, with the vertex
% id of Vertex1 and all references to the id of Vertex2 are replaced
% by references to Vertex1.

merge_vertices(vertex(N,As,Ps),vertex(M,Bs,Qs),G0,[vertex(N,Cs,Rs)|G]) :-
      append(Bs,As,Cs),
      append(Qs,Ps,Rs),
      replace_edges(G0,M,N,G).            

% = replace_edges(+OldGraph,VertexId1,VertexId2,-NewGraph)
%
% All references to VertexId1 in OldGraph are replaced by references
% to VertexId2 in NewGraph.

replace_edges([],_,_,[]).
replace_edges([vertex(N,As,Ps)|Ls],X,Y,[vertex(N,As,Qs)|Ms]) :-
       replace_edges1(Ps,X,Y,Qs),
       replace_edges(Ls,X,Y,Ms).

replace_edges1([],_,_,[]).
replace_edges1([P1-P2|Ps],X,Y,[Q1-Q2|Qs]) :-
        replace_vertex(P1,X,Y,Q1),
        replace_vertex(P2,X,Y,Q2),
        replace_edges1(Ps,X,Y,Qs).

replace_vertex(V,X,Y,W) :-      
     ( 
       V=X ->
       W=Y
     ;
       W=V
     ).

% = connected(+Graph)
%
% true if it is still possible to make Graph connected through
% performing axiom links.
% A graph with a single vertex is connected. A vertex which does not
% have atomic formulas (and therefore will not be further connected
% to other vertices) is only connected if it has at least one par pair 

connected([_]) :- !.

connected(L) :- 
       connected1(L).

connected1([vertex(_,As,Ps)|R]) :-
    (
       As = []
    ->
       Ps = [_|_]
    ;
       true
    ),
      connected1(R).
connected1([]).

% = cyclic(+Vertex1,+Vertex2,+Graph)
% true if linking Vertex1 and Vertex2 in Graph will produce
% a cycle for some switching of Graph.
% - If Vertex1 and Vertex2 don't have a common ancestor, linking
%   them won't produce a cycle.
% - If Vertex1 and Vertex2 have a common ancestor, linking them
%   will produce a cycle unless they are on different branches
%   of a par link.

cyclic(E1,E2,G0) :-
      select(A,[E1,E2|G0],G1),
      ancestor(A,E1,G1,_,Path1,[]),
      ancestor(A,E2,G1,_,Path2,[]),
      !,
      \+ branches(Path1,Path2).

branches([X|Xs],[Y|Ys]) :-
  (
      X=l(P),Y=r(P)
  ->
      true
  ;
      X=r(P),Y=l(P)
  ->
      true
  ;
      X=Y
  ->
      branches(Xs,Ys)
  ).

ancestor(vertex(N,_,Ps),vertex(M,_,Rs),G0,G,L0,L) :-
      ( N=M,
        L=L0
      ;
        Ps=[P1-P2|Ps0],
       (  select(vertex(P1,_,Qs),G0,G1),
          L0=[l(P1-P2)|L1],
          ancestor(vertex(P1,_,Qs),vertex(M,_,Rs),G1,G,L1,L)
       ;
         select(vertex(P2,_,Qs),G0,G1),
         L0=[r(P1-P2)|L1],
         ancestor(vertex(P2,_,Qs),vertex(M,_,Rs),G1,G,L1,L)
       ;
         ancestor(vertex(N,_,Ps0),vertex(M,_,Rs),G0,G,L0,L)
       )
      ).


% ============================================================
% Label Reductions
% ============================================================


% = normalize(+Label,-NormalLabel)
% perform label reductions and rewrite according to
% structural postulates until a normal label results.

% search is breadth first with closed set, and succeeds only once.
% solution(Label) = normal(Label)
% child(Parent,Child) = rewrite(Parent,Child)

normalize(Start0,Answer) :-
     remove_divisions(Start0,Start),
     breadth_star1([],1,[Start|B],B,node(Start,0,empty,empty),Answer).


% normalize(+Label,+NormalLabel,-PathHead,?PathTail)
% as normalize/2 but now with dl pair representing path to solution

normalize(S0,S,Path0,Path) :-
       remove_divisions(S0,S1,Path0,Path1),
       breadth_first_search(S1,S,Path1,Path),
       !.

% = normal(+Label)
% a label is normal if all occurences of unzip, unpack, 
% dl, dr, l and r have been reduced, the words are in the
% right order, and it contains no grammar-internal modes.

% normal labels can contain logical constants of the form x-X

normal(Label) :-
     normal(Label,-1,_).

normal(M-_,N0,N) :-
    (
      M = x
    ->                    
      N = N0 
    ;
     M is N0+1,
     N = M
    ).

normal(p(I,A,B),N0,N) :-
     external(I),
     normal(A,N0,N1),
     normal(B,N1,N).

normal(zip(I,A),N0,N) :-
     external_dia(I),
     normal(A,N0,N).
     

% = select_divisions(+Label,-LabelWithHole,-DList)
% when called with a Label select divisions will return a copy of that
% label with a hole on the place of the divisions in it, and a
% difference list containing Division-Hole pairs. The pairs are
% ordered in such a way that a Division is ground when all Holes
% before it one the list are filled

select_divisions(N-W,N-W) -->
     [].
select_divisions(zip(I,A0),zip(I,A)) -->
     select_divisions(A0,A).
select_divisions(unzip(I,A0),unzip(I,A)) -->
     select_divisions(A0,A).
select_divisions(unpack(I,A0),H) -->
     select_divisions(A0,A),
     [unpack(I,A)-H].
select_divisions(dr(I,A0,V),H) -->
     select_divisions(A0,A),
     [dr(I,A,V)-H].
select_divisions(dl(I,V,A0),H) -->
     select_divisions(A0,A),
     [dl(I,V,A)-H].
select_divisions(p(I,A0,B0),p(I,A,B)) -->
     select_divisions(A0,A),
     select_divisions(B0,B).
select_divisions(l(I,A0),l(I,A)) -->
     select_divisions(A0,A).
select_divisions(r(I,A0),r(I,A)) -->
     select_divisions(A0,A).



% remove_divisions(+Label,-DivFreeLabel)
% remove all dr, dl occurences from label 
 
% first select_divisions/4 is called, which returns a (possibly
% non-ground) DivFreeLabel and a difference-list containing
% Division-Hole pairs 

remove_divisions(S0,S) :-
     select_divisions(S0,S1,L,[]),
     remove_divisions1(L,S1,S).

% = remove_divisions(+ListOfDHPairs,?LabelWithHole,-GroundLabel)
% successively remove the divisions from the list of pairs and put
% them back in the corresponding holes

remove_divisions1([],S0,S) :-
     breadth_star([],1,[S0|B],B,node(S0,0,empty,empty),S). % check lp even if there
					 % are no divisions
remove_divisions1([D-H|Rest],S0,S) :-
     breadth_star([],1,[D|B],B,node(D,0,empty,empty),H),
     remove_divisions1(Rest,S0,S).

% FnF: no comment for remove_divisions/4 ?

remove_divisions(S0,S,Path0,Path) :-
        select_divisions(S0,S1,L,[]),
        remove_divisions1(L,S1,S,Path0,Path).

remove_divisions1([],S0,S,Path0,Path) :-
        breadth_first_search1(S0,S,Path0,Path).
remove_divisions1([D-H|Rest],S0,S,Path0,Path) :-
        breadth_first_search1(D,H,Path0,Path1),
        remove_divisions1(Rest,S0,S,Path1,Path).

% breadth first search which returns dl path to solution
% because we know there is a solution we represent the Open
% set as a dl instead of a queue

breadth_first_search(Start,Answer,Path0,Path) :-
        breadth_star([],1,[Start/[]|Rest],Rest,[Start],[],RevPath,Answer),
        reverse_dl(RevPath,Path0,Path).

breadth_first_search1(Start,Answer,Path0,Path) :-
        breadth_star([],1,[Start/[]|Rest],Rest,[Start],[],RevPath,Answer),
        check_lp1(Answer),
        reverse_dl(RevPath,Path0,Path).

% search is breadth first with closed set, and succeeds only once
%
% solution(Label) = check_lp(Label)
% child(Parent,Child) = rewrite(Parent,Child)

% = breadth_star(+NewChildren,+QLength,+QFront,+QBack,+Closed,?Answer)

breadth_star([],N0,[Node|F],B,Closed,Answer) :-
     N0 > 0,
     N is N0-1,
     ( 
       check_lp(Node)
     ->
       Answer = Node
     ;
       children(Node,Children),
       union(Children,Closed,Closed1,Children1,[]),
       breadth_star(Children1,N,F,B,Closed1,Answer)
     ).
breadth_star([X|Xs],N0,F,[X|B],Closed,Answer) :-
     N is N0+1,
     update_statistics('search nodes'),
     breadth_star(Xs,N,F,B,Closed,Answer).

% FnF: no comment for breadth_star/8


breadth_star([],N0,[Node/NodePath|Front],Back,Closed,_NodePath0,RevPath,Answer) :-
       N0 > 0,
       N is N0-1,
       ( 
         Answer = Node, RevPath = NodePath
       ; 
         labelled_children(Node,Children),
         lb_ord_union(Closed,Children,Closed1,Children1),
         breadth_star(Children1,N,Front,Back,Closed1,NodePath,RevPath,Answer)
       ).

breadth_star([Child-Label|Children],N0,F,[(Child/[Label|Path])|B],Closed,
              Path,RevPath,Answer) :-
       N is N0+1,
       breadth_star(Children,N,F,B,Closed,Path,RevPath,Answer).


breadth_star1([],N0,[Node|F],B,Closed,Answer) :-
     N0 > 0,
     N is N0-1,
     (
       normal(Node)
     ->
       Answer = Node
     ; 
       children(Node,Children),
       union(Children,Closed,Closed1,Children1,[]),
       breadth_star1(Children1,N,F,B,Closed1,Answer)
     ).
breadth_star1([X|Xs],N0,F,[X|B],Closed,Answer) :-
     N is N0+1,
     update_statistics('search nodes'),
     breadth_star1(Xs,N,F,B,Closed,Answer).



% = Linear precedence

% = check_lp(+Label)
%
% true if the words in Label are in the order they appear in the
% input list of words. This is only checked for modes which are
% declares as transparent, since it's not necessarily true that
% we can put the words in the right order immediately. 

check_lp(Node) :-
     check_lp(Node,x,_).

% check_lp(+Node,+GreaterThan,-Max)

check_lp(N-_,X,Y) :-
    (
      N = x 
    ->
      X = Y
    ;
      precedes(X,N), 
      N = Y
    ).

check_lp(p(I,A,B),N0,N) :-
   (
      transparent(I)
   ->
      check_lp(A,N0,N1),
      check_lp(B,N1,N)
    ;
      check_lp(A,x,_),
      check_lp(B,x,_)
   ).
 
check_lp(zip(I,A),N0,N) :-
   (
      transparent_dia(I)
   ->
      check_lp(A,N0,N)
    ;
      check_lp(A,x,_)
   ).

check_lp(unzip(_,A),N0,N) :-
     check_lp(A,N0,N).

check_lp(unpack(_,A),N0,N) :-
     check_lp(A,N0,N).

check_lp(dl(_,_,A),N0,N) :-
     check_lp(A,N0,N).

check_lp(dr(_,A,_),N0,N) :-
     check_lp(A,N0,N).

check_lp(l(_,A),N0,N) :-
     check_lp(A,N0,N).

check_lp(r(_,A),N0,N) :-
     check_lp(A,N0,N).


% =

check_lp1(_-_).
check_lp1(zip(_,A)) :-
    check_lp1(A).
check_lp1(p(_,A,B)) :-
    check_lp1(A),
    check_lp1(B).
check_lp1(l(_,A)) :-
    check_lp1(A).
check_lp1(r(_,A)) :-
    check_lp1(A).
check_lp1(unzip(_,A)) :-
    check_lp1(A).

% = children/2 will generate a set of children from a given parent.

children(ParentNode,ChildrenNodes) :-
     findall(ChildNode,rewrite(ParentNode,ChildNode),ChildrenNodes).

labelled_children(ParentNode,ChildrenSet) :-
     findall(ChildNode-Label,rewrite(ParentNode,ChildNode,Label),ChildrenNodes),
     keysort(ChildrenNodes,ChildrenSet).

% =
     
add_children([],_,L,L).

add_children([Child-Label|Children],Path,[(Child/[Label|Path])|L0],L) :-
      add_children(Children,Path,L0,L).

% =

% rewrite(+Label0,?Label) 
% true if Label0 is obtainable from Label in a single residuation 
% reduction or postulate rewrite 

rewrite(D,D1) :-
     reduce(D,D1).
rewrite(D,D1) :-
     postulate(D,D1,_Name).
rewrite(unpack(J,D),unpack(J,D1)) :-
     rewrite(D,D1).
rewrite(unzip(J,D),unzip(J,D1)) :-
     rewrite(D,D1).
rewrite(zip(J,D),zip(J,D1)) :-
     rewrite(D,D1).
rewrite(l(J,D),l(J,D1)) :-
     rewrite(D,D1).
rewrite(r(J,D),r(J,D1)) :- 
     rewrite(D,D1).
rewrite(p(J,D,G),p(J,D1,G)) :-
     rewrite(D,D1).
rewrite(p(J,G,D),p(J,G,D1)) :-
     rewrite(D,D1).
rewrite(dr(J,D,G),dr(J,D1,G)) :-
     rewrite(D,D1).
rewrite(dl(J,G,D),dl(J,G,D1)) :-
     rewrite(D,D1).


% rewrite(+Label0,?Label,?Name) 
% true if Label0 is obtainable from Label in a single residuation 
% reduction or postulate rewrite by applying the rule indicated
% by Name 

rewrite(unpack(J,D0),unpack(J,D),L) :-
     rewrite(D0,D,L).
rewrite(unzip(J,D0),unzip(J,D),L) :-
     rewrite(D0,D,L).
rewrite(zip(J,D0),zip(J,D),L) :-
     rewrite(D0,D,L).
rewrite(l(J,D0),l(J,D),L) :-
     rewrite(D0,D,L).
rewrite(r(J,D0),r(J,D),L) :- 
     rewrite(D0,D,L).
rewrite(p(J,D0,G),p(J,D,G),L) :-
     rewrite(D0,D,L).
rewrite(p(J,G,D0),p(J,G,D),L) :-
     rewrite(D0,D,L).
rewrite(dr(J,D0,G),dr(J,D,G),L) :-
     rewrite(D0,D,L).
rewrite(dl(J,G,D0),dl(J,G,D),L) :-
     rewrite(D0,D,L).
rewrite(D0,D,D0) :-
     reduce(D0,D).
rewrite(D0,D,sr(Name,D0,D)) :-
     postulate(D0,D,Name).


% = Residuation reductions

reduce(p(J,l(J,D),r(J,D)),D). % product
reduce(dr(J,p(J,G,D),D),G).   % division right
reduce(dl(J,D,p(J,D,G)),G).   % division left
reduce(zip(J,unzip(J,D)),D).  % diamond
reduce(unpack(J,zip(J,D)),D). % box


% ============================================================
% Auxiliaries
% ============================================================

generate_antecedent(L-W,L-W).
generate_antecedent(p(I,X,Y),sp(I,A,B)) :-
       generate_antecedent(X,A),
       generate_antecedent(Y,B).
generate_antecedent(zip(I,X),sdia(I,A)) :-
       generate_antecedent(X,A).


reverse_dl([],Y,Y).

reverse_dl([X|Xs],Ys,Zs) :-
    reverse_dl(Xs,Ys,[X|Zs]).

% =

duo_select(X,Y,[X|Xs],Xs,[Y|Ys],Ys).
duo_select(V,W,[X|Xs],[X|Vs],[Y|Ys],[Y|Ws]) :-
       duo_select(V,W,Xs,Vs,Ys,Ws).


%   lb_ord_union/4 is ord_union/4 (form Quintus library(ordsets) 
%   where NewSet and ReallyNew are Item-Label pairs and OldSet and 
%   Union lists of Items

%   lb_ord_union(+OldSet, +NewSet, ?Union, ?ReallyNew)
%   is true when Union is NewSet U OldSet and ReallyNew is NewSet \ OldSet.
%   This is useful when you have an iterative problem, and you're adding
%   some possibly new elements (NewSet) to a set (OldSet), and as well as
%   getting the updated set (Union) you would like to know which if any of
%   the "new" elements didn't already occur in the set (ReallyNew).

lb_ord_union([], Set2, Set2, Set2).
lb_ord_union([Head1|Tail1], Set2, Union, New) :-
	lb_ord_union_1(Set2, Head1, Tail1, Union, New).

lb_ord_union_1([], Head1, Tail1, [Head1|Tail1], []).
lb_ord_union_1([Head2-Label|Tail2], Head1, Tail1, Union, New) :-
	compare(Order, Head1, Head2),
	lb_ord_union(Order, Head1, Tail1, Label, Head2, Tail2, Union, New).

lb_ord_union(<, Head1, Tail1, Label, Head2, Tail2, [Head1|Union], New) :-
	lb_ord_union_2(Tail1, Label, Head2, Tail2, Union, New).
lb_ord_union(>, Head1, Tail1, Label, Head2, Tail2, [Head2|Union], [Head2-Label|New]) :-
	lb_ord_union_1(Tail2, Head1, Tail1, Union, New).
lb_ord_union(=, Head1, Tail1, _, _,    Tail2, [Head1|Union], New) :-
	lb_ord_union(Tail1, Tail2, Union, New).

lb_ord_union_2([], Label, Head2, Tail2, [Head2|Tail2], [Head2-Label|Tail2]).
lb_ord_union_2([Head1|Tail1], Label, Head2, Tail2, Union, New) :-
	compare(Order, Head1, Head2),
	lb_ord_union(Order, Head1, Tail1, Label, Head2, Tail2, Union, New).
