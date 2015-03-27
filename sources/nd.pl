% ---------------------------------------------------------------------
% $Id: nd.pl,v 1.24 2002/12/04 21:17:45 gjv Exp $
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


:- module(nd,
		  [first_nd/6,
		   collapse/2,
		   eta_reduce/2]).

:- use_module(compatibility).
:- (prolog_vendor(sics) -> use_module(library(lists), [append/3,select/3]) ; true).

:- use_module([auxiliaries,options,reduce_sem]).

% ---------------------------------------------------------------------
% Proof net -> Natural Deduction
% ---------------------------------------------------------------------



% FnF: return first ND (no backtracking to get more nds)
%
first_nd(Hyp, OneRules, TwoRules, Con0, Con, ND0) :-
	generate_nd(Hyp, OneRules, TwoRules, Con0, Con, ND0),
	!.
first_nd(_, _, _, _, _, _) :-
     write('Error generating deduction!!!'), % this should never happen
     nl,
     !,
     fail.




% generate_nd(+ListOfNDs,+ListOfOneRules,+ListOfTwoRules,
%             ,-NDOut).

generate_nd([ND0|NDs0],One,Two,Con0,Con,ND) :-
        discharged(NDs0,ND0,1,_,Disch),
        generate_nd([ND0|NDs0],One,Two,Con0,Con,Disch,_,ND).

generate_nd([ND],[],[],Con,Con,D,D,ND).

generate_nd(NDs,Ones,Twos,Con0,Con,D0,D,ND) :-
        select(Two,Twos,NewTwos),
        match_two(Two,NDs,NewNDs),
        generate_nd(NewNDs,Ones,NewTwos,Con0,Con,D0,D,ND).

generate_nd(NDs0,Ones0,Twos,Con0,Con,D0,D,ND) :-
	select(One,Ones0,Ones1),
        match_one(One,NDs0,NDs1,A,B,D0,Con0),
      ( Ones0 \== [One|Ones1] ->
	  portray_selection(Ones0,One)
      ;
	  true
      ),
	map_replace_label(Ones1,A,B,Ones),
        replace_labels(NDs1,A,B,NDs),
        replace_labels(D0,A,B,D1),
        replace_labels2(Twos,A,B,NewTwos),
        update_const(Con0,A,B,Con1),
        generate_nd(NDs,Ones,NewTwos,Con1,Con,D1,D,ND).

portray_selection(_X,_Y).

discharged([],X,N0,N,Zs) :-
       discharged1(X,N0,N,Zs,[]).
discharged([X|Xs],Y,N0,N,Zs0) :-
       discharged1(Y,N0,N1,Zs0,Zs),
       discharged(Xs,X,N1,N,Zs).

discharged1(R,N0,N,Xs,Ys) :-
        R=rule(Name,_,_,_,_),
        discharged2(Name,R,N0,N,Xs,Ys).

discharged2(lex,_,N,N,Rs,Rs).
discharged2(hyp(N0),R,N0,N,[R|Rs],Rs) :- N is N0+1.
discharged2(uhyp,_,N,N,Rs,Rs).





% match_one(+OneRule,+NDsIn,-NDsOut,-Redex,-Contractum)
% 
     	
match_one(dr(I,p(I,L1,L2),L2),S0,[rule(dri(I,N),L1,dr(I,A,B),lambda(X,Y),[R])|S],
        dr(I,p(I,L1,L2),L2),L1,Disch,_) :-
        select(R,S0,S),
        R=rule(_,p(I,L1,L2),A,Y,_),
        member_check(rule(hyp(N),L2,B,X,_),Disch).

match_one(dl(I,L1,p(I,L1,L2)),S0,[rule(dli(I,N),L2,dl(I,B,A),lambda(X,Y),[R])|S],
        dl(I,L1,p(I,L1,L2)),L2,Disch,_) :-
        select(R,S0,S),
        R=rule(_,p(I,L1,L2),A,Y,_),
        member_check(rule(hyp(N),L1,B,X,_),Disch).

match_one(p(J,l(J,D),r(J,D)),S0,[rule(pe(J,N1,N2),GammaAPB,C,Sem,[P,R])|S],
        p(J,l(J,D),r(J,D)),D,Disch,Con) :-
        select(R,S0,S1),
        R=rule(_,GammaAB,C,Sem0,_),
        replace_db_label(GammaAB,p(J,l(J,D),r(J,D)),D,GammaAPB),
        select(P,S1,S),
        P=rule(_,D,p(J,A,B),SemProd,_),
        \+ GammaAB = GammaAPB,
        member_check(cf(l(J,D),_,CX),Con),
        member_check(cf(r(J,D),_,CY),Con),
        member_check(rule(hyp(N1),l(J,D),A,_,_),Disch),
        member_check(rule(hyp(N2),r(J,D),B,_,_),Disch),
        replace_sem(Sem0,CX,fst(SemProd),Sem1),
        replace_sem(Sem1,CY,snd(SemProd),Sem).

match_one(zip(J,unzip(J,D)),S0,[rule(diae(J,N),GammaDiaA,C,Sem,[P,R])|S],
        zip(J,unzip(J,D)),D,Disch,Con) :-
        select(R,S0,S1),
        R=rule(_,GammaA,C,Sem0,_),
        select(P,S1,S),
        P=rule(_,D,dia(J,A),SemDiaA,_),
        replace_db_label(GammaA,zip(J,unzip(J,D)),D,GammaDiaA),
        \+ GammaA = GammaDiaA,
        member_check(cf(unzip(J,D),_,CA),Con),
        member_check(rule(hyp(N),unzip(J,D),A,_,_),Disch),
        replace_sem(Sem0,CA,dedia(SemDiaA),Sem).

match_one(unpack(J,zip(J,D)),S0,[rule(boxi(J),D,box(J,B),conbox(V),[R])|S],
        unpack(J,zip(J,D)),D,_,_):-
        select(R,S0,S),
        R=rule(_,zip(J,D),B,V,_).

match_one(sr(Name,L1,L2),S0,[rule(Name,GammaL2,C,X,[R])|S],L1,L2,_,_) :-
        select(R,S0,S),
        R=rule(_,GammaL1,C,X,_),
        replace_db_label(GammaL1,L1,L2,GammaL2),
        \+ GammaL1 = GammaL2.




% match_two(+TwoRule,+NDIn,-NDOut)
% 

match_two(dl(I,L1-B,p(I,L1,L2)-A),S0,[rule(dle(I),p(I,L1,L2),A,appl(X,Y),[P,R])|S]) :-
        select(R,S0,S1),
        R=rule(_,L2,dl(I,B,A),X,_),
        select(P,S1,S),
        P=rule(_,L1,B,Y,_).

match_two(dr(I,p(I,L2,L1)-A,L1-B),S0,[rule(dre(I),p(I,L2,L1),A,appl(X,Y),[R,P])|S]) :-
        select(R,S0,S1),
        R=rule(_,L2,dr(I,A,B),X,_),
        select(P,S1,S),
        P=rule(_,L1,B,Y,_).

match_two(p(I,L1-A,L2-B),S0,
      [rule(pi(I),p(I,L1,L2),p(I,A,B),pair(X,Y),[R,P])|S]) :-
         select(R,S0,S1),
         R=rule(_,L1,A,X,_),
         select(P,S1,S),
         P=rule(_,L2,B,Y,_).

match_two(dia(I,L1-C),S0,
      [rule(diai(I),zip(I,L1),dia(I,C),condia(X),[R])|S]) :-
         select(R,S0,S),
         R=rule(_,L1,C,X,_).

match_two(box(I,zip(I,L1)-A),S0,
      [rule(boxe(I),zip(I,L1),A,debox(X),[R])|S]) :-
         select(R,S0,S),
         R=rule(_,L1,box(I,A),X,_).





% ==========================================================

% = replace_type(+DB0,+A,+B,-DB)
% replacing type A with B in DB0 yields DB

replace_type(A,A,B,B).
replace_type(sp(I,G0,D),A,B,sp(I,G,D)) :-
        replace_type(G0,A,B,G).
replace_type(sp(I,G,D0),A,B,sp(I,G,D)) :-
        replace_type(D0,A,B,D).
replace_type(sdia(I,G0),A,B,sdia(I,G)) :-
        replace_type(G0,A,B,G).

update_const([],_,_,[]).
update_const([cf(L1,S1,X)|Xs],A,B,[cf(L1,S1,X),cf(L2,S1,X)|Zs]) :-
        replace_label(L1,A,B,L2),
        L1 \== L2,
        !,
        update_const(Xs,A,B,Zs).
update_const([X|Xs],A,B,[X|Zs]) :-
        update_const(Xs,A,B,Zs).





% ==========================================================

% rather cumbersome replacement of all references to a redex
% by its contractum

replace_labels([],_,_,[]).
replace_labels([rule(N,Ant0,Suc,Sem,R)|Rest0],A,B,[rule(N,Ant,Suc,Sem,R)|Rest]) :-
        replace_label(Ant0,A,B,Ant),
        replace_labels(Rest0,A,B,Rest).

map_replace_label([],_,_,[]).
map_replace_label([X|Xs],A,B,[Y|Ys]) :-
	replace_label(X,A,B,Y),
	map_replace_label(Xs,A,B,Ys).

map_replace_label1([],_,_,[]).
map_replace_label1([X-H|Xs],A,B,[Y-H|Ys]) :-
	replace_label(X,A,B,Y),
	map_replace_label1(Xs,A,B,Ys).

replace_label1(zip(I,C0),A,B,zip(I,C)) :-
        replace_label1(C0,A,B,C).
replace_label1(p(I,C0,D0),A,B,p(I,C,D)) :-
        replace_label1(C0,A,B,C),
        replace_label1(D0,A,B,D).
replace_label1(L0-T,A,B,L-T) :-
        replace_label(L0,A,B,L).

replace_label(A,A,B,B) :- !.
replace_label(sr(Name,C0,D0),A,B,sr(Name,C,D)) :-
	replace_label(C0,A,B,C),
	replace_label(D0,A,B,D).
replace_label(L-C,_,_,L-C).
replace_label(p(I,C0,D0),A,B,p(I,C,D)) :-
        replace_label(C0,A,B,C),
        replace_label(D0,A,B,D).
replace_label(dl(I,C,D0),A,B,dl(I,C,D)) :-
        replace_label(D0,A,B,D).
replace_label(dr(I,C0,D),A,B,dr(I,C,D)) :-
        replace_label(C0,A,B,C).
replace_label(l(I,C0),A,B,l(I,C)) :-
        replace_label(C0,A,B,C).
replace_label(r(I,C0),A,B,r(I,C)) :-
        replace_label(C0,A,B,C).
replace_label(zip(I,C0),A,B,zip(I,C)) :-
        replace_label(C0,A,B,C).
replace_label(unzip(I,C0),A,B,unzip(I,C)) :-
        replace_label(C0,A,B,C).
replace_label(unpack(I,C0),A,B,unpack(I,C)) :-
        replace_label(C0,A,B,C).

replace_db_label(A,A,B,B) :- !.
%replace_db_label(L-C,_,_,L-C).
replace_db_label(p(I,C0,D),A,B,p(I,C,D)) :-
        replace_db_label(C0,A,B,C).
replace_db_label(p(I,C,D0),A,B,p(I,C,D)) :-
        replace_db_label(D0,A,B,D).
replace_db_label(zip(I,C0),A,B,zip(I,C)) :-
        replace_db_label(C0,A,B,C).

replace_labels2([],_,_,[]).
replace_labels2([X|Xs],A,B,[Y|Ys]) :-
        replace_label2(X,A,B,Y),
        replace_labels2(Xs,A,B,Ys).

replace_label2(p(I,L1-X,L2-Y),A,B,p(I,L3-X,L4-Y)) :-
        replace_label(L1,A,B,L3),
        replace_label(L2,A,B,L4).
replace_label2(dl(I,L1-X,L2-Y),A,B,dl(I,L3-X,L4-Y)) :-
        replace_label(L1,A,B,L3),
        replace_label(L2,A,B,L4).
replace_label2(dr(I,L1-X,L2-Y),A,B,dr(I,L3-X,L4-Y)) :-
        replace_label(L1,A,B,L3),
        replace_label(L2,A,B,L4).
replace_label2(dia(I,L1-X),A,B,dia(I,L2-X)) :-
        replace_label(L1,A,B,L2).
replace_label2(box(I,L1-X),A,B,box(I,L2-X)) :-
        replace_label(L1,A,B,L2).






% ===

collapse(rule(A,B,C,X,[]),rule(A,B,C,X,[])) :- !.
collapse(rule(A,B,C,X,Ds),Out) :-
      (logical_rule(A) ->
          collapse1(Ds,Hs),
          Out=rule(A,B,C,X,Hs)
       ;
      (output_sr(yes) ->
       (collapse_sr(Xs,E),
        member_check(A,Xs),
        collapse2(Ds,Xs,Gs) ->
        collapse1(Gs,Hs)
       ;
        A=E,
        collapse1(Ds,Hs)
       ),
       Out=rule(E,B,C,X,Hs))
      ;  
        collapse1(Ds,[rule(A1,_,_,_,Ds1)]),
        Out=rule(A1,B,C,X,Ds1)
      ).

collapse1([],[]).
collapse1([X|Xs],[Y|Ys]) :-
        collapse(X,Y),
        collapse1(Xs,Ys).








% collapse ListOfRules only if all rules are in CR

collapse2([rule(A,_,_,_,Es)],Xs,Fs) :-
        \+ logical_rule(A) ->
        member_check(A,Xs),
        collapse3(Es,Xs,Fs).

collapse3([rule(A,_,_,_,Es)],Xs,Fs) :-
        \+ logical_rule(A),
        member_check(A,Xs),
        !,
        collapse3(Es,Xs,Fs).

collapse3(Fs,_,Gs) :-
        collapse1(Fs,Gs).







% = eta_reduce(+Proof,-EtaReducedProof)
% performs eta reductions on proof.

eta_reduce(Proof0,Proof) :-
   (eta_long_proofs(yes) ->
    Proof=Proof0
   ;
    eta_reduce1(Proof0,Proof)). 

eta_reduce1(rule(N,A,S,Sem,Rules0),Z) :-
    map_eta_reduce(Rules0,Rules),
    \+ Rules0=Rules,
    !,
    eta_reduce1(rule(N,A,S,Sem,Rules),Z).
eta_reduce1(X,Z) :-
    eta_reduction(X,Y),
    !,
    eta_reduce1(Y,Z).
eta_reduce1(X,X).

map_eta_reduce([],[]).
map_eta_reduce([X|Xs],[Y|Ys]) :-
     eta_reduce(X,Y),
     map_eta_reduce(Xs,Ys).

replace_rule(W,X,Y,Z,F) :-
       (W=X ->
	   Y=Z,
	   F=1
       ;
	   W=Z,
	   F=0).

replace_rule(rule(A,B,C,D,Rs0),X,Y,rule(A,B,C,D,Rs),F) :-
	replace_rule1(Rs0,X,Y,Rs,F).

replace_rule1([],_,_,[],0).
replace_rule1([R0|Rs0],X,Y,[R|Rs],F) :-
	replace_rule(R0,X,Y,R,F0),
	replace_rule1(Rs0,X,Y,Rs,F1),
	F is F0\/F1.


eta_reduction(rule(dli(I,_),X,dl(I,A,B),_Sem1,[
               rule(dle(I),p(I,Y,X),B,_Sem2,[
                rule(hyp(_),Y,A,_Sem3,[]),
                REDEX  
               ])
              ]),
              REDEX).
eta_reduction(rule(dri(I,_),X,dr(I,B,A),_Sem1,[
               rule(dre(I),p(I,X,Y),B,_Sem2,[
                REDEX,
                rule(hyp(_),Y,A,_Sem3,[])
               ])
              ]),
              REDEX).
eta_reduction(rule(boxi(I),X,box(I,A),_Sem1,[rule(boxe(I),zip(I,X),A,_Sem2,[REDEX])]),REDEX).

