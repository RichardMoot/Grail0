% ---------------------------------------------------------------------
% $Id: statistics.pl,v 1.6 2002/12/02 15:34:37 vermaat Exp $
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

:- module(statistics,[write_statistics/0,
                      reset_statistics/1,
		      reset_statistic/1,
                      update_statistics/1]).


:- use_module(auxiliaries).

:- dynamic 'gather statistics'/1.

'gather statistics'(on).

% ---------------------------------------------------------------------
% reset_prolog_state
%
% cleans up old information from the database.
% ---------------------------------------------------------------------
reset_statistics(off) :-
	retractall(statistics:'gather statistics'(_)),
	assert(statistics:'gather statistics'(off)),
	!.

reset_statistics(on) :-
	reset_statistic('total links'),
	reset_statistic('plan links'),
	reset_statistic('acc links'),
	reset_statistic('lab links'),
	reset_statistic('search nodes'),
	reset_statistic('start time'),
	reset_statistic('garbage collection'),
	retractall(statistics:'gather statistics'(_)),
	assert(statistics:'gather statistics'(on)),
	!.


% Special cases
reset_statistic('start time') :-
	predicate_for_key('start time', _, OldStartTime),
	retractall(OldStartTime),
	statistics(runtime, [Time|_]),
	predicate_for_key('start time', Time, NewStartTime),
	assert(NewStartTime),
	!.

reset_statistic('garbage collection') :-
	predicate_for_key('garbage collection', _, OldGarbageCollection),
	retractall(OldGarbageCollection),
	statistics(garbage_collection, GarbageCollection),
	predicate_for_key('garbage collection', GarbageCollection, NewGarbageCollection),
	assert(NewGarbageCollection),
	!.

% Generic reset
reset_statistic(StatisticItem) :-
	predicate_for_key(StatisticItem, _, OldPredicate),
	retractall(OldPredicate),
	predicate_for_key(StatisticItem, 0, NewPredicate),
	assert(NewPredicate).

%update_statistics(_) :- !.

% Generic update
update_statistics(_) :-
	statistics:'gather statistics'(off),
	!.

update_statistics(StatisticItem) :-
	predicate_for_key(StatisticItem, OldValue, CurrentLocalizedStatisticPredicate),
 	retract(CurrentLocalizedStatisticPredicate),
 	NewValue is OldValue + 1,
	predicate_for_key(StatisticItem, NewValue, NewLocalizedStatisticPredicate),
 	assert(NewLocalizedStatisticPredicate).

% = write_statistics
% 
% print out some runtime statistics to the screen.

write_statistics :-
	statistics:'gather statistics'(off),
	!.

write_statistics :-
     format('~n== statistics ==~2n',[]),
     statistics(runtime,[T|_]),
     statistics:'start time'(T0),
     Time is (T-T0)*0.001,
     format('CPU Time used : ~p~n',[Time]),
     statistics(garbage_collection,[NGC1,Bytes1,GCTime1,_]),
     statistics:'garbage collection'([NGC0,Bytes0,GCTime0,_]),
     NGC is NGC1 - NGC0,
 (
     NGC =:= 0
 ->
      true
 ;
      KBytes is (Bytes1-Bytes0) // 1024,
      GCTime is (GCTime1-GCTime0)*0.001,
      format('Garbage coll. : ~D (~D Kb, ~p sec)~n',[NGC,KBytes,GCTime])
 ),
     statistics:'search nodes'(NOD),
     format('Label count   : ~D~n',[NOD]),
     statistics:'total links'(TL),
     statistics:'plan links'(PL),
     statistics:'acc links'(ACC),
     statistics:'lab links'(LAB),
     DifPl is TL - PL,
     Dif1 is PL - ACC,
     Dif2 is ACC - LAB,
     calculate_tabs([TL,PL,ACC,LAB],[TTL,TPL,TACC,TLAB]),
     calculate_tabs([DifPl,Dif1,Dif2],[TDifPl,TDif1,TDif2]),
     write('Total links   : '),tab(TTL),format('~D~n',[TL]),
     write('Planar links  : '),tab(TPL),format('~D ',[PL]),
                               tab(TDifPl),format('(~D)~n',[DifPl]),
     write('ACC links     : '),tab(TACC),format('~D ',[ACC]),
                               tab(TDif1),format('(~D)~n',[Dif1]),
     write('Label links   : '),tab(TLAB),format('~D ',[LAB]),
                               tab(TDif2),format('(~D)~n',[Dif2]),
			       reset_statistics(on).


% Auxilary; returns predicate-in-module for statistic name
predicate_for_key(StatisticItem, Var, Predicate) :-
	UnqualifiedPredicate =.. [StatisticItem, Var],
	Predicate =.. [:, statistics, UnqualifiedPredicate].


:- 'gather statistics'(S),
   reset_statistics(S).
