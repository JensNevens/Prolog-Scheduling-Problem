%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project Declarative Programming %
%           Jens Nevens           %
%								  %
%		  find_optimal(-S)        %
%          is_optimal(?S)         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(find_optimal, [find_optimal/1, 
	                     is_optimal/1]).
%:- use_module(small_instance).
%:- use_module(large_short_instance).
%:- use_module(large_long_instance).
:- use_module(utils).
:- use_module(valid).
:- use_module(cost).

:- dynamic best/2.

% find_optimal(-S)
%		-S: Schedule with lowest cost.
%
%		Binds S to a schedule with the lowest cost.
%		For this, the optimal/1 predicate is called.
%		This will assert all best schedules with the
%		best/2 functor. Since find_optimal/1 only needs
%		to return one possible schedule, a cut is placed
%		at the end.
find_optimal(Schedule) :-
	optimal(_),
	best(Schedule, Cost),
	format("Best Cost is ~f", Cost),
	retractall(best(_,_)),
	!.

% is_optimal(-S)
%		-S: Schedule with lowest cost
%
%		Binds S to all possible schedules with lowest
%		cost. For this, the optimal/1 predicate is called.
%		This will assert all best schedules with the best/2
%		functor. Since is_optimal/1 needs to return all
%		optimal schedules, it is allowed to backtrack
%		over the best-predicate.
is_optimal(Schedule) :-
	optimal(_),
	best(Schedule, Cost),
	format("Best Cost is ~f", Cost),
	retract(best(Schedule,_)).

% optimal(-S)
%		-S: Schedule with lowest cost
%
%		Generates all possible schedules, calculates their
%		cost and updates the best using update_best/2.
%		The fail at the end ensures backtracked over is_valid/1.
%		When this is no longer possible, optimal(-S) succeeds
%		through the second predicate.
optimal(_) :- 
	assert(best(nil,1000000)), 
	is_valid(Schedule),
	cost(Schedule,Cost), 
	update_best(Schedule,Cost),
	fail.

optimal(_) :- !.

% update_best(+Schedule, +Cost)
%		+Schedule: schedule/1 functor that contains a list of
%			event/4 functors.
%		+Cost: integer
%
%		Succeeds when the best schedule, i.e. the schedule with 
%		the lowest cost, is replaced with the given schedule with 
%		given cost because its cost is even lower. When the cost is
%		the same, the schedule is also asserted using the best/2
%		function. When this is not the case, nothing is done.
update_best(Schedule, Cost) :-
	best(_,BestCost),
	Cost == BestCost,
	!,
	assert(best(Schedule,Cost)).

update_best(Schedule,Cost) :- 
	best(_,BestCost), 
	Cost < BestCost, 
	!, 
	retractall(best(_,_)),
	assert(best(Schedule,Cost)).

update_best(_,_).




