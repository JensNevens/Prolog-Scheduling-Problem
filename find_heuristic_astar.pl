%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project Declarative Programming %
%           Jens Nevens           %
%								  %
%     find_heuristically(-S)      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(find_heuristic_astar, [find_heuristically/1]).

%:- use_module(small_instance).
%:- use_module(large_short_instance).
%:- use_module(large_long_instance).

:- use_module(utils).
:- use_module(valid).
:- use_module(cost).

:- dynamic to_schedule/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE: THIS DOES NOT ALWAYS WORK IN UNDER %
% 2 MINUTES FOR THE LARGE SHORT INSTANCE   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         A* Algorithm                        %
%												              %
% Nodes have form node(S,D,F)                                 %
%   where S describes the state or configuration              %
%         D is the depth of the node                          %
%         F is the evaluation function value                  %
% Based on                                                    %
%   http://www.cpp.edu/~jrfisher/www/prolog_tutorial/5_1.html %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve(+State, -Solution)
%		+State: Starting state description
%		-Solution: Solution found by the A* Algorithm
%
%		Evalutate the search function f on the start state
%		and start the search process.
solve(State, Solution) :- 
	f_function(State, 0, F),
	search([node(State,0,F)], Solution).

% f_function(+State, +Depth, -Cost)
%		+State: A State description
%		+Depth: Depth of current state
%		-Cost: Effort to successfully get from start 
%			to goal by going through this node
%
%		Evalutate f-function for current state
f_function(State,D,F) :- 
	h_function(State,H),
	F is D + H.

% search(+OpenList, -Solution)
%		+OpenList: List of states to be investigated
%		-Solution: Solution found by A* Algorithm
%
%		If first element of OpenList is a goal, return
%		this state. We only want 1 solution, so cut.
%		Otherwise, expand all children of current node,
%		sort them according to cost and proceed with this
%		as OpenList.
search([node(State,_,_)|_], State) :- 
	goal(State), 
	retract(to_schedule(_)),
	!.

search([Node|_], Solution) :-
	expand(Node,Children),
	sort_functors_asc(3, Children, SortedChildren),
	search(SortedChildren, Solution).

% expand(+State, -Children)
%		+State: A state description
%		-Children: All children of State
%
%		Generate all children of State. These all have
%		depth+1 and know their cost via f_function.
expand(node(State,D,_), AllChildren) :-
	 bagof(node(Child, D1, F),
		   (D1 is D+1,
			 move(State,Child),
			 f_function(Child,D1,F)),
		   AllChildren).

%%%%%%%%%%%%%%%%%%%%%%%
% End of A* Algorithm %
%%%%%%%%%%%%%%%%%%%%%%%

% find_heuristically(-Schedule)
%		-Schedule: schedule/1 functor that contains a
%			list of event/4 functors
%
%		Invoke preprocessing, assert the exams to be
%		scheduled in a random order and start the 
%		A* Algorithm.
find_heuristically(Schedule) :-
	findall(EID, exam(EID,_), ExamIDs),
	do_preprocess(ExamIDs),
	random_permutation(ExamIDs, Shuffled),
	asserta(to_schedule(Shuffled)),
	solve([], Events),
	Schedule = schedule(Events),
	cost(Schedule,Cost),
	format("Cost of this solution is ~f", Cost).

% h_function(+State, -Cost)
%		+State: a State description
%		-Cost: Cost of this State
%
%		f_function in A* Algorithm is the
%		h_function + depth. h_function in this case
%		is the cost of the schedule.
h_function(State,H) :-
	cost(schedule(State),H).

% goal(+State)
%		+State: a State description
%
%		A state is a goal state if it is a valid schedule
goal(State) :-
	is_valid(schedule(State)).

% move(+State, -Child)
%		+State: A State description
%		-Child: A child of this State
%
%		The next exam that needs to be scheduled is taken.
%		An event is created for this exam and added to the schedule
%		in such a way that the schedule remains valid. This is done
%		in the next_valid/3 predicate.
move(State,ChildState) :-
	to_schedule([EID|RestExams]),
	retract(to_schedule(_)),
	asserta(to_schedule(RestExams)),
	next_valid(State, EID, Child),
	append([Child],State,ChildState).

% next_valid(+Events, +EID, -Event)
%		+Events: Already scheduled events
%		+EID: Exam ID of the exam to be scheduled
%		-Event: Resulting event so the schedule remains valid
%
%		An event can be added when the room is large enough
%		for the number of students in the course, when a room is
%		available and when it does not cause conflicts with any
%		of the already scheduled events.
next_valid(Events, EID, event(EID,RID,Day,Start)) :-
	exam(EID,_),
	has_exam(CID,EID),
	follows_course(CID,Students),
	length(Students, StudentSize),
	room(RID,_),
	capacity(RID, Capacity),
	Capacity >= StudentSize,
	availability(RID, Day, StartHour, EndHour),
	between(StartHour, EndHour, Start),
	duration(EID,Duration),
	End is Start + Duration,
	End =< EndHour,
	not(conflict(event(EID,RID,Day,Start), Events)).




