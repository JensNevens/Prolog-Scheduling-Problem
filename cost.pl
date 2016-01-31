%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project Declarative Programming %
%           Jens Nevens           %
%								  %
%           cost(+S,?C)           %
%       violates_sc(+S, -SC)      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cost, [cost/2,
				 violates_sc/2]).

%:- use_module(small_instance).
%:- use_module(large_short_instance).
%:- use_module(large_long_instance).

:- use_module(utils).

% cost(+Scedule, ?Cost)
%		+Schedule: schedule/1 functor with list of
%			event/4 functors
%		?Cost: Total cost of given Schedule
%
%		Computed total cost and violated constraints
%		but returns only the former, or checks whether
%		they can be unified.
cost(Schedule, Cost) :-
	ccost(Schedule, Cost, _).

% violates_sc(+Scedule, -Violates)
%		+Schedule: schedule/1 functor with list of
%			event/4 functors
%		-Violates: All soft constraints violated by
%			the given Schedule.
%
%		Computes total cost and violated constraints
%		but only returns the latter.
violates_sc(Schedule, Violates) :-
	ccost(Schedule, _, Violates).

% ccost(+Schedule, ?Cost, -Violates)
%		+Schedule: schedule/1 functor that contains list
%			of event/4 functors.
%		?Cost: Total cost of given Schedule
%		-Violates: Soft constraints violated by Schedule
%
%		First, preprocessing is done. Afterwards, all costs
%		and violated constraints for lecturer and students
%		are computed seperately. Finally, the total cost
%		is computed.
ccost(schedule(Events), Cost, Violates) :-
	findall(EID, exam(EID,_), ExamIDs),
	findall(LID, lecturer(LID,_), Lecturers),
	findall(SID, student(SID,_), Students),
	do_preprocess(ExamIDs),
	for_person(Lecturers, Events, LCost, LViolates),
	for_person(Students, Events, SCost, SViolates),
	length(Lecturers, LecturerSize),
	length(Students, StudentSize),
	Cost is ((LCost / LecturerSize) + (SCost / StudentSize)) / 2,
	append(SViolates, LViolates, Violates).

% for_person(+Persons, +Events, -Cost, -Violates)
%		+Persons: List of PID were PID is ID of student
%			or lecturer
%		+Events: List of event/4 functors
%		-Cost: Total penalty for this person
%		-Violates: All constraints violated by this
%			person
%
%		Loops through the list of Persons. When current
%		PID is a lecturer, the soft constraint
%		correction_time is calculated and all other
%		soft constraints are checked by for_event/4.
%		When PID is a student, soft constraint
%		study_time is calculated and other soft
%		constraint are checked by for_event/4. Uses
%		an accumulator to sum all costs and append 
%		all violated constraints.
for_person(Persons, Events, Cost, Violates) :-
	for_person_acc(Persons, Events, 0, [], Cost, Violates).

for_person_acc([], _, Cost, Violates, Cost, Violates).

for_person_acc([PID|RestPersons], Events, Cost0, Violates0, Cost, Violates) :-
	lecturer(PID,_),
	%
	for_event(PID, Events, EventCost, EventViolates),
	sort_functors_dec(3, Events, SortedEvents),
	last_day(LastDay),
	correction_time(PID, SortedEvents, LastDay, 0, DTL, CorrectionCost),
	NewCost is Cost0 + EventCost + CorrectionCost,
	append_constraint(PID, DTL, CorrectionCost, Violates0, CorrectionViolates),
	append(CorrectionViolates, EventViolates, NewViolates),
	!,
	for_person_acc(RestPersons, Events, NewCost, NewViolates, Cost, Violates).

for_person_acc([PID|RestPersons], Events, Cost0, Violates0, Cost, Violates) :-
	student(PID,_),
	%
	for_event(PID, Events, EventCost, EventViolates),
	sort_functors_asc(3, Events, SortedEvents),
	first_day(FirstDay),
	study_time(PID, SortedEvents, FirstDay, 0, DTL, StudyCost),
	NewCost is Cost0 + EventCost + StudyCost,
	append_constraint(PID, DTL, StudyCost, Violates0, StudyViolates),
	append(StudyViolates, EventViolates, NewViolates),
	!,
	for_person_acc(RestPersons, Events, NewCost, NewViolates, Cost, Violates).

% append_constraint(+PID, +DTL, +Cost, +Violates, -NewViolates)
%		+PID: ID of student or lecturer
%		+DTL: Amount of days to little to study or correct
%		+Cost: Penalty for study_time or correction_time
%		+Violates: List of violated constraints
%		-NewViolates: Extended list of violates constraints
%
%		When the DTL and Cost for study_time or correction_time
%		are 0, the list of violated constraints is not extended.
%		When DTL and Cost are different from 0, and PID is a
%		lecturer, then list of violated constraints is extended
%		with sc_correction_time/3 functor. In case were PID
%		is a student, list of violated constraints is extended
%		with sc_study_time/3 functor.
append_constraint(_, 0, 0, Violates, Violates).

append_constraint(PID, DTL, Cost, Violates, NewViolates) :-
	lecturer(PID, _),
	append([sc_correction_time(PID, DTL, Cost)], Violates, NewViolates),
	!.

append_constraint(PID, DTL, Cost, Violates, NewViolates) :-
	student(PID,_),
	append([sc_study_time(PID, DTL, Cost)], Violates, NewViolates),
	!.

% for_event(+PID, +Events, -Cost, -Violates)
%		+PID: ID of student or lecturer
%		+Events: List of event/4 functors
%		-Cost: Penalty of soft constraints
%		-Violates: Violated soft constraints
%
%		Loops through the list of Events. When person
%		with PID is linked to event, the following
%		soft constraints are checked: no_exam_in_period,
%		lunch_break, not_in_period, same_day and b2b.
%		The cost of all these constraints are added and
%		the violated constraints are appended using an
%		accumulator.
for_event(PID, Events, Cost, Violates) :-
	for_event_acc(PID, Events, 0, [], Cost, Violates).

for_event_acc(_, [], Cost, Violates, Cost, Violates).

for_event_acc(PID, [event(EID,RID,Day,Start)|RestEvents], Cost0, Violates0, Cost, Violates) :-
	link_to_exam(PID,EID),
	no_exam_in_period(PID, event(EID,RID,Day,Start), Cost1, Violates1),
	lunch_break(PID, event(EID,RID,Day,Start), Cost2, Violates2),
	not_in_period(PID, event(EID,RID,Day,Start), Cost3, Violates3),
	same_day(PID, event(EID,RID,Day,Start), RestEvents, Cost4, Violates4),
	b2b(PID, event(EID,RID,Day,Start), RestEvents, Cost5, Violates5),
	NewCost is Cost0 + Cost1 + Cost2 + Cost3 + Cost4 + Cost5,
	append(Violates1, Violates0, V1),
	append(Violates2, V1, V2),
	append(Violates3, V2, V3),
	append(Violates4, V3, V4),
	append(Violates5, V4, NewViolates),
	!,
	for_event_acc(PID, RestEvents, NewCost, NewViolates, Cost, Violates).

for_event_acc(PID, [_|RestEvents], Cost0, Violates0, Cost, Violates) :-
	for_event_acc(PID, RestEvents, Cost0, Violates0, Cost, Violates).

% correction_time(+PID, +Events, +LastDay, +CorrectionDays, -DTL, -Cost)
%		+PID: ID of student or lecturer
%		+Events: Sorted list of event/4 functors
%			Sorted descending on Day.
%		+LastDay: Day of previously encountered exam.
%			Initialized with last day of exam period
%		+CorrectionDays: Number of days person with PID
%			has left to correct exams. Initialized
%			with 0.
%		-DTL: Number of days lecturer has too little
%			to correct
%		-Cost: Total penalty. This equal DTL * Penalty
%
%		Checks for each event in Events whether the lecturer
%		with PID is linked to this event. When this is the case,
%		the number of days this lecturer has available to correct
%		exams is computed and the number of days this lecturer
%		needs to correct is queried. Delta is the difference.
%		When Delta is negative, the lecturer has not enough time
%		to correct. When this is zero or positive, the lecturer
%		does have enough time to correct exams. Then, the next
%		event in the list is studied. LastDay is replaced by the
%		day of the current event and CorrectionDays is replaced
%		by the number of days the lecturer has left to correct.
%		Since the events are sorted descending, these days left
%		can be used to correct to next exam. At every loop the 
%		Cost is incremented with Penalty * DTL. 
correction_time(PID, Events, LastDay, CorrectionDays, DTL, Cost) :-
	correction_time_acc(PID, Events, LastDay, CorrectionDays, 0, 0, DTL, Cost).

correction_time_acc(_, [], _, _, DTL, Cost, DTL, Cost).

correction_time_acc(PID, [event(EID,_,Day,_)|RestEvents], LastDay, CorrectionDays, DTL0, Cost0, DTL, Cost) :-
	has_exam(CID,EID),
	teaches(PID,CID),
	%
	DaysAvailable is CorrectionDays + (LastDay - Day), 
	sc_correction_time(EID, DaysNeeded), 
	Delta is DaysAvailable - DaysNeeded, 
	DaysLeft is max(0, Delta),
	PenaltyDays is min(0,Delta), 
	sc_correction_penalty(PID, Penalty),
	NewCost is Cost0 + (Penalty * abs(PenaltyDays)),
	NewDTL is DTL0 + abs(PenaltyDays),
	!,
	correction_time_acc(PID, RestEvents, Day, DaysLeft, NewDTL, NewCost, DTL, Cost).

correction_time_acc(PID, [_|RestEvents], LastDay, CorrectionDays, DTL0, Cost0, DTL, Cost) :-
	correction_time_acc(PID, RestEvents, LastDay, CorrectionDays, DTL0, Cost0, DTL, Cost).

% study_time(+PID, +Events, +FirstDay, +StudyDays, -DTL, -Cost)
%		+PID: ID of student or lecturer
%		+Events: Sorted list of event/4 functors.
%			Sorted ascending on Day.
%		+FirstDay: Day of previously encountered exam.
%			Initialized with first day of exam period
%		+StudyDays: Number of days person with PID
%			has left to study. Initialized with 0.
%		-DTL: Number of days student has too little
%			to study
%		-Cost: Total penalty. This equals DTL * Penalty
%
%		Checks for each event in Events whether the student
%		with PID is linked to this event. When this is the case,
%		the number of days this student has available to study
%		is computed and the number of days this student needs
%		to study is queried. Delta is the difference of these 2.
%		When Delta is negative, the student has not enough time
%		to study. When this is zero or positive, the student has
%		enough time to study. Then, the next event in the list is
%		studied. FirstDay is replaced by the Day of the previously
%		encountered exam and StudyDays is replaced by the number 
%		of days the student has left to study. Since the events are
%		sorted ascending, these days left can be used for the
%		next exam. Each time, the Cost incremented with Penalty * DTL.
study_time(PID, Events, FirstDay, StudyDays, DTL, Cost) :-
	study_time_acc(PID, Events, FirstDay, StudyDays, 0, 0, DTL, Cost).

study_time_acc(_, [], _, _, DTL, Cost, DTL, Cost).

study_time_acc(PID, [event(EID,_,Day,_)|RestEvents], FirstDay, StudyDays, DTL0, Cost0, DTL, Cost) :-
	has_exam(CID,EID),
	follows_course(CID, Students),
	member(PID, Students),
	%
	DaysAvailable is StudyDays + (Day - FirstDay),
	sc_study_time(EID, DaysNeeded),
	Delta is DaysAvailable - DaysNeeded,
	DaysLeft is max(0,Delta),
	PenaltyDays is min(0,Delta),
	sc_study_penalty(PID,Penalty),
	NewCost is Cost0 + (Penalty * abs(PenaltyDays)),
	NewDTL is DTL0 + abs(PenaltyDays),
	!,
	study_time_acc(PID, RestEvents, Day, DaysLeft, NewDTL, NewCost, DTL, Cost).

study_time_acc(PID, [_|RestEvents], FirstDay, StudyDays, DTL0, Cost0, DTL, Cost) :-
	study_time_acc(PID, RestEvents, FirstDay, StudyDays, DTL0, Cost0, DTL, Cost).

% no_exam_in_period(+PID, +Event, -Cost, -Violates)
%		+PID: ID of student or lecturer
%		+Event: event/4 functor
%		-Cost: Resulting penalty
%		-Violates: Violated constraints
%
%		When Event falls in period not preferred by
%		person with PID, Cost=Penalty and Violates=
%		sc_no_exam_in_period/5 functor. Note some extra
%		arithmetic is needed since between(A,B,C) returns
%		true when A =< C =< B, while we want A =< C < B
%		in case of the end-hour and A < C =< B in case of
%		the start hour. When constraint is not violated
%		Cost=0 and Violates=[]
no_exam_in_period(PID, event(_,_,Day,Start), Penalty, [sc_no_exam_in_period(PID,Day,From,Till,Penalty)]) :-
	sc_no_exam_in_period(PID,Day,From,Till,Penalty),
	TillMin is Till - 1,
	between(From,TillMin,Start).

no_exam_in_period(PID, event(EID,_,Day,Start), Penalty, [sc_no_exam_in_period(PID,Day,From,Till,Penalty)]) :-
	sc_no_exam_in_period(PID,Day,From,Till,Penalty),
	duration(EID,Duration),
	End is Start + Duration,
	FromPlus is From + 1,
	between(FromPlus,Till,End).

no_exam_in_period(_,_,0,[]).

% lunch_break(+PID, +Event, -Cost, -Violates)
%		+PID: ID of student or lecturer
%		+Event: event/4 functor
%		-Cost: Resulting penalty
%		-Violates: Violated constraints
%
%		When Event falls during lunch break and
%		person with PID does not prefer this, 
%		Cost=Penalty and Violates=sc_lunch_break/2
%		functor. When this is not the case, Cost=0
%		and Violates=[]
lunch_break(PID, event(EID,_,_,Start), Penalty, [sc_lunch_break(PID,EID,Penalty)]) :-
	Start == 12,
	sc_lunch_break(PID,Penalty).

lunch_break(PID, event(EID,_,_,Start), Penalty, [sc_lunch_break(PID,EID,Penalty)]) :-
	duration(EID,Duration),
	End is Start + Duration,
	End == 13,
	sc_lunch_break(PID,Penalty).

lunch_break(_,_,0,[]).

% not_in_period(+PID, +Event, -Cost, -Violates)
%		+PID: ID of student or lecturer
%		+Event: event/4 functor
%		-Cost: Resulting penalty
%		-Violates: Violated constraints
%
%		When Event falls within period not preferred by
%		person PID, Cost=Penalty and Violates=
%		sc_not_in_period/6 functor. Note some extra
%		arithmetic is needed since between(A,B,C) returns
%		true when A =< C =< B, while we want A =< C < B
%		in case of the end-hour and A < C =< B in case of
%		the start hour. When constraint is not violated,
%		Cost=0 and Violates=[].
not_in_period(PID, event(EID,_,Day,Start), Penalty, [sc_not_in_period(PID,EID,Day,From,Till,Penalty)]) :-
	sc_not_in_period(PID,EID,Day,From,Till,Penalty),
	TillMin is Till - 1,
	between(From,TillMin,Start).

not_in_period(PID, event(EID,_,Day,Start), Penalty, [sc_not_in_period(PID,EID,Day,From,Till,Penalty)]) :-
	sc_not_in_period(PID,EID,Day,From,Till,Penalty),
	duration(EID,Duration),
	End is Start + Duration,
	FromPlus is From + 1,
	between(FromPlus,Till,End).

not_in_period(_, _, 0, []).

% same_day(+PID, +Event, +RestEvents, -Cost, -Violates)
%		+PID: ID of student or lecturer
%		+Event: event/4 functor
%		+RestEvents: List of event/4 functors
%		-Cost: Resulting penalty
%		-Violates: Violated constraints
%
%		Loops over RestEvents. When Event and element of
%		RestEvents are on the same day, Cost is incremented
%		with a penalty and an sc_same_day/4 functor is
%		appended to Violates. When this condition is not met
%		the next element in the list is checked. This uses an
%		accumulator.
same_day(PID, Event, OtherEvents, Cost, Violates) :-
	same_day_acc(PID, Event, OtherEvents, 0, [], Cost, Violates).

same_day_acc(_, _, [], Cost, Violates, Cost, Violates).

same_day_acc(PID, event(EID,RID,Day,Start), [event(EID2,_,Day,_)|RestEvents], Cost0, Violates0, Cost, Violates) :-
	link_to_exam(PID, EID2),
	%
	sc_same_day(PID,Penalty),
	NewCost is Cost0 + Penalty,
	append([sc_same_day(PID, EID2, EID, Penalty)], Violates0, NewViolates),
	!,
	same_day_acc(PID, event(EID,RID,Day,Start), RestEvents, NewCost, NewViolates, Cost, Violates).

same_day_acc(PID, event(EID,RID,Day,Start), [_|RestEvents], Cost0, Violates0, Cost, Violates) :-
	same_day_acc(PID, event(EID,RID,Day,Start), RestEvents, Cost0, Violates0, Cost, Violates).

% b2b(+PID, +Event, +RestEvents, -Cost, -Violates)
%		+PID: ID of student or lecturer
%		+Event: event/4 functor
%		+RestEvents: List of event/4 functors
%		-Cost: Resulting penalty
%		-Violates: Violated constraints
%
%		Loops over RestEvents. When Event and element of
%		RestEvents are back 2 back, i.e. the same day and
%		consecutively, Cost is incremented with a penalty
%		and an sc_b2b/4 functor is appended to Violates.
%		When the conditions are not met, next element in
%		the list is checked. This uses an accumulator.
b2b(PID, Event, OtherEvents, Cost, Violates) :-
	b2b_acc(PID, Event, OtherEvents, 0, [], Cost, Violates).

b2b_acc(_, _, [], Cost, Violates, Cost, Violates).

b2b_acc(PID, event(EID,RID,Day,Start1), [event(EID2,_,Day,Start2)|RestEvents], Cost0, Violates0, Cost, Violates) :-
	duration(EID,Duration),
	Start2 is Start1 + Duration,
	link_to_exam(PID, EID2),
	%
	sc_b2b(PID,Penalty),
	NewCost is Cost0 + Penalty,
	append([sc_b2b(PID,EID2,EID,Penalty)], Violates0, NewViolates),
	!,
	b2b_acc(PID, event(EID,RID,Day,Start1), RestEvents, NewCost, NewViolates, Cost, Violates).

b2b_acc(PID, event(EID,RID,Day,Start1), [event(EID2,_,Day,Start2)|RestEvents], Cost0, Violates0, Cost, Violates) :-
	duration(EID2,Duration),
	Start1 is Start2 + Duration,
	link_to_exam(PID, EID2),
	%
	sc_b2b(PID,Penalty),
	NewCost is Cost0 + Penalty,
	append([sc_b2b(PID,EID2,EID,Penalty)], Violates0, NewViolates),
	!,
	b2b_acc(PID, event(EID,RID,Day,Start1), RestEvents, NewCost, NewViolates, Cost, Violates).

b2b_acc(PID, event(EID,RID,Day,Start), [_|RestEvents], Cost0, Violates0, Cost, Violates) :-
	b2b_acc(PID, event(EID,RID,Day,Start), RestEvents, Cost0, Violates0, Cost, Violates).














