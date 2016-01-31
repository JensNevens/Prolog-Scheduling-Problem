%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project Declarative Programming %
%           Jens Nevens           %
%								  %
%			is_valid(?S)          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(valid, [is_valid/1,
	              conflict/2]).

%:- use_module(small_instance).
%:- use_module(large_short_instance).
%:- use_module(large_long_instance).

:- use_module(utils).

% is_valid(?Schedule)
%		?Schedule: A schedule/1 functor that contains a
%			list of event/4 functors.
%
%		is_valid/1 will trigger preprocessing and then call
%		is_valid/2. This will return a valid schedule if
%		?Schedule was not bound. If ?Schedule was bound, it
%		will return true or false.
is_valid(schedule(Events)) :- 
	findall(EID, exam(EID,_), ExamIDs),
	do_preprocess(ExamIDs),
	is_valid(schedule(Events), ExamIDs).

% is_valid(?Schedule, +ExamIDs)
%		?Schedule: A schedule/1 functor that contains a
%			list of event/4 functors.
%		+ExamIDs: IDs of all exams
%
%		is_valid/2 will loop through all ExamIDs. If ?Schedule
%		is bound, it will check if the room is large enough, if
%		the room is available during the exam and if there are 
%		no conflicts with other (already checked) exams. If ?Schedule
%		is not bound, it will generate exams in this manner. is_valid/2
%		can only end when all exams are scheduled.
is_valid(schedule([]), []).

is_valid(schedule([event(EID,RID,Day,Start)|RestEvents]), ExamIDs) :-
	delete_first(EID,ExamIDs,NewExamIDs),
	is_valid(schedule(RestEvents), NewExamIDs),
	exam(EID,_),
	has_exam(CID,EID),
	follows_course(CID,Students),
	length(Students, Size),
	room(RID,_),
	capacity(RID,Capacity),
	Capacity >= Size,
	availability(RID,Day,StartHour,EndHour),
	between(StartHour,EndHour,Start),
	duration(EID,Duration),
	End is Start + Duration,
	End =< EndHour,
	not(conflict(event(EID,RID,Day,Start), RestEvents)).

% conflict(+Event, +OtherEvents)
%		+Event: An event/4 functor
%		+OtherEvents: List of event/4 functors
%
%		conflict/2 will loop through the list of OtherEvents
%		and check if Event conflicts with these. There is a
%		conflict when two exams are on the same day in the 
%		same room, on the same day and with overlapping hours
%		and overlapping students or on the same day and with
%		overlapping hours and overlapping lecturer.
conflict(_, []) :-
	fail.

conflict(event(EID1,RID,Day,Start1), [event(EID2,RID,Day,Start2)|_]) :-
	collision(EID1, Start1, EID2, Start2).

conflict(event(EID1,_,Day,Start1), [event(EID2,_,Day,Start2)|_]) :-
	has_exam(CID1,EID1),
	has_exam(CID2,EID2),
	student_overlap(CID1,CID2),
	collision(EID1, Start1, EID2, Start2).

conflict(event(EID1,_,Day,Start1), [event(EID2,_,Day,Start2)|_]) :-
	has_exam(CID1,EID1),
	has_exam(CID2,EID2),
	teacher_overlap(CID1,CID2),
	collision(EID1, Start1, EID2, Start2).

conflict(Event, [_|RestEvents]) :-
	conflict(Event,RestEvents).

% collision(+EID1, +Start1, +EID2, +Start2)
%		+EID1, +EID2: Exam IDs
%		+Start1, +Start2: Start hours of these EIDs
%
%		collision/4 will check if exams EID1 and EID2 overlap.
%		This can be in 2 cases: Start2 < Start1 =< End2
%		or Start2 =< End1 < End2
collision(_, Start1, EID2, Start2) :-
	duration(EID2, Duration2),
	End2 is (Start2 + Duration2) - 1,
	between(Start2, End2, Start1).

collision(EID1, Start1, EID2, Start2) :-
	duration(EID1, Duration1),
	duration(EID2, Duration2),
	End1 is Start1 + Duration1,
	End2 is Start2 + Duration2,
	StartPlus is Start2 + 1,
	between(StartPlus, End2, End1).














