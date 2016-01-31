%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project Declarative Programming %
%           Jens Nevens           %
%								  %
%         pretty_print(+S)        %
%      pretty_print(+SID, +S)     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(print, [pretty_print/1,
				  pretty_print/2]).

:- use_module(small_instance).
%:- use_module(large_short_instance).
%:- use_module(large_long_instance).

:- use_module(utils).

% pretty_print(+SID, +Schedule)
%		+SID: Student ID
%		+Schedule: schedule/1 functor that contains a list
%			of event/4 functors.
%
%		The events in the given schedule are filtered by 
%		filter_events/3, based on the given SID. The 
%		resulting list is passed to print_day/1, via findall, 
%		so all alternatives are tried.
pretty_print(PID, schedule(Events)) :-
	filter_events(Events, PID, FilteredEvents),
	findall(_, print_day(FilteredEvents), _).

% filter_events(+Events, +SID, -FilteredEvents)
%		+Events: List of event/4 functors
%		+SID: Student ID
%		-FilteredEvents: +Events filtered by +SID
%
%		Loops through the list of given events and add all 
%		elements of Events to FilteredEvents when the student 
%		with SID follows the course associated with Event.
filter_events(Events, PID, Filtered) :-
	filter_events_acc(Events, PID, [], Filtered).

filter_events_acc([], _, Filtered, Filtered).

filter_events_acc([event(EID,RID,Day,Start)|RestEvents], PID, Filtered0, Filtered) :-
	student(PID,_),
	has_exam(CID,EID),
	findall(SID, follows(SID,CID), Students),
	member(PID, Students),
	append([event(EID,RID,Day,Start)], Filtered0, NewFiltered),
	!,
	filter_events_acc(RestEvents, PID, NewFiltered, Filtered).

filter_events_acc([event(EID,RID,Day,Start)|RestEvents], PID, Filtered0, Filtered) :-
	lecturer(PID,_),
	has_exam(CID,EID),
	teaches(PID,CID),
	append([event(EID,RID,Day,Start)], Filtered0, NewFiltered),
	!,
	filter_events_acc(RestEvents, PID, NewFiltered, Filtered).

filter_events_acc([_|RestEvents], PID, Filtered0, Filtered) :-
	filter_events_acc(RestEvents, PID, Filtered0, Filtered).

% pretty_print(+Schedule)
%		+Schedule: schedule/1 functor that contains a list of
%			event/4 functors.
%
%		The events are passed to print_day/1
%		via findall, so all alternatives are tried. This means
%		that the events are grouped together based on all possible
%		values for Days.
pretty_print(schedule(Events)) :-
	findall(_, print_day(Events), _).

% print_day(+Events)
%		+Events: List of event/4 functors
%
%		The setof groups the given events together
%		based on Day. RID and Start are not bound in the member-predicate,
%		but are bound in the template. The Day is printed to output
%		and the resulting list is passed to print_event/1. Since this is
%		done via findall, all alternatives are tried, i.e. for each day,
%		the resulting list of events is grouped on all possible rooms
%		that still occur in the list.
print_day(Events) :-
	setof(event(EID,RID,_,Start), RID^Start^member(event(EID,RID,Day,Start),Events), Result),
	format("~n ~n*** DAY ~d *** ~n ~n", Day),
	findall(_, print_event(Result), _).

% print_event(+Events)
%		+Events: List of event/4 functors
%
%		The setof groups the given events together
%		based on RID. Start is not bound in the member-predicate,
%		but is bound in the template. Afterwards, the name of 
%		the room is printed to output, the resulting list is 
%		sorted based on Start-hour and passed on to print/1.
print_event(Events) :-
	setof(event(EID,_,Day,Start), Start^member(event(EID,RID,Day,Start),Events), Result),
	room(RID, RoomName),
	format("~a:~n", RoomName),
	sort_functors_asc(4, Result, Sorted),
	print(Sorted).

% print(+Events)
%		+Events: List of event/4 functors
%
%		Loops through the list of given events
%		gets extra information about the event, such as
%		exam name, lecturer name, ..., and prints this
%		information to output using format.
print([]).

print([event(EID,_,_,Start)|RestEvents]) :-
	exam(EID,ExamName),
	duration(EID,Duration),
	End is Start + Duration,
	has_exam(CID,EID),
	teaches(LID,CID),
	lecturer(LID, LecturerName),
	format("~d:00 - ", Start),
	format("~d:00: ", End),
	format("~a ", ExamName),
	format("(~a).~n", LecturerName),
	print(RestEvents).
