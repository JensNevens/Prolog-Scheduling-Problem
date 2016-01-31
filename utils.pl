%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project Declarative Programming %
%           Jens Nevens           %
%								  %
%			   utils              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(utils, [do_preprocess/1,
				  list_overlap/2,
				  delete_first/3,
				  clean/0,
				  sort_functors_asc/3,
				  sort_functors_dec/3,
				  follows_course/2,
				  student_overlap/2,
				  teacher_overlap/2,
				  preprocess_done/0,
				  link_to_exam/2]).

%:- use_module(small_instance).
%:- use_module(large_short_instance).
%:- use_module(large_long_instance).

:- dynamic follows_course/2.
:- dynamic student_overlap/2.
:- dynamic teacher_overlap/2.
:- dynamic preprocess_done/0.

% do_preprocess(+ExamIDs)
%		+ExamIDs: IDs of all exams
%
%		Invokes preprocessing. This executes and asserts
%		predicates which are used a lot later on. When
%		preprocess is done, the preprocess_done/0 predicate
%		is asserted, so it is only done once.
do_preprocess(_) :- 
	preprocess_done, 
	!.

do_preprocess(ExamIDs) :-
	preprocess(ExamIDs),
	assert(preprocess_done),
	!.

% preprocess(+ExamIDs)
%		+ExamIDs: IDs of all exams
%
%		Does the actual preprocessing. Three predicates
%		are asserted: follows_course/2, teacher_overlap/2
%		and student_overlap/2. This predicate loops through
%		the list of exam IDs and calls has_teacher_overlap/2
%		and has_student_overlap/3 for each CID associated
%		with the EID.
preprocess([]).

preprocess([EID|RestExams]) :-
	has_exam(CID,EID),
	findall(SID, follows(SID,CID), Students),
	asserta(follows_course(CID,Students)),
	has_teacher_overlap(CID, RestExams),
	has_student_overlap(CID, Students, RestExams),
	preprocess(RestExams).

% has_teacher_overlap(+CID, +EIDs)
%		+CID: Course ID
%		+EIDs: List of Exam IDs
%
%		Checks if the lecturer of course CID is
%		associated with any of the courses linked
%		to the exams in EIDs. If yes, teacher_overlap/2
%		is asserted.
has_teacher_overlap(_, []).

has_teacher_overlap(CID, [OtherEID|RestExams]) :-
	has_exam(OtherCID, OtherEID),
	teaches(LID, CID),
	teaches(LID, OtherCID),
	asserta(teacher_overlap(CID, OtherCID)),
	asserta(teacher_overlap(OtherCID, CID)),
	has_teacher_overlap(CID,RestExams).
	
has_teacher_overlap(CID, [_|RestExams]) :-
	has_teacher_overlap(CID, RestExams).

% has_student_overlap(+CID, +Students, +EIDs)
%		+CID: Course ID
%		+Students: Students following this course
%		+EIDs: List of Exam IDs
%
%		Checks if any of the Students following course
%		with CID are associated with any of the courses
%		linked to the exam in EIDs. If yes, student_overlap/2
%		is asserted.
has_student_overlap(_, _, []).

has_student_overlap(CID, Students, [OtherEID|RestExams]) :-
	has_exam(OtherCID,OtherEID),
	findall(SID, follows(SID,OtherCID), OtherStudents),
	list_overlap(Students, OtherStudents),
	asserta(student_overlap(CID, OtherCID)),
	asserta(student_overlap(OtherCID,CID)),
	has_student_overlap(CID, Students, RestExams).
	
has_student_overlap(CID, Students, [_|RestExams]) :-
	has_student_overlap(CID, Students, RestExams).

% list_overlap(+List1, +List2)
%		+List1: Any list
%		+List2: Any list
%
%		Checks if any member of List1 is also a
%		member of List2. When a member is found,
%		no backtracking is needed. It could also find
%		the other members, but this is not necessary.
list_overlap([],_) :- 
	false.

list_overlap([First|_],OtherList) :-
	member(First, OtherList),
	!.

list_overlap([_|Rest],OtherList) :-
	list_overlap(Rest,OtherList).

% clean
%		Retract all predicates asserted
%		by the preprocessing
clean :-
	retractall(follows_course(_,_)),
	retractall(student_overlap(_,_)),
	retractall(teacher_overlap(_,_)),
	retractall(preprocess_done).

% delete_first(?Elem, ?List, ?ResultList)
%		?Elem: Any ground term
%		?List: Any List
%		?ResultList: Same as List but without Elem
%
%		ResultList is the same as List but with the
%		first occurence of Elem removed.
delete_first(E,[E|T],T) :- !.

delete_first(E,[H|T1],[H|T2]) :- 
	delete_first(E,T1,T2).

% sort_functors_asc(+Arg, +Functors, -SortedFunctors)
% sort_functors_dec(+Arg, +Functors, -SortedFunctors)
%		+Arg: i'th element of functor on which to sort
%		+Functors: List of functors
%		-SortedFunctors: List where Functors is sorted
%			based on the Arg'th element of the functor.
%
%		Sorts list on functors Functors based on the
%		Arg'th element of the functor. Results in
%		SortedFunctors. Duplicates are kept.
sort_functors_asc(Arg, Functors, SortedFunctors) :-
	sort(Arg, @=<, Functors, SortedFunctors).

sort_functors_dec(Arg, Functors, SortedFunctors) :-
	sort(Arg, @>=, Functors, SortedFunctors).

% link_to_exam(+PID, +EID)
%		+PID: ID of student or lecturer
%		+EID: ID of exam
%
%		Checks whether person with PID is linked to exam
%		with EID. If PID is a lecturer, PID needs to teach
%		CID associated with EID. If PID is a student, 
%		PID needs to follow course CID associated with EID.
link_to_exam(PID, EID) :-
	lecturer(PID,_),
	has_exam(CID,EID),
	teaches(PID,CID),
	!.

link_to_exam(PID, EID) :-
	student(PID,_),
	has_exam(CID,EID),
	follows_course(CID, Students),
	member(PID, Students),
	!.

link_to_exam(_, _) :- 
	fail.






