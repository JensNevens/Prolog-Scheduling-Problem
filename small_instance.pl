:- module(small_instance, [lecturer/2,
						   student/2,
						   course/2,
						   exam/2,
						   room/2,
						   has_exam/2,
						   duration/2,
						   follows/2,
						   teaches/2,
						   capacity/2,
						   first_day/1,
						   last_day/1,
						   availability/4,
						   sc_lunch_break/2,
						   sc_b2b/2,
						   sc_same_day/2,
						   sc_no_exam_in_period/5,
						   sc_not_in_period/6,
						   sc_correction_time/2,
						   sc_correction_penalty/2,
						   sc_study_time/2,
						   sc_study_penalty/2]).

%Allow grouping constraints per students/lecturers
:- discontiguous
	 sc_lunch_break/2,
	 sc_not_in_period/6,
	 sc_same_day/2,
	 sc_b2b/2.

lecturer(l1,'Mr John').
lecturer(l2,'Mr Francis').
lecturer(l3,'Mr Josef').
lecturer(l4,'Ms Ann').

student(s1,'Anna').
student(s2,'Max').
student(s3,'Bill').
student(s4,'Carla').

course(c1,'Math').
course(c2,'Science & Technology').
course(c3,'Philosophy').
course(c4,'Religion').
course(c5,'English').

exam(e1,'Math').
exam(e2,'Science & Technology').
exam(e3,'Philosophy').
exam(e4,'Religion').
exam(e5,'English').

room(r1,'Small room').
room(r2,'Large room').

has_exam(c1,e1).
has_exam(c2,e2).
has_exam(c3,e3).
has_exam(c4,e4).
has_exam(c5,e5).

duration(Exam,2) :- exam(Exam,_). %every exam takes 2 hours

follows(Student,c1) :- student(Student,_). %every student follows Math
follows(Student,c2) :- student(Student,_). %every student follows Science & Technology
follows(Student,c5) :- student(Student,_). %every student follows Languages
follows(s2,c3). %Max follows philosophy
follows(s3,c3). %Bill follows philosophy
follows(s1,c4). %Anna follows religion
follows(s4,c4). %Carla follows religion

teaches(l1,c1).
teaches(l1,c2).
teaches(l2,c3).
teaches(l3,c4).
teaches(l4,c5).

capacity(r1,2).
capacity(r2,4).

%first and last day of exam period
first_day(1).
last_day(5).

%Rooms are available
availability(Room,1,10,12) :- room(Room,_). %Day 1, all rooms are available from 10 to 12
availability(Room,2,10,12) :- room(Room,_). %Day 2, all rooms are available from 10 to 12
availability(Room,3,10,15) :- room(Room,_). %Day 3, all rooms are available from 10 to 15
availability(Room,4,10,12) :- room(Room,_). %Day 4, all rooms are available from 10 to 12
availability(Room,5,10,12) :- room(Room,_). %Day 5, all rooms are available from 10 to 12

%soft-constraints
%lecturer
sc_lunch_break(L,1) :- lecturer(L,_). %lecturers prefer a lunchbreak
sc_b2b(L,2) :- lecturer(L,_). %lecturers prefer not to have exams back 2 back
sc_no_exam_in_period(l3,3,0,24,5). %Josef prefers no exams at day 3
sc_no_exam_in_period(l4,Day,0,12,1) :- first_day(FirstDay),last_day(LastDay),between(FirstDay,LastDay,Day). %Ann prefers no exams before noon
sc_no_exam_in_period(l1,Day,14,24,5) :- first_day(FirstDay),last_day(LastDay),between(FirstDay,LastDay,Day). %John prefers no exams after 14h
sc_not_in_period(l1,e2,1,0,24,3). %Science & technology preferably not day 1
sc_correction_time(e1,2).
sc_correction_time(e2,1).
sc_correction_time(e3,1).
sc_correction_time(e4,1).
sc_correction_time(e5,2).
sc_correction_penalty(L,3) :- lecturer(L,_). %guarantee enough correction time

%student
sc_lunch_break(S,1) :- student(S,_). %students prefer a lunchbreak
sc_same_day(S,2) :- student(S,_). %students prefer not to have multiple exams on the same day
sc_b2b(S,5) :- student(S,_). %students prefer not to have exams back 2 back
sc_study_time(e1,2). 
sc_study_time(e2,1).
sc_study_time(e3,1).
sc_study_time(e4,1).
sc_study_time(e5,1).
sc_study_penalty(S,3) :- student(S,_). %guarantee enough study time





