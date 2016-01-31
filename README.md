# Exam Scheduling Problem

## Note

This project is part of the course Declarative Programming taught at Vrije Universiteit Brussel

## The Exam Timetabling Problem

The examination timetabling problem can be stated as follows: Where and when to schedule which exams, during the examination period? Making a good schedule is challenging due to the large number of students enrolled, often with individual programs, lecturers teaching multiple courses and the limited number and capacity of rooms. Furthermore, both lecturers and students have (often conflicting) preferences. E.g. While lecturers want sufficient correction time, students want more time to study. It therefore represents a major administrative activity for academic institutions. Partial automation of this task is an active research area.
 
## Problem Instances
To test your scheduler, problem instances are provided. Each defines a set of courses, their exams, lecturers teaching and students following them, as well as a set of rooms and their capacity/availabilities. Each specifies the following knowledge:

- A set of students: **student(SID,Name)**: A student with unique identifier 'SID' and name 'Name'.
 
- A set of lecturers: **lecturer(LID,Name)**: A lecturer with unique identifier 'LID' and name 'Name'.
 
- A set of courses: **course(CID,Name)**: A course with unique identifier 'CID' and name 'Name'.
 
- A set of exams: **exam(EID,Name)**: An exam with unique identifier 'EID' and name 'Name'.
 
- A set of rooms: **room(RID,Name)**: A room with unique identifier 'RID' and name 'Name'.
 
- Which course has which exams: **has_exam(CID,EID)**: The course with 'CID' has the exam with 'EID'. (Note: Each exam is related to exactly 1 course)
 
- Duration of each exam: **duration(EID,Duration)**: The exam with 'EID' takes 'Duration' hours.
 
- Students following a course: **follows(SID,CID)**: The student with 'SID' follows the course with 'CID'.
 
- Which lecturer teaches which courses: **teaches(LID,CID)**: The lecturer with 'LID' teaches the course with 'CID'. (Note: Each course is taught by exactly 1 lecturer)
 
- The capacity of rooms: **capacity(RID,Capacity)**: The room with 'RID' can facilitate at most 'Capacity' students.
 
- The first day of the study/exam period: **first_day(FirstDay)**.
 
- The last day of the correction/exam period: **last_day(LastDay)**.
 
- The availabilities of rooms: **availability(RID,Day,From,Till)**: The room with 'RID' is available day 'Day' from 'From' o'clock till 'Till' o'clock.
 
- The correction time required for each exam: **sc_correction_time(EID,Days)**: 'Days' days are required to correct the exam with 'EID'. (Note: The day of the exam excluded)
 
- The study time required for each exam: **sc_study_time(EID,Days)**: 'Days' days are required to study for the exam with 'EID'. (Note: The day of the exam excluded) The following table gives an overview of the instances provided:

| # | name | students | lecturers | courses | rooms | exam period length | optimal sq |
|---|------|----------|-----------|---------|-------|--------------------|------------|
| 1 | small       | 4   | 4  | 5  | 2 | 5 Days  | 1.875 |
| 2 | large_short | 100 | 19 | 34 | 3 | 9 Days  | ???   |
| 3	| large_long  | 100 | 19 | 34 | 3 | 23 Days | 0   |
 
## Hard Constraints
The following constraints must be met in order for a schedule to be admissable:

- All exams must be scheduled exactly once and start at the hour (e.g. 15:00 but not 15:30).
- Exams can only take place in a room that is available for the entire period of the exam (start to end) whose capacity exceeds or equals the number of students attending the exam (subscribed to the course).
- No 2 exams can take place at the same time 
-- in the same room 
-- if 1 or more students are subscribed to both courses (this includes multiple exams of the same course).
-- if the same lecturer teaches both courses.
 
## Soft Constraints
Soft constraints, unlike hard constraints, are not required for the schedule to be admissable. Rather, they are desirable (for lecturers, students or both). Each soft-constraint has a corresponding penalty, which is imposed when it is not met. One schedule is better than another if it has a lower sum of penalties. For the project we consider the following soft constraints:

- **sc_lunch_break(PID,Penalty)**: A penalty of 'Penalty' is imposed for each exam a Lecturer/Student with 'PID' has during lunch break (i.e. from 12 till 13 o'clock).
- **sc_no_exam_in_period(LID,Day,From,Till,Penalty)**: A penalty of 'Penalty' is imposed for each exam Lecturer with 'LID' has on day 'Day', (partially) in the period from 'From' o'clock till 'Till' o'clock.
- **sc_not_in_period(PID,EID,Day,From,Till,Penalty)**: A penalty of 'Penalty' is imposed if the exam with 'EID' is held on day 'Day', (partially) in the period from 'From' o'clock till 'Till' o'clock. It is a constraint of Person with 'PID'.
- **sc_same_day(PID,Penalty)**: A penalty of 'Penalty' is imposed for each pair of exams the person with 'PID' has on the same day. E.g. if the person has 3 exams on one day, 'Penalty' is imposed 3 times.
- **sc_b2b(PID,Penalty)**: A penalty of 'Penalty' is imposed for each pair of exams the person with 'PID' has back-to-back (same day and consecutively). E.g. if the person has 3 exams consecutively, 'Penalty' is imposed 2 times.
- **sc_correction_penalty(LID,Penalty)**: A penalty of 'Penalty' is imposed for each day the lecturer with 'LID' has too little to correct all his exams. Note that a lecturer can correct exams, the same day he has another exam (just not the exam itself). E.g. assume a lecturer has 2 exams in a 1-5 day exam period, each requiring 2 days to correct. If exams are held day X and Y, Z times 'Penalty' is imposed.

|   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| X | 1 | 1 | 1 | 1 | 1 | 2 | 2 | 2 | 2 | 3 | 3 | 3 | 4 | 4 | 5 |
| Y | 1 | 2 | 3 | 4 | 5 | 2 | 3 | 4 | 5 | 3 | 4 | 5 | 4 | 5 | 5 |
| Z | 0 | 0 | 0 | 1 | 2 | 1 | 1 | 1 | 2 | 2 | 2 | 2 | 3 | 3 | 4 |
- **sc_study_penalty(SID,Penalty)**: A penalty of 'Penalty' is imposed for each day the student with 'SID' has too little to study for all his exams. Note that a student can study for exams, the same day he has another exam (just for the exam itself). E.g. assume a student has 2 exams in a 1-5 day exam period, each requiring 2 days to study for. If exams are held day X and Y, Z times 'Penalty' is imposed.

|   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| X|	1|	1|	1|	1|	1|	2|	2|	2|	2|	3|	3|	3|	4|	4|	5|
| Y|	1|	2|	3|	4|	5|	2|	3|	4|	5|	3|	4|	5|	4|	5|	5|
| Z|	4|	3|	2|	2|	2|	3|	2|	1|	1|	2|	1|	0|	1|	0|	0|
 
