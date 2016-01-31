:- module(large_short_instance, [lecturer/2,
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

% Students:
student(s1,'Cythia Serpe').
student(s2,'Christeen Friedli').
student(s3,'Kacie Faragoza').
student(s4,'Yuonne Cormany').
student(s5,'Marilu Brauner').
student(s6,'Myrl Weader').
student(s7,'Jani Sinks').
student(s8,'Wyatt Enriguez').
student(s9,'Lamont Vanduyn').
student(s10,'Shawanda Vanwyngaarden').
student(s11,'Shizue Utecht').
student(s12,'Demarcus Mervyn').
student(s13,'Charline Predom').
student(s14,'Yukiko Chaffins').
student(s15,'Reynaldo Portlock').
student(s16,'Cory Sauerbry').
student(s17,'Esta Ciraulo').
student(s18,'Nicky Testolin').
student(s19,'Eliana Updike').
student(s20,'Larisa Bachicha').
student(s21,'Wesley Mederios').
student(s22,'Orpha Caballes').
student(s23,'Agripina Husak').
student(s24,'Jerrica Getzschman').
student(s25,'Britni Storks').
student(s26,'Dimple Cayetano').
student(s27,'Sydney Whitbeck').
student(s28,'Kaylee Palmateer').
student(s29,'Hope Lovick').
student(s30,'Bernarda Rosul').
student(s31,'Enriqueta Begeman').
student(s32,'Landon Plancarte').
student(s33,'Bonny Schwarm').
student(s34,'Joel Tiznado').
student(s35,'Echo Chillemi').
student(s36,'Ernestine Kadish').
student(s37,'Katelin Realmuto').
student(s38,'Erick Parman').
student(s39,'Sammy Qualey').
student(s40,'Linh Koppang').
student(s41,'Doris Honnen').
student(s42,'Agripina Shafto').
student(s43,'Aleshia Deines').
student(s44,'Lia Bandasak').
student(s45,'Lindsey Keens').
student(s46,'Wilmer Snerling').
student(s47,'Nelia Huie').
student(s48,'Corliss Jerman').
student(s49,'Margurite Hatake').
student(s50,'Eboni Harmeyer').
student(s51,'Richelle Cavaco').
student(s52,'Tommie Stipp').
student(s53,'Olen Verkuilen').
student(s54,'Sun Hase').
student(s55,'Robbie Hoschek').
student(s56,'Chara Coatsworth').
student(s57,'Breann Ozburn').
student(s58,'Jann Mcelhany').
student(s59,'Mandy Wharff').
student(s60,'Gregory Zarling').
student(s61,'Lavelle Benage').
student(s62,'Annice Booth').
student(s63,'Hannelore Stehly').
student(s64,'Dillon Russe').
student(s65,'Ricky Neblock').
student(s66,'Merrilee Tortelli').
student(s67,'Sade Jenks').
student(s68,'Enriqueta Haro').
student(s69,'Warren Polian').
student(s70,'Marybeth Jellison').
student(s71,'Salena Keidong').
student(s72,'Ilse Fontneau').
student(s73,'Annamaria Ganoung').
student(s74,'Haydee Burbano').
student(s75,'Angella Yundt').
student(s76,'Lenna Briski').
student(s77,'Garland Zakrajsek').
student(s78,'Geri Thoran').
student(s79,'Carola Hackbart').
student(s80,'Andrea Nwadiora').
student(s81,'Elease Weser').
student(s82,'Cynthia Antrobus').
student(s83,'Alberto Sumners').
student(s84,'Wilfredo Bassil').
student(s85,'Kattie Kierzewski').
student(s86,'Adolfo Mavai').
student(s87,'Eugenie Rigatti').
student(s88,'Shela Brisker').
student(s89,'Delena Groholski').
student(s90,'Inge Kassin').
student(s91,'Jerry Shinney').
student(s92,'Sueann Mitchem').
student(s93,'Cody Baadsgaard').
student(s94,'Calandra Berninger').
student(s95,'Maud Halwood').
student(s96,'Melina Hallford').
student(s97,'Alline Kluck').
student(s98,'Amira Trabucco').
student(s99,'Irena Quivers').
student(s100,'Dorene Winsley').

% Lecturers:
lecturer(l1,'Janis Bolls').
lecturer(l2,'Ana Falacco').
lecturer(l3,'Morgan Nosek').
lecturer(l4,'Glenn Janson').
lecturer(l5,'Bethel Kievit').
lecturer(l6,'Hong Fedde').
lecturer(l7,'Nicky Waltos').
lecturer(l8,'Zulma Derwin').
lecturer(l9,'Shae Mostowy').
lecturer(l10,'Faye Bakhshian').
lecturer(l11,'Delmer Zwilling').
lecturer(l12,'Shalon Stains').
lecturer(l13,'Colleen Raike').
lecturer(l14,'Wanda Griffins').
lecturer(l15,'Oswaldo Bardo').
lecturer(l16,'Jama Nguyn').
lecturer(l17,'Charleen Mellema').
lecturer(l18,'Adelina Gord').
lecturer(l19,'Madeleine Ryba').

% Courses:
course(c1,'Math 1.0').
course(c2,'Math 2.0').
course(c3,'Advanced Math 2.0').
course(c4,'Religion 1.0').
course(c5,'Religion 2.0').
course(c6,'Philosophy 1.0').
course(c7,'Philosophy 2.0').
course(c8,'History 1.0').
course(c9,'History 2.0').
course(c10,'Socio-Economic Initiation').
course(c11,'Politics & Sociology').
course(c12,'Psychology').
course(c13,'Art Initiation').
course(c14,'Art History').
course(c15,'Architecture').
course(c16,'Painting & Sculpture').
course(c17,'Music & Performing Art').
course(c18,'English 1.0').
course(c19,'English 2.0').
course(c20,'Advanced English 2.0').
course(c21,'French').
course(c22,'German').
course(c23,'Dutch').
course(c24,'Spanish').
course(c25,'Russian').
course(c26,'Chinese').
course(c27,'Latin').
course(c28,'Information & Communication Technology').
course(c29,'Science Initiation').
course(c30,'Biology').
course(c31,'Physics').
course(c32,'Chemistry').
course(c33,'Informatics').
course(c34,'Economy').

% Exams:
exam(e1,'Math 1.0').
exam(e2,'Math 2.0').
exam(e3,'Advanced Math 2.0').
exam(e4,'Religion 1.0').
exam(e5,'Religion 2.0').
exam(e6,'Philosophy 1.0').
exam(e7,'Philosophy 2.0').
exam(e8,'History 1.0').
exam(e9,'History 2.0').
exam(e10,'Socio-Economic Initiation').
exam(e11,'Politics & Sociology').
exam(e12,'Psychology').
exam(e13,'Art Initiation').
exam(e14,'Art History').
exam(e15,'Architecture').
exam(e16,'Painting & Sculpture').
exam(e17,'Music & Performing Art').
exam(e18,'English 1.0').
exam(e19,'English 2.0').
exam(e20,'Advanced English 2.0').
exam(e21,'French').
exam(e22,'German').
exam(e23,'Dutch').
exam(e24,'Spanish').
exam(e25,'Russian').
exam(e26,'Chinese').
exam(e27,'Latin').
exam(e28,'Information & Communication Technology').
exam(e29,'Science Initiation').
exam(e30,'Biology').
exam(e31,'Physics').
exam(e32,'Chemistry').
exam(e33,'Informatics').
exam(e34,'Economy').

% Rooms:
room(r1,'Small room').
room(r2,'Normal room').
room(r3,'Large room').

% Courses have exams:
has_exam(c1,e1).
has_exam(c2,e2).
has_exam(c3,e3).
has_exam(c4,e4).
has_exam(c5,e5).
has_exam(c6,e6).
has_exam(c7,e7).
has_exam(c8,e8).
has_exam(c9,e9).
has_exam(c10,e10).
has_exam(c11,e11).
has_exam(c12,e12).
has_exam(c13,e13).
has_exam(c14,e14).
has_exam(c15,e15).
has_exam(c16,e16).
has_exam(c17,e17).
has_exam(c18,e18).
has_exam(c19,e19).
has_exam(c20,e20).
has_exam(c21,e21).
has_exam(c22,e22).
has_exam(c23,e23).
has_exam(c24,e24).
has_exam(c25,e25).
has_exam(c26,e26).
has_exam(c27,e27).
has_exam(c28,e28).
has_exam(c29,e29).
has_exam(c30,e30).
has_exam(c31,e31).
has_exam(c32,e32).
has_exam(c33,e33).
has_exam(c34,e34).

% Exams have durations:
duration(e1,3).
duration(e2,3).
duration(e3,4).
duration(e4,2).
duration(e5,2).
duration(e6,2).
duration(e7,2).
duration(e8,2).
duration(e9,2).
duration(e10,2).
duration(e11,2).
duration(e12,2).
duration(e13,2).
duration(e14,2).
duration(e15,2).
duration(e16,2).
duration(e17,2).
duration(e18,3).
duration(e19,3).
duration(e20,4).
duration(e21,2).
duration(e22,2).
duration(e23,2).
duration(e24,2).
duration(e25,2).
duration(e26,2).
duration(e27,2).
duration(e28,2).
duration(e29,2).
duration(e30,2).
duration(e31,2).
duration(e32,2).
duration(e33,2).
duration(e34,2).

% Students follow courses:
follows(s1,c1).
follows(s2,c1).
follows(s3,c1).
follows(s6,c1).
follows(s9,c1).
follows(s10,c1).
follows(s13,c1).
follows(s15,c1).
follows(s16,c1).
follows(s17,c1).
follows(s19,c1).
follows(s20,c1).
follows(s21,c1).
follows(s22,c1).
follows(s25,c1).
follows(s26,c1).
follows(s27,c1).
follows(s30,c1).
follows(s31,c1).
follows(s33,c1).
follows(s36,c1).
follows(s46,c1).
follows(s47,c1).
follows(s50,c1).
follows(s51,c1).
follows(s55,c1).
follows(s56,c1).
follows(s58,c1).
follows(s64,c1).
follows(s67,c1).
follows(s68,c1).
follows(s69,c1).
follows(s77,c1).
follows(s79,c1).
follows(s80,c1).
follows(s82,c1).
follows(s83,c1).
follows(s84,c1).
follows(s85,c1).
follows(s86,c1).
follows(s91,c1).
follows(s92,c1).
follows(s94,c1).
follows(s96,c1).
follows(s97,c1).
follows(s99,c1).
follows(s4,c2).
follows(s5,c2).
follows(s11,c2).
follows(s28,c2).
follows(s29,c2).
follows(s32,c2).
follows(s35,c2).
follows(s37,c2).
follows(s38,c2).
follows(s39,c2).
follows(s42,c2).
follows(s44,c2).
follows(s52,c2).
follows(s54,c2).
follows(s57,c2).
follows(s59,c2).
follows(s61,c2).
follows(s71,c2).
follows(s74,c2).
follows(s75,c2).
follows(s78,c2).
follows(s88,c2).
follows(s89,c2).
follows(s90,c2).
follows(s100,c2).
follows(s7,c3).
follows(s8,c3).
follows(s12,c3).
follows(s14,c3).
follows(s18,c3).
follows(s23,c3).
follows(s24,c3).
follows(s34,c3).
follows(s40,c3).
follows(s41,c3).
follows(s43,c3).
follows(s45,c3).
follows(s48,c3).
follows(s49,c3).
follows(s53,c3).
follows(s60,c3).
follows(s62,c3).
follows(s63,c3).
follows(s65,c3).
follows(s66,c3).
follows(s70,c3).
follows(s72,c3).
follows(s73,c3).
follows(s76,c3).
follows(s81,c3).
follows(s87,c3).
follows(s93,c3).
follows(s95,c3).
follows(s98,c3).
follows(s1,c4).
follows(s2,c4).
follows(s6,c4).
follows(s15,c4).
follows(s17,c4).
follows(s19,c4).
follows(s21,c4).
follows(s22,c4).
follows(s26,c4).
follows(s30,c4).
follows(s31,c4).
follows(s33,c4).
follows(s47,c4).
follows(s56,c4).
follows(s58,c4).
follows(s68,c4).
follows(s80,c4).
follows(s82,c4).
follows(s83,c4).
follows(s86,c4).
follows(s91,c4).
follows(s92,c4).
follows(s94,c4).
follows(s99,c4).
follows(s4,c5).
follows(s5,c5).
follows(s7,c5).
follows(s11,c5).
follows(s14,c5).
follows(s23,c5).
follows(s28,c5).
follows(s29,c5).
follows(s32,c5).
follows(s34,c5).
follows(s37,c5).
follows(s38,c5).
follows(s39,c5).
follows(s41,c5).
follows(s42,c5).
follows(s44,c5).
follows(s45,c5).
follows(s49,c5).
follows(s53,c5).
follows(s54,c5).
follows(s57,c5).
follows(s59,c5).
follows(s61,c5).
follows(s62,c5).
follows(s63,c5).
follows(s66,c5).
follows(s71,c5).
follows(s72,c5).
follows(s73,c5).
follows(s74,c5).
follows(s75,c5).
follows(s76,c5).
follows(s81,c5).
follows(s88,c5).
follows(s89,c5).
follows(s90,c5).
follows(s93,c5).
follows(s95,c5).
follows(s100,c5).
follows(s3,c6).
follows(s9,c6).
follows(s10,c6).
follows(s13,c6).
follows(s16,c6).
follows(s20,c6).
follows(s25,c6).
follows(s27,c6).
follows(s36,c6).
follows(s46,c6).
follows(s50,c6).
follows(s51,c6).
follows(s55,c6).
follows(s64,c6).
follows(s67,c6).
follows(s69,c6).
follows(s77,c6).
follows(s79,c6).
follows(s84,c6).
follows(s85,c6).
follows(s96,c6).
follows(s97,c6).
follows(s8,c7).
follows(s12,c7).
follows(s18,c7).
follows(s24,c7).
follows(s35,c7).
follows(s40,c7).
follows(s43,c7).
follows(s48,c7).
follows(s52,c7).
follows(s60,c7).
follows(s65,c7).
follows(s70,c7).
follows(s78,c7).
follows(s87,c7).
follows(s98,c7).
follows(s1,c8).
follows(s2,c8).
follows(s3,c8).
follows(s6,c8).
follows(s9,c8).
follows(s10,c8).
follows(s13,c8).
follows(s15,c8).
follows(s16,c8).
follows(s17,c8).
follows(s19,c8).
follows(s20,c8).
follows(s21,c8).
follows(s22,c8).
follows(s25,c8).
follows(s26,c8).
follows(s27,c8).
follows(s30,c8).
follows(s31,c8).
follows(s33,c8).
follows(s36,c8).
follows(s46,c8).
follows(s47,c8).
follows(s50,c8).
follows(s51,c8).
follows(s55,c8).
follows(s56,c8).
follows(s58,c8).
follows(s64,c8).
follows(s67,c8).
follows(s68,c8).
follows(s69,c8).
follows(s77,c8).
follows(s79,c8).
follows(s80,c8).
follows(s82,c8).
follows(s83,c8).
follows(s84,c8).
follows(s85,c8).
follows(s86,c8).
follows(s91,c8).
follows(s92,c8).
follows(s94,c8).
follows(s96,c8).
follows(s97,c8).
follows(s99,c8).
follows(s8,c9).
follows(s11,c9).
follows(s23,c9).
follows(s24,c9).
follows(s35,c9).
follows(s37,c9).
follows(s39,c9).
follows(s43,c9).
follows(s45,c9).
follows(s52,c9).
follows(s60,c9).
follows(s63,c9).
follows(s65,c9).
follows(s70,c9).
follows(s73,c9).
follows(s76,c9).
follows(s78,c9).
follows(s81,c9).
follows(s87,c9).
follows(s88,c9).
follows(s89,c9).
follows(s95,c9).
follows(s1,c10).
follows(s2,c10).
follows(s3,c10).
follows(s6,c10).
follows(s9,c10).
follows(s10,c10).
follows(s13,c10).
follows(s15,c10).
follows(s16,c10).
follows(s17,c10).
follows(s19,c10).
follows(s20,c10).
follows(s21,c10).
follows(s22,c10).
follows(s25,c10).
follows(s26,c10).
follows(s27,c10).
follows(s30,c10).
follows(s31,c10).
follows(s33,c10).
follows(s36,c10).
follows(s46,c10).
follows(s47,c10).
follows(s50,c10).
follows(s51,c10).
follows(s55,c10).
follows(s56,c10).
follows(s58,c10).
follows(s64,c10).
follows(s67,c10).
follows(s68,c10).
follows(s69,c10).
follows(s77,c10).
follows(s79,c10).
follows(s80,c10).
follows(s82,c10).
follows(s83,c10).
follows(s84,c10).
follows(s85,c10).
follows(s86,c10).
follows(s91,c10).
follows(s92,c10).
follows(s94,c10).
follows(s96,c10).
follows(s97,c10).
follows(s99,c10).
follows(s8,c11).
follows(s23,c11).
follows(s24,c11).
follows(s43,c11).
follows(s45,c11).
follows(s60,c11).
follows(s63,c11).
follows(s65,c11).
follows(s70,c11).
follows(s73,c11).
follows(s76,c11).
follows(s81,c11).
follows(s87,c11).
follows(s95,c11).
follows(s8,c12).
follows(s23,c12).
follows(s24,c12).
follows(s43,c12).
follows(s45,c12).
follows(s60,c12).
follows(s63,c12).
follows(s65,c12).
follows(s70,c12).
follows(s73,c12).
follows(s76,c12).
follows(s81,c12).
follows(s87,c12).
follows(s95,c12).
follows(s1,c13).
follows(s2,c13).
follows(s3,c13).
follows(s6,c13).
follows(s9,c13).
follows(s10,c13).
follows(s13,c13).
follows(s15,c13).
follows(s16,c13).
follows(s17,c13).
follows(s19,c13).
follows(s20,c13).
follows(s21,c13).
follows(s22,c13).
follows(s25,c13).
follows(s26,c13).
follows(s27,c13).
follows(s30,c13).
follows(s31,c13).
follows(s33,c13).
follows(s36,c13).
follows(s46,c13).
follows(s47,c13).
follows(s50,c13).
follows(s51,c13).
follows(s55,c13).
follows(s56,c13).
follows(s58,c13).
follows(s64,c13).
follows(s67,c13).
follows(s68,c13).
follows(s69,c13).
follows(s77,c13).
follows(s79,c13).
follows(s80,c13).
follows(s82,c13).
follows(s83,c13).
follows(s84,c13).
follows(s85,c13).
follows(s86,c13).
follows(s91,c13).
follows(s92,c13).
follows(s94,c13).
follows(s96,c13).
follows(s97,c13).
follows(s99,c13).
follows(s11,c14).
follows(s35,c14).
follows(s37,c14).
follows(s39,c14).
follows(s52,c14).
follows(s78,c14).
follows(s88,c14).
follows(s89,c14).
follows(s11,c15).
follows(s35,c15).
follows(s37,c15).
follows(s39,c15).
follows(s52,c15).
follows(s78,c15).
follows(s88,c15).
follows(s89,c15).
follows(s11,c16).
follows(s35,c16).
follows(s37,c16).
follows(s39,c16).
follows(s52,c16).
follows(s78,c16).
follows(s88,c16).
follows(s89,c16).
follows(s11,c17).
follows(s35,c17).
follows(s37,c17).
follows(s39,c17).
follows(s52,c17).
follows(s78,c17).
follows(s88,c17).
follows(s89,c17).
follows(s1,c18).
follows(s2,c18).
follows(s3,c18).
follows(s6,c18).
follows(s9,c18).
follows(s10,c18).
follows(s13,c18).
follows(s15,c18).
follows(s16,c18).
follows(s17,c18).
follows(s19,c18).
follows(s20,c18).
follows(s21,c18).
follows(s22,c18).
follows(s25,c18).
follows(s26,c18).
follows(s27,c18).
follows(s30,c18).
follows(s31,c18).
follows(s33,c18).
follows(s36,c18).
follows(s46,c18).
follows(s47,c18).
follows(s50,c18).
follows(s51,c18).
follows(s55,c18).
follows(s56,c18).
follows(s58,c18).
follows(s64,c18).
follows(s67,c18).
follows(s68,c18).
follows(s69,c18).
follows(s77,c18).
follows(s79,c18).
follows(s80,c18).
follows(s82,c18).
follows(s83,c18).
follows(s84,c18).
follows(s85,c18).
follows(s86,c18).
follows(s91,c18).
follows(s92,c18).
follows(s94,c18).
follows(s96,c18).
follows(s97,c18).
follows(s99,c18).
follows(s7,c19).
follows(s8,c19).
follows(s11,c19).
follows(s12,c19).
follows(s14,c19).
follows(s18,c19).
follows(s23,c19).
follows(s24,c19).
follows(s34,c19).
follows(s35,c19).
follows(s37,c19).
follows(s39,c19).
follows(s40,c19).
follows(s41,c19).
follows(s43,c19).
follows(s45,c19).
follows(s48,c19).
follows(s49,c19).
follows(s52,c19).
follows(s53,c19).
follows(s60,c19).
follows(s62,c19).
follows(s63,c19).
follows(s65,c19).
follows(s66,c19).
follows(s70,c19).
follows(s72,c19).
follows(s73,c19).
follows(s76,c19).
follows(s78,c19).
follows(s81,c19).
follows(s87,c19).
follows(s88,c19).
follows(s89,c19).
follows(s93,c19).
follows(s95,c19).
follows(s98,c19).
follows(s4,c20).
follows(s5,c20).
follows(s28,c20).
follows(s29,c20).
follows(s32,c20).
follows(s38,c20).
follows(s42,c20).
follows(s44,c20).
follows(s54,c20).
follows(s57,c20).
follows(s59,c20).
follows(s61,c20).
follows(s71,c20).
follows(s74,c20).
follows(s75,c20).
follows(s90,c20).
follows(s100,c20).
follows(s4,c21).
follows(s5,c21).
follows(s28,c21).
follows(s29,c21).
follows(s32,c21).
follows(s38,c21).
follows(s42,c21).
follows(s44,c21).
follows(s54,c21).
follows(s57,c21).
follows(s59,c21).
follows(s61,c21).
follows(s71,c21).
follows(s74,c21).
follows(s75,c21).
follows(s90,c21).
follows(s100,c21).
follows(s4,c22).
follows(s5,c22).
follows(s28,c22).
follows(s29,c22).
follows(s32,c22).
follows(s38,c22).
follows(s42,c22).
follows(s44,c22).
follows(s54,c22).
follows(s57,c22).
follows(s59,c22).
follows(s61,c22).
follows(s71,c22).
follows(s74,c22).
follows(s75,c22).
follows(s90,c22).
follows(s100,c22).
follows(s4,c23).
follows(s5,c23).
follows(s28,c23).
follows(s29,c23).
follows(s32,c23).
follows(s38,c23).
follows(s42,c23).
follows(s44,c23).
follows(s54,c23).
follows(s57,c23).
follows(s59,c23).
follows(s61,c23).
follows(s71,c23).
follows(s74,c23).
follows(s75,c23).
follows(s90,c23).
follows(s100,c23).
follows(s29,c24).
follows(s42,c24).
follows(s54,c24).
follows(s57,c24).
follows(s59,c24).
follows(s74,c24).
follows(s4,c25).
follows(s5,c25).
follows(s71,c25).
follows(s75,c25).
follows(s100,c25).
follows(s32,c26).
follows(s28,c27).
follows(s38,c27).
follows(s44,c27).
follows(s61,c27).
follows(s90,c27).
follows(s1,c28).
follows(s2,c28).
follows(s3,c28).
follows(s6,c28).
follows(s9,c28).
follows(s10,c28).
follows(s13,c28).
follows(s15,c28).
follows(s16,c28).
follows(s17,c28).
follows(s19,c28).
follows(s20,c28).
follows(s21,c28).
follows(s22,c28).
follows(s25,c28).
follows(s26,c28).
follows(s27,c28).
follows(s30,c28).
follows(s31,c28).
follows(s33,c28).
follows(s36,c28).
follows(s46,c28).
follows(s47,c28).
follows(s50,c28).
follows(s51,c28).
follows(s55,c28).
follows(s56,c28).
follows(s58,c28).
follows(s64,c28).
follows(s67,c28).
follows(s68,c28).
follows(s69,c28).
follows(s77,c28).
follows(s79,c28).
follows(s80,c28).
follows(s82,c28).
follows(s83,c28).
follows(s84,c28).
follows(s85,c28).
follows(s86,c28).
follows(s91,c28).
follows(s92,c28).
follows(s94,c28).
follows(s96,c28).
follows(s97,c28).
follows(s99,c28).
follows(s1,c29).
follows(s2,c29).
follows(s3,c29).
follows(s6,c29).
follows(s9,c29).
follows(s10,c29).
follows(s13,c29).
follows(s15,c29).
follows(s16,c29).
follows(s17,c29).
follows(s19,c29).
follows(s20,c29).
follows(s21,c29).
follows(s22,c29).
follows(s25,c29).
follows(s26,c29).
follows(s27,c29).
follows(s30,c29).
follows(s31,c29).
follows(s33,c29).
follows(s36,c29).
follows(s46,c29).
follows(s47,c29).
follows(s50,c29).
follows(s51,c29).
follows(s55,c29).
follows(s56,c29).
follows(s58,c29).
follows(s64,c29).
follows(s67,c29).
follows(s68,c29).
follows(s69,c29).
follows(s77,c29).
follows(s79,c29).
follows(s80,c29).
follows(s82,c29).
follows(s83,c29).
follows(s84,c29).
follows(s85,c29).
follows(s86,c29).
follows(s91,c29).
follows(s92,c29).
follows(s94,c29).
follows(s96,c29).
follows(s97,c29).
follows(s99,c29).
follows(s7,c30).
follows(s12,c30).
follows(s14,c30).
follows(s18,c30).
follows(s34,c30).
follows(s40,c30).
follows(s41,c30).
follows(s48,c30).
follows(s49,c30).
follows(s53,c30).
follows(s62,c30).
follows(s66,c30).
follows(s72,c30).
follows(s93,c30).
follows(s98,c30).
follows(s7,c31).
follows(s12,c31).
follows(s14,c31).
follows(s18,c31).
follows(s34,c31).
follows(s40,c31).
follows(s41,c31).
follows(s48,c31).
follows(s49,c31).
follows(s53,c31).
follows(s62,c31).
follows(s66,c31).
follows(s72,c31).
follows(s93,c31).
follows(s98,c31).
follows(s7,c32).
follows(s12,c32).
follows(s14,c32).
follows(s18,c32).
follows(s34,c32).
follows(s40,c32).
follows(s41,c32).
follows(s48,c32).
follows(s49,c32).
follows(s53,c32).
follows(s62,c32).
follows(s66,c32).
follows(s72,c32).
follows(s93,c32).
follows(s98,c32).
follows(s7,c33).
follows(s12,c33).
follows(s14,c33).
follows(s18,c33).
follows(s34,c33).
follows(s40,c33).
follows(s41,c33).
follows(s48,c33).
follows(s49,c33).
follows(s53,c33).
follows(s62,c33).
follows(s66,c33).
follows(s72,c33).
follows(s93,c33).
follows(s98,c33).
follows(s8,c34).
follows(s23,c34).
follows(s24,c34).
follows(s43,c34).
follows(s45,c34).
follows(s60,c34).
follows(s63,c34).
follows(s65,c34).
follows(s70,c34).
follows(s73,c34).
follows(s76,c34).
follows(s81,c34).
follows(s87,c34).
follows(s95,c34).

% Lectures teach courses:
teaches(l1,c1).
teaches(l2,c2).
teaches(l3,c3).
teaches(l4,c4).
teaches(l4,c5).
teaches(l5,c6).
teaches(l5,c7).
teaches(l6,c8).
teaches(l6,c9).
teaches(l7,c10).
teaches(l7,c11).
teaches(l8,c12).
teaches(l9,c13).
teaches(l6,c14).
teaches(l10,c15).
teaches(l9,c16).
teaches(l11,c17).
teaches(l12,c18).
teaches(l13,c19).
teaches(l13,c20).
teaches(l14,c21).
teaches(l12,c22).
teaches(l12,c23).
teaches(l14,c24).
teaches(l15,c25).
teaches(l16,c26).
teaches(l14,c27).
teaches(l1,c28).
teaches(l17,c29).
teaches(l17,c30).
teaches(l3,c31).
teaches(l18,c32).
teaches(l1,c33).
teaches(l2,c34).

% Rooms have a capacity:
capacity(r1,10).
capacity(r2,30).
capacity(r3,50).

%Exam/study period starts:
first_day(1).

%Exam/correction period ends:
last_day(9).

%Rooms have availabilities:
availability(Room,Day,9,17) :- room(Room,_), between(3,7,Day).

%SOFT-CONSTRAINTS:
%lecturers
sc_lunch_break(L,1) :- lecturer(L,_). %Lecturers prefer a lunchbreak (12-13).
sc_b2b(L,2) :- lecturer(L,_). %lecturers prefer not to have exams back 2 back
%desired correction time (per exam):
sc_correction_time(e1,6).
sc_correction_time(e2,4).
sc_correction_time(e3,5).
sc_correction_time(e4,2).
sc_correction_time(e5,4).
sc_correction_time(e6,2).
sc_correction_time(e7,2).
sc_correction_time(e8,4).
sc_correction_time(e9,2).
sc_correction_time(e10,4).
sc_correction_time(e11,2).
sc_correction_time(e12,2).
sc_correction_time(e13,4).
sc_correction_time(e14,1).
sc_correction_time(e15,1).
sc_correction_time(e16,1).
sc_correction_time(e17,1).
sc_correction_time(e18,6).
sc_correction_time(e19,5).
sc_correction_time(e20,3).
sc_correction_time(e21,2).
sc_correction_time(e22,2).
sc_correction_time(e23,2).
sc_correction_time(e24,1).
sc_correction_time(e25,1).
sc_correction_time(e26,1).
sc_correction_time(e27,1).
sc_correction_time(e28,4).
sc_correction_time(e29,4).
sc_correction_time(e30,2).
sc_correction_time(e31,2).
sc_correction_time(e32,2).
sc_correction_time(e33,2).
sc_correction_time(e34,2).
sc_correction_penalty(L,2) :- lecturer(L,_). %Lecturers prefer sufficient correction time.
%Some exams shouldn't fall on the last days of the exam period
sc_not_in_period(l1,e1,7,0,24,10).
sc_not_in_period(l12,e18,7,0,24,10).
sc_not_in_period(l3,e3,7,0,24,5).
sc_not_in_period(l13,e19,7,0,24,5).
sc_not_in_period(l1,e1,6,0,24,5).
sc_not_in_period(l12,e18,6,0,24,5).
%Lecturer time preferences/unavailabilities
sc_no_exam_in_period(l3,Day,0,10,4) :- between(3,7,Day). %Morgan Nosek prefers to have no exam before 10h.
sc_no_exam_in_period(l4,Day,0,13,1) :- between(3,7,Day). %Glenn Janson prefers to have no exam before 13h.
sc_no_exam_in_period(l12,5,0,24,5). %Shalon Stains prefers to have no exam day 5.
sc_no_exam_in_period(l12,7,12,24,5). %Shalon Stains prefers to have no exam in the afternoon, day 7.
sc_no_exam_in_period(l14,7,12,24,5). %Wanda Griffins prefers to have no exam in the afternoon, day 7.
sc_no_exam_in_period(l15,5,0,24,5). %Oswaldo Bardo prefers to have no exam day 5.
sc_no_exam_in_period(l16,5,12,24,2). %Jama Nguyn prefers to have no exam in the afternoon, day 5.
sc_no_exam_in_period(l17,6,12,24,5). %Charleen Mellema prefers to have no exam in the afternoon, day 6.
sc_no_exam_in_period(l17,7,0,24,5). %Charleen Mellema prefers to have no exam day 7.

%students
sc_lunch_break(S,1) :- student(S,_). %Students prefer a lunchbreak (12-13).
sc_same_day(S,3) :- student(S,_). %students prefer not to have multiple exams on the same day
sc_b2b(S,5) :- student(S,_). %students prefer not to have exams back 2 back
%desired study time (per exam):
sc_study_time(e1,3).
sc_study_time(e2,3).
sc_study_time(e3,5).
sc_study_time(e4,2).
sc_study_time(e5,2).
sc_study_time(e6,2).
sc_study_time(e7,2).
sc_study_time(e8,2).
sc_study_time(e9,2).
sc_study_time(e10,2).
sc_study_time(e11,2).
sc_study_time(e12,2).
sc_study_time(e13,2).
sc_study_time(e14,2).
sc_study_time(e15,2).
sc_study_time(e16,2).
sc_study_time(e17,2).
sc_study_time(e18,3).
sc_study_time(e19,3).
sc_study_time(e20,5).
sc_study_time(e21,2).
sc_study_time(e22,2).
sc_study_time(e23,2).
sc_study_time(e24,2).
sc_study_time(e25,2).
sc_study_time(e26,2).
sc_study_time(e27,2).
sc_study_time(e28,2).
sc_study_time(e29,2).
sc_study_time(e30,2).
sc_study_time(e31,2).
sc_study_time(e32,2).
sc_study_time(e33,2).
sc_study_time(e34,2).
sc_study_penalty(S,2) :- student(S,_). %Students prefer sufficient study time.
%Some exams shouldn't fall on the first days of the exam period
sc_not_in_period(S,e3,3,0,24,8) :- student(S,_),follows(S,C),has_exam(C,e3).
sc_not_in_period(S,e19,3,0,24,8) :- student(S,_),follows(S,C),has_exam(C,e19).
sc_not_in_period(S,e3,4,0,24,4) :- student(S,_),follows(S,C),has_exam(C,e3).
sc_not_in_period(S,e19,4,0,24,4) :- student(S,_),follows(S,C),has_exam(C,e19).
