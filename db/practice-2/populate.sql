drop table Students cascade;
drop table Groups cascade;
drop table Marks cascade;
drop table Subjects cascade;
drop table GroupHasSubjects cascade;
drop table Teachers cascade;
drop table TeacherTeachesSubjects cascade;

/* tables */
create table Students (
    studentId int not null,
    studentName varchar(100) not null,
    birthDate date not null,
    groupId int not null,
    unique(studentId, groupId)
);

create table Groups (
    groupId int not null,
    groupName varchar(100) unique not null,
    studentWitness int not null,
    subjectWitness int not null
);

create table Marks (
    studentId int not null,
    subjectId int not null,
    dateReceived date,
    markAsLetter char(1) not null,
    markAsPoints int not null,
    unique(studentId, subjectId)
);

create table Subjects (
    subjectId int unique not null,
    subjectName varchar(100) not null,
    subjectDescription varchar(100) not null,
    subjectCapacity int not null,
    groupWitness int not null,
    teacherWitness int not null
);

create table GroupHasSubjects (
    groupId int not null,
    subjectId int not null,
    unique(groupId, subjectId)
);

create table Teachers (
    teacherId int unique not null,
    teacherName varchar(100) not null,
    teacherDescription varchar(500),
    subjectWitness int not null
);

create table TeacherTeachesSubjects (
    teacherId int not null,
    subjectId int not null,
    unique(teacherId, subjectId)
);

/* add primary keys */
alter table Students 
    add constraint studentsPK 
    primary key (studentId);

alter table Groups 
    add constraint groupsPK 
    primary key (groupId);

alter table Marks 
    add constraint marksPK 
    primary key (studentId, subjectId);

alter table Subjects 
    add constraint subjectsPK 
    primary key (subjectId);

alter table GroupHasSubjects 
    add constraint groupsSubjectsPK 
    primary key (groupId, subjectId);

alter table Teachers 
    add constraint teachersPK 
    primary key (teacherId);

alter table TeacherTeachesSubjects 
    add constraint teachersSubjectsPK 
    primary key (teacherId, subjectId);

/* add foreign keys */
/* see commentary in DML section 
   about using deferrable policy */

alter table Students 
    add constraint studentBelongsToGroupFK
    foreign key (groupId) 
    references Groups (groupId)
    deferrable initially immediate;

alter table Groups 
    add constraint studentWitness
    foreign key (groupId, studentWitness) 
    references Students (groupId, studentId)
    deferrable initially immediate;

alter table Groups 
    add constraint subjectWitness
    foreign key (groupId, subjectWitness) 
    references GroupHasSubjects (groupId, subjectId)
    deferrable initially immediate;

alter table Marks 
    add constraint markOfStudentFK
    foreign key (studentId) 
    references Students (studentId);

alter table Marks 
    add constraint markOfSubjectFK
    foreign key (subjectId) 
    references Subjects (subjectId);

alter table Subjects 
    add constraint groupWitness
    foreign key (subjectId, groupWitness) 
    references GroupHasSubjects (subjectId, groupId)
    deferrable initially immediate;

alter table Subjects 
    add constraint teacherWitness
    foreign key (subjectId, teacherWitness) 
    references TeacherTeachesSubjects (subjectId, teacherId)
    deferrable initially immediate;

alter table GroupHasSubjects 
    add constraint linkGroupSubjectToSubjectFK
    foreign key (subjectId) 
    references Subjects (subjectId)
    deferrable initially immediate;

alter table GroupHasSubjects 
    add constraint linkGroupSubjectToGroupFK
    foreign key (groupId) 
    references Groups (groupId)
    deferrable initially immediate;

alter table Teachers 
    add constraint subjectWitness
    foreign key (teacherId, subjectWitness) 
    references TeacherTeachesSubjects (teacherId, subjectId)
    deferrable initially immediate;

alter table TeacherTeachesSubjects 
    add constraint linkTeacherSubjectToSubjectFK
    foreign key (subjectId) 
    references Subjects (subjectId)
    deferrable initially immediate;

alter table TeacherTeachesSubjects 
    add constraint linkTeacherSubjectToTeacherFK
    foreign key (teacherId) 
    references Teachers (teacherId)
    deferrable initially immediate;


/* We're using transaction to insert entries that have mutually 
   dependant foreign keys. PostgreSQL checks constraints on transaction 
   end only, we only need to append deferrable policy `initially deferred` 
   to constraints that would have conflicted. 

   I think that using transactions is the best idea here, because 
   it is almost the same as the financial transaction - in no time you 
   can have an empty group OR a student without a group. You can have only 
   both or nothing. */
start transaction;

set constraints 
    studentBelongsToGroupFK,
    studentWitness,
    subjectWitness,
    groupWitness,
    teacherWitness,
    linkGroupSubjectToSubjectFK,
    linkGroupSubjectToGroupFK,
    subjectWitness,
    linkTeacherSubjectToSubjectFK,
    linkTeacherSubjectToTeacherFK
    deferred;


insert into Students 
    (studentId, studentName, birthDate, groupId) 
    values 
    (0, 'Егор Тарасов', current_date, 0),
    (1, 'Дмитрий Гнатюк', current_date, 1),
    (2, 'Александра Иванова', current_date, 0);

insert into Groups 
    (groupId, groupName, studentWitness, subjectWitness) 
    values 
    (0, 'M34381', 0, 0),
    (1, 'M34391', 1, 1);

insert into GroupHasSubjects
    (groupId, subjectId)
    values
    (0, 0),
    (1, 1);

insert into Subjects 
    (subjectId, subjectName, subjectDescription, 
     subjectCapacity, groupWitness, teacherWitness) 
    values 
    (0, 'Дизайн баз данных', 
        'Научитесь проектировать базы данных', 100, 0, 0),
    (1, 'Проектирование программного обеспечения', 
        'Научитесь проектировать программное обеспечение', 50, 1, 1);

insert into TeacherTeachesSubjects
    (teacherId, subjectId)
    values
    (0, 0),
    (1, 1);

insert into Teachers 
    (teacherId, teacherName, subjectWitness) 
    values 
    (0, 'Георгий Корнеев', 0),
    (1, 'Александр Киракозов', 1);

commit;

/* Marks do not form a loop of foreign keys, so we can 
   insert them independently without a scoped transaction. */
insert into Marks
    (studentId, subjectId, dateReceived, 
     markAsLetter, markAsPoints)
    values
    (0, 0, current_date, 'A', 95),
    (1, 0, current_date, 'A', 94),
    (1, 1, current_date, 'A', 93);

/* We can also execute single inserts into foreign-key-looped tables, 
   but only if they already have the data we are referring to. */

insert into Students 
    (studentId, studentName, birthDate, groupId) 
    values 
    (3, 'Еще-один-студент', current_date, 1);
