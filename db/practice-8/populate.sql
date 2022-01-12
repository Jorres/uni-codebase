drop table Students cascade;
drop table Groups cascade;
drop table Marks cascade;
drop table Lecturers cascade;
drop table LecturerBindings cascade;
drop table Courses cascade;

create table Groups (
    groupId int primary key,
    groupName varchar(10) unique not null
);

-- primary key. В реальной жизни автосоздается.
-- hash, потому что не нужен покрывающий и не нужна нигде
-- упорядоченность, получится дорогой индекс из-за строки.
-- ДЗ-5.2.1 Полная информация о студентах, natural join по GroupId
-- ДЗ-5.2.2 Полная информация о студентах по StudentName, 
-- natural join по GroupId
create unique index Groups_groupId_pkey 
    on Groups using hash (groupId);

-- Полезно, т.к. groupName - тоже ключ, и есть кейсы, где 
-- нужно селектить по groupName, и наверняка очень часто будем
-- искать по префиксу, например M343* - выберем КТ четвертого курса
-- Также сделаем его покрывающим, это дешево (инт по сравнению
-- со строкой), и теперь можно не ходить на диск, если нужны groupId. 
-- ДЗ-7.2.4 Перевод всех студентов группы по :*GroupName
-- ДЗ-7.2.5 Перевод всех студентов в существующую группу
create unique index Groups_groupName_key 
    on Groups using btree (groupName, groupId);


-------------------

create table Courses (
    courseId int primary key,
    courseName varchar(100) not null
);

-- primary key. В реальной жизни автосоздается.
-- hash, потому что не нужен покрывающий и не нужна нигде
-- упорядоченность, получится дорогой индекс из-за строки.
-- ДЗ-5.5.1, ФИО студента и названия предметов которые у него 
-- есть по плану, natural join Students-Courses
-- ДЗ-6.6.1,  GroupId и CourseId, такие что все студенты группы 
-- сдали предмет
create unique index Courses_courseId_pkey 
    on Courses using hash (courseId);

-- btree, чтобы можно было искать по префиксу названия, и чтобы 
-- можно было сделать покрывающим - доложим ещё courseId. 
-- (те же причины, что и в индексe на groupName)
-- ДЗ-5.3.2, Информацию о студентах с :Mark по предмету :CourseName
create index Courses_courseName
    on Courses using btree (courseName, courseId);

-------------------

create table Lecturers (
    lecturerId int primary key,
    lecturerName varchar(100) not null
);

-- Индекс на primary key в Lecturers я бы вообще не создавал при 
-- наших сценариях использования, так как: если нужны исключительно 
-- lecturerId, то в заданиях они есть в контексте таблицы Plan, 
-- а если требуются lecturerName, то эту задачу исполнит покрывающий 
-- индекс btree (lecturerName, lecturerId), который приведён далее.
-- create unique index Lecturers_lecturerId_pkey 
--    on Lecturers using hash (lecturerId);
-- 
-- Стандартный покрывающий индекс, аналогичен Groups и Courses
-- ДЗ-5.3.4 Информацию о студентах с :Mark по предмету :LecturerName
-- ДЗ-5.3.6 Информацию о студентах с :Mark по предмету :LecturerName
create index Lecturers_lecturerName
    on Lecturers using btree (lecturerName, lecturerId);

-------------------

create table Students (
    studentId int primary key,
    studentName varchar(100) not null,
    groupId int not null,
    foreign key (groupId) references Groups (groupId),
    unique (studentId, groupId)
);


-- primary key. В реальной жизни автосоздается.
-- hash, потому что не нужен покрывающий и не нужна нигде
-- упорядоченность, получится дорогой индекс из-за строки.
-- ДЗ-7.1.3, Удаление студентов без оценок
-- ДЗ-7.1.4, Удаление студентов с 3+ оценками
-- ДЗ-7.2.1, Изменение имени студента
create unique index Students_studentId_pkey 
    on Students using hash (studentId);

-- Часто нужно выбрать студентов по группе, но при этом не нужна 
-- упорядоченность и не нужна допинформация => не нужен упорядоченный
-- индекс.
-- ДЗ-7.1.1, Удаление студентов по :GroupId
-- ДЗ-7.1.2, Удаление студентов по :GroupName, `groupId in ...`
-- ДЗ-7.1.8, Удаление студентов c 2- долгами, `Students njoin Plan`
create index Students_groupId_fkey 
    on Students using hash (groupId);

-- multicolumn indexes unsupported in PostgreSQL
-- ДЗ-6.3.1  StudentId, CourseId, такие что у студента был предмет
create index Students_groupId_studentId
    on Students using hash (groupId, studentId);

-------------------

create table Plan (
    
)

-- Я считаю, что индексы плана по одному атрибуту полезны, 
-- т.к. план - это по смыслу джойн-табличка, и очень много юзкейсов,
-- когда по одному компоненту плана нужно достать другие. 
-- (очень много natural join'ов Plan со всем, что угодно)
--
-- Индекс по lecturerId полезен отдельно, т.к. почти всегда
-- lecturerId берется из плана, а не из таблицы Lecturers.
-- ДЗ-5.3.3 Информацию о студентах с :Mark по предмету :LecturerId
-- ДЗ-7.1.6 Удаление студентов с долгами
-- ДЗ-7.1.7 Удаление студентов с 2+ долгами
create index Plan_lecturerId
    on Plan using hash (lecturerId);

-- ДЗ-5.3.3 Информацию о студентах с :Mark по предмету :LecturerId
-- (natural join по CourseId)
-- ДЗ-5.3.5 Информацию о студентах с :Mark по предмету :LecturerId
-- (natural join по CourseId с Marks)
create index Plan_courseId
    on Plan using hash (courseId);

-- Иногда хочется выбрать информацию из плана для конкретной группы
-- ДЗ-5.3.3 Информация о студентах с Mark по предмету 
create index Plan_groupId
    on Plan using hash (groupId);

-- Есть еще один индекс, альтернативный индексу просто на groupId.
-- Он позволит получать курсы по группе, как предыдущий, но еще 
-- преподавателей по курсу + группе. 
-- ДЗ-5.3.3 Информация о студентах с Mark по предмету 
create index Plan_groupId_courseId
    on Plan using btree (groupId, courseId);

-------------------

create table Marks (
    studentId int,
    courseId int,
    mark char(1),
    primary key (studentId, courseId)
);


-- primary key. В реальной жизни автосоздается.
-- Для ситуаций, когда нужны все оценки конкретного студента по 
-- конкретному предмету. Таких сценариев много
-- ДЗ-5.3.2 Информацию о студентах с :Mark по предмету :CourseName
-- ДЗ-7.3.5 Число долгов одного студента
-- ДЗ-7.3.7 Число долгов каждого студента группы
create index Marks_studentId_courseId
    on Marks using hash (studentId, courseId); 

-- Для ситуаций, когда нужны все оценки одного студента. Обычно
-- нужны все оценки, а известно только studentId, поэтому hash.
-- Тут возникает практический вопрос - возможно, лучше предыдущий
-- индекс сделать btree, и тогда его можно будет частично 
-- использовать по studentId, или же создать отдельный hash индекс. 
-- Наверное, здесь надо мерять, главное вспомнить, что можно двумя 
-- способами. 
-- ДЗ-7.3.1 Число оценок студента
-- ДЗ-7.3.2 Число оценок каждого студента
-- ДЗ-5.8.1 SumMark по :StudentId
create index Marks_studentId_courseId
    on Marks using hash (studentId); 

-- Для ситуаций, когда нужны все оценки по фиксированному курсу. 
-- Обычно нужны все оценки, а не подмножество, поэтому hash, а не 
-- btree, т.к. от btree не будет выгоды. 
-- ДЗ-6.5.3 StudentId имеющих оценки по всем предметам :LecturerName
-- ДЗ-6.5.4 StudentId имеющих оценки по всем предметам :LecturerName, 
-- которые он у него вёл
create index Marks_mark
    on Marks using hash (courseId);


------------------

select
    avg(coalesce(cast(Mark as double), 0)) as AvgMark
from 
    Students left join Marks 
on
    Marks.StudentId = Students.StudentId
where
    Students.GroupId in 
        (select 
            GroupId 
        from 
            Groups 
        where 
            GroupName = :GroupName) and
    Marks.CourseId in 
        (select 
            CourseId 
        from 
            Courses 
        where 
            CourseName = :CourseName);

-- Первыми будут полезные индексы, которые помогут получить 
-- courseId и groupId по соответствующим *Name.
-- Btree, так как нам поможет покрывающий индекс - Id дешево
-- положить в индекс, если там уже лежит строка имени
create index Courses_courseName 
    on Courses using btree (courseName, courseId);
create unique index Groups_courseName 
    on Groups using btree (groupName, groupId);

-- Этот индекс поможет быстро выбрать все оценки по курсу. 
-- Было бы классно, если бы из Marks можно было еще до джойна 
-- выбрать только те оценки студентов, которые нам нужны (из нужной
-- группы), но я не придумал, как :\ 
create index Marks_courseId on Marks using hash (courseId);

-- Этот индекс поможет выбрать всех студентов из нужной группы. 
-- Hash, так как нам всё равно придется пробегаться по каждому из 
-- студентов группы. 
create unique index Students_groupId 
on Students using hash (studentId);

----------------
