drop table if exists Groups cascade;
drop table if exists Students cascade;

create table Groups (
    group_id int,
    group_no char(6)
);
create table Students (
    student_id int, 
    name varchar(30),
    group_id int
);

insert into Groups
    (group_id, group_no) values
    (1, 'M34371'),
    (2, 'M34391');
insert into Students
    (student_id, name, group_id) values
    (1, 'Erica Shefer', 2),
    (2, 'Daniil Boger', 1),
    (3, 'Ravil Galiev', 1);

select group_id, group_no from Groups;
select student_id, name, group_id from Students;
select Students.name, Groups.group_no
    from Students natural join Groups;
select Students.name, Groups.group_no
    from Students
         inner join Groups
         on Students.group_id = Groups.group_id;

/* do not need this since I use cascade */
/* alter table Groups */ 
/*     drop constraint if exists group_id_unique; */

alter table Groups 
    add constraint group_id_unique 
    unique(group_id);

delete from Groups where group_no = 'M34381';

update Students set group_id = 5 where student_id = 1;
update Students set group_id = 1 where student_id = 1;

alter table Students 
    add constraint student_group_fk 
    foreign key (group_id) references Groups (group_id);

insert into Students
    (student_id, name, group_id) values
    (4, 'Гнатюк Дмитрий Вячеславович', 2),
    (5, 'Коробков Роман Леонидович', 2),
    (6, 'Левашов Георгий Игоревич', 2),
    (7, 'Шапошников Борис Юрьевич', 2);

select * from Students;
select * from Groups;
select count(*) from Groups;
/* select count(*) from Students; */
/* select count(*) from Students where group_id in (select group_id from Groups where group_no = 'M34371'); */
/* select count(*) from Students natural join Groups where name like '%ев%' or group_no like '%7%'; */
/* select count(*) from Students natural join Groups where name like '%r' and group_no like '%7%'; */

delete from Groups;
delete from Students;

insert into Groups
    (group_id, group_no) values
    (1, 'M34342'),
    (2, 'M34351'),
    (3, 'M34361'),
    (4, 'M34371');

insert into Students
    (student_id, name, group_id) values
    (1, 'Студент 1', 1),
    (2, 'Студент 2', 1),
    (3, 'Студент 3', 1),
    (4, 'Студент 4', 1);

insert into Students
    (student_id, name, group_id) values
    (5, 'Студент 5', 2),
    (6, 'Студент 6', 2),
    (7, 'Студент 7', 2);

insert into Students
    (student_id, name, group_id) values
    (8, 'Студент 8', 3),
    (9, 'Студент 9', 3);

insert into Students
    (student_id, name, group_id) values
    (10, 'Студент 10', 4);

select * from Students;
select * from Groups;

select group_no, count(*) from Groups g inner join Students s on g.group_id = s.group_id group by group_no order by group_no desc;

select group_no, count(*) from Groups g inner join Students s on g.group_id <> s.group_id where group_no like '%1' group by group_no order by group_no;


/* this will not work due to foreign key constraint: */
/* insert into Students */ 
/*     (student_id, name, group_id) values */
/*     (1, 'Erica Shefer', 100); */

/* create table Какаятотаблица (колонкараз int, колонкадва char(6)); */
/* select * from Какаятотаблица; */





















