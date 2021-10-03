
StudentId -> StudentName, GroupId, GroupName
GroupId -> GroupName
GroupName -> GroupId
CourseId -> CourseName
GroupId, CourseId -> LecturerId
LecturerId -> LecturerName
StudentId, CourseId -> Mark

## 1NF

Проверяем условия первой НФ.
1. В отношении нет повторяющихся групп - верно. 
2. Все атрибуты атомарны (т.е. нет коллекций атрибутов) - верно. 
3. У отношения есть ключ - есть, вот он: `StudentId, CourseId`.

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) 
=> (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) 

## 2NF

Проверяем условия второй НФ. 
1. Неключевые атрибуты не должны функционально зависеть от части ключа, а только от ключа в целом. 

У нас это не так: 
StudentName, GroupId, GroupName функционально зависит от StudentId;
CourseName от CourseId.

StudentId -> StudentName, GroupId, GroupName
GroupId -> GroupName
GroupName -> GroupId
CourseId -> CourseName
GroupId, CourseId -> LecturerId
LecturerId -> LecturerName
StudentId, CourseId -> Mark

Но также мы теряем функциональную зависимость GroupId, CourseId -> LecturerId, если просто выкинем GroupId 
из оставшегося отношения, поэтому добавим еще одно отношение. Можно бы вот такое: (GroupId, CourseId, LecturerId),
но LecturerId еще определяет LecturerName, поэтому в итоге добавим вот такое (GroupId, CourseId, LecturerId, LecturerName)

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) => 
(CourseId, CourseName) ;  
(StudentId, StudentName, GroupId, GroupName) ; 
(StudentId, CourseId, Mark) ;
(GroupId, CourseId, LecturerId, LecturerName)

## 3NF

Проверяем условия третьей НФ. 
1. Неключевые атрибуты непосредственно (не транзитивно) зависят от ключей.

Какие у нас есть цепочки, к этому приводящие:
StudentId -> GroupId -> GroupName
StudentId -> GroupId ; GroupId, CourseId -> LecturerId -> LecturerName

Расщепляем их по правой части.
Итого результат:

(CourseId, CourseName) => 
(CourseId, CourseName)
(StudentId, StudentName, GroupId, GroupName) => 
(StudentId, StudentName, GroupId) ; 
(GroupId, GroupName)
(StudentId, CourseId, Mark) => 
(StudentId, CourseId, Mark)
(GroupId, CourseId, LecturerId, LecturerName) => 
(GroupId, CourseId, LecturerId) ; 
(LecturerId, LecturerName)

## NFBK

1. В каждой нетривиальной зависимости X -> Y, X является надключом

Это нам нужно проверить для каждого отношения, которое мы получили к данному моменту. 

Отношения: 

Для вот этих трёх все очевидно:
(LecturerId, LecturerName) -- тут LecturerId ключ и надключ
(GroupId, GroupName) -- тут в обе стороны
(CourseId, CourseName) -- тут тоже в одну сторону

Для вот этих трех так сразу неочевидно, но достаточно будет перебрать все непустые сочетания для левой части ФЗ, по 7 штук в каждом отношении, и убедиться,
что если присутствует ФЗ с такой левой частью, и ее правая часть тоже в этом отношении, то левая часть - надключ.

(StudentId, CourseId, Mark)
(GroupId, CourseId, LecturerId)
(StudentId, StudentName, GroupId) 

Утверждается, что каждое из моих отношений находится в НФБК. 

Также заметим, что у нас была 3НФ и не было пересекающихся ключей в каждом отношении => НФБК.

Итого: 

(LecturerId, LecturerName) => (LecturerId, LecturerName) 
(GroupId, GroupName) => (GroupId, GroupName)
(CourseId, CourseName) => (CourseId, CourseName)
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark)
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId)
(StudentId, StudentName, GroupId)  => (StudentId, StudentName, GroupId) 

## 4NF

Заметим сразу, что по первой теореме Дейта-Фейгина следующие отношения находятся в 5НФ за бесплатно:
(т.к. все ключи простые, и каждое из них в 3НФ):
(LecturerId, LecturerName)
(GroupId, GroupName)
(CourseId, CourseName)
(StudentId, StudentName, GroupId)

Воспользуемся вариантом: каждая нетривиальная МЗ является ФЗ и отношение в НФБК (так как у нас уже есть НФБК)
Рассмотрим оставшиеся два отношения, для каждого из них в лоб проверим по 6 нетривиальных МЗ. Три вида A -> B | C, 
и еще три вида empty -> A, B | C.

1. (StudentId, CourseId, Mark) 

StudentId -> CourseId | Mark ; Петя, БД, 3 && Петя, Алгоритмы, 5
CourseId -> StudentId | Mark ; БД, Вася, 3 && БД, Вася, 5
Mark -> StudentId | CourseId ; 5, Вася, Алгоритмы && 5, Петя, БД 

Для МЗ из пустого множества достаточно предъявить три строки из отношения, которые отличаются во всех трех атрибутах
(т.к. уже не получается декартово произведение)

Контрпример: Петя, Алгоритмы, 3 && Вася, БД, 5
empty -> Mark | CourseId, StudentId 
empty -> StudentId | CourseId, Mark 
empty -> CourseId | Mark, StudentId 

2. (GroupId, CourseId, LecturerId)

GroupId -> CourseId | LecturerId ; M3138, Дискретка, Андрей Сергеевич && М3138, Введение в программирование, Георгий Александрович
CourseId -> GroupId | LecturerId ; Введение, М3138, Н.В.Ведерников && Введение, М3134, Георгий Александрович
LecturerId -> GroupId | CourseId ; Георгий Александрович, М3138, Введение && Георгий Александрович, М34381, БД 

Для МЗ из пустого множества достаточно предъявить три строки из отношения, которые отличаются во всех трех атрибутах
(т.к. уже не получается декартово произведение)

Контрпример: Георгий Александрович, БД, М34381 && Андрей Сергеевич, Дискретка, М3138
empty -> LecturerId | CourseId, GroupId ; , , && , , 
empty -> GroupId | CourseId, LecturerId ; , , && , , 
empty -> CourseId | LecturerId, GroupId ; , , && , , 

Итого:

(LecturerId, LecturerName) => (LecturerId, LecturerName) 
(GroupId, GroupName) => (GroupId, GroupName)
(CourseId, CourseName) => (CourseId, CourseName)
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark)
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId)
(StudentId, StudentName, GroupId)  => (StudentId, StudentName, GroupId) 

## 5NF

Ищем нетривиальные зависимости соединения и проверяем, что каждый элемент в нем - надключ.
Зависимость соединения двух групп атрибутов - это просто множественная зависимость, мы такие 
поискали на прошлом шаге. Поэтому тут имеет смысл искать только с тремя группами атрибутов, где
в каждой группе по одному (так как у нас всего три атрибута):

1. StudentId, CourseId, Mark
Интуитивно кажется, что зависимости нет. Попробуем показать:
Студент А имеет оценку B по предмету C
Cтудент D имеет оценку E по предмету F

... ???


GroupId, CourseId, LecturerId
Здесь нет такой зависимости, т.к. у разных групп один и тот же курс могут читать разные преподы

