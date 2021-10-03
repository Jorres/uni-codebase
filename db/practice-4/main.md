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
1. StudentName, GroupId, GroupName функционально зависит от StudentId
2. CourseName от CourseId

StudentId -> StudentName, GroupId, GroupName
GroupName -> GroupId
GroupId -> GroupName
CourseId -> CourseName
GroupId, CourseId -> LecturerId
LecturerId -> LecturerName
StudentId, CourseId -> Mark


(StudentId, StudentName, GroupId, GroupName, CourseId, LecturerId, LecturerName, Mark) 

1. Разбили начальное, по CourseId -> CourseName, осталось
   (CourseId, CourseName)
   (StudentId, StudentName, GroupId, GroupName, CourseId, LecturerId, LecturerName, Mark)

2. Попробуем подобрать разбиение:

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) 
1. Бьем по теореме Хита по ФЗ CourseId -> CourseName
(CourseId, CourseName)
(... исходное, кроме CourseName)
2. Бьем оставшееся большое теореме Хита по ФЗ GroupId, CourseId -> LecturerId, LecturerName
(GroupId, CourseId, LecturerId, LecturerName)
(GroupId, CourseId, StudentId, StudentName, GroupName, Mark) (+)
3. Бьем (+) по StudentId -> GroupId, StudentName, CourseId
(StudentId, GroupId, StudentName, GroupName)
(StudentId, CourseId, Mark)

Методом подстановки по очереди убеждаемся, что не потеряли ни одну исходную ФЗ (для каждой ФЗ есть отношение, 
в котором есть полный набор атрибутов из данной ФЗ). 

Итого:

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) => 
(CourseId, CourseName);
(StudentId, CourseId, Mark);
(StudentId, GroupId, StudentName, GroupName);
(GroupId, CourseId, LecturerId, LecturerName)

## 3НФ

Проверяем условия третьей НФ. 
1. Неключевые атрибуты непосредственно (не транзитивно) зависят от ключей.

Какие у нас есть цепочки, к этому приводящие:
1. StudentId -> GroupId -> GroupName
2. GroupId, CourseId -> LecturerId -> LecturerName

Расщепляем их по правой части.
Итого результат:

(CourseId, CourseName) => 
(CourseId, CourseName) 
(StudentId, CourseId, Mark) => 
(StudentId, CourseId, Mark) 
(StudentId, GroupId, StudentName, GroupName) =>
(StudentId, GroupId, StudentName) ; (GroupId, GroupName)
(GroupId, CourseId, LecturerId, LecturerName) => 
(GroupId, CourseId, LecturerId) ; (LecturerId, LecturerName)

## NFBK

Проверим условия для НФБК
1. В каждой нетривиальной зависимости X -> Y, X является надключом

Можно скосить угол и воспользоваться фактом, что 3НФ + непересекающиеся ключи => НФБК. 
Покажем, что это выполняется для всех шести отношений:

- (CourseId, CourseName) -- true, ключ `CourseId` (имя курса неуникально)
- (StudentId, StudentName, GroupId) -- true, ключ `StudentId` (имя студента даже + группа неуникально)
- (GroupId, GroupName) -- true, ключи `GroupId`, `GroupName` (имя группы уникально, но два непересекающихся ключа)
- (LecturerId, LecturerName) -- true, ключ `LecturerId` (имя преподавателя неуникально)
- (StudentId, CourseId, Mark) -- true, eдинственный ключ `StudentId, CourseId`
- (GroupId, CourseId, LecturerId) -- true, единственный ключ `GroupId, CourseId`

Отлично, получили НФБК сразу из 3НФ, да при этом не потеряли никаких ФЗ!

(CourseId, CourseName) => (CourseId, CourseName) 
(StudentId, StudentName, GroupId) => (StudentId, StudentName, GroupId) 
(GroupId, GroupName) => (GroupId, GroupName) 
(LecturerId, LecturerName) => (LecturerId, LecturerName) 
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark) 
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId)

## 4NF

 
Заметим сразу, что по первой теореме Дейта-Фейгина следующие отношения находятся в 5НФ за бесплатно:
(т.к. все ключи простые, и каждое из них в 3НФ):
(CourseId, CourseName)
(GroupId, GroupName)
(LecturerId, LecturerName)
(StudentId, StudentName, GroupId)

(StudentId, CourseId, Mark)
(GroupId, CourseId, LecturerId)

Воспользуемся вариантом: каждая нетривиальная МЗ является ФЗ и отношение в НФБК (так как у нас уже есть НФБК)
Рассмотрим оставшиеся два отношения, для каждого из них в лоб проверим по 6 нетривиальных МЗ. Три вида A -> B | C, 
и еще три вида empty -> A, B | C.

Достаточно будет привести примеры, которые покажут, что две группы атрибутов справа в МЗ не образуют декартово 
произведение (т.е. привести такие наборы строк, чтобы при существовании ТОЛЬКО ИХ в таблице не образовалось 
декартово произведение нужных атрибутов).

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
empty -> LecturerId | CourseId, GroupId 
empty -> GroupId | CourseId, LecturerId 
empty -> CourseId | LecturerId, GroupId 

Ура ура, не нашли (нетривиальных) множественных зависимостей, получаем 4НФ без телодвижений.

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
в каждой группе непусто => ровно по одному атрибуту, так как у нас всего три атрибута:

1. StudentId, CourseId, Mark
Какое условие соответствует наличию зависимости соединения этих атрибутов? Вот такое:
(У студента 1 есть курс 2) &&
(По курсу 2 выставлена оценка 3) &&
(У студента 1 оценка 3) => у студента 1 по курсу 2 оценка 3

Контрпример:

У студента 1 есть курс 2, оценка 4.
По курсy 2 есть оценка 3 студенту 5.
У студента 1 есть оценка 3 по курсy 6.

Имеются все три условия, но не выполняется следствие. 

2. GroupId, CourseId, LecturerId
Какое условие соответствует наличию зависимости соединения этих атрибутов? Вот такое:

(У группы 1 есть предмет 2) && 
(Предмет 2 ведет лектор 3) && 
(Лектор 3 ведет у группы 1) => У группы 1 предмет 2 ведет лектор 3.

Контрпример:
У группы 1 есть предмет 2, ведет лектор 4.
Предмет 2 ведет лектор 3 у группы 5.
Лектор 3 ведет у группы 1 предмет 6.

Имеются все три условия, но не выполняется следствие. 

Таким образом, наш набор отношений находится уже в 5НФ (какие-то мы в лоб перебрали, а для каких-то применили
теорему Дейта-Фейгина 1 с лекции).

(LecturerId, LecturerName) => (LecturerId, LecturerName) 
(GroupId, GroupName) => (GroupId, GroupName)
(CourseId, CourseName) => (CourseId, CourseName)
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark)
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId)
(StudentId, StudentName, GroupId)  => (StudentId, StudentName, GroupId) 
