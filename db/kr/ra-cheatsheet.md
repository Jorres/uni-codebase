
# Intro

relation - something that returns a relation, be it a relation, or the result of proj/sigma query

sigma{columnId = :columnValue && someOtherClause}(relation)
prog{column1, column2, column3}(relation) 

## Simple example

proj{StudentId, StudentName, GroupName}(
    sigma{StudentId = :StudentId}(
        Students natural-join Groups
    )
)

## Equivalent sql

select 
    StudentId, StudentName, GroupName
from
    Students natural join Groups
on
    Students.GroupId = Groups.GroupId
where
    StudentId = :StudentId;
    
## Complex example

proj{StudentId, StudentName, GroupId}(
    Students natural-join 
    sigma{Mark = :Mark}(
        Marks natural-join proj{CourseId}(
            sigma{LecturerId = :LecturerId}(Plan)
        )
    )
)

## Other operations

- diff. Maps to SQL except.
`Students diff sigma{studentId = :studentId}(Students) -- everyone except given one`


## Some playground while remembering
Информацию о студентах с заданной оценкой по дисциплине
Которую у него вёл лектор, заданный ФИО (StudentId, StudentName, GroupId по :Mark, :LecturerName).
```ra
proj{StudentId, StudentName, GroupId}(
    sigma{Mark = :Mark && LecturerName = :LecturerName}(
        Students nj Plan nj Lecturers nj Marks
    )
)
```


