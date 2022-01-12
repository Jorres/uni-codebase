# Intro

Program is a sequence of relations

Ivans(Id, LastName) :-
    People(Id, FirstName, LastName),
    FirstName = 'Ivan'.
    
Ivans(Id, LastName) :-
    People(Id, 'Ivan', LastName),
    
## Beware of infinite relations!

NotStudent(Id, Name) :- not Student(Id, Name, _).
                                              ^
                                              this creates an infinite number of things we have to check
                                              
                                             
Projection:
`Q(A1, A2, ...) :- R(A1, _, A2, ..., _, _).`

Union (implicit union, very very bad in my opinion)
`Q(A1, A2, ...) :- R1(A1, A2, ...).`
`Q(A1, A2, ...) :- R2(A1, A2, ...).`


except is from relational algebra,
exists is from relational calculus (datalog), 
all kinds of joins are from relational algebra (you direct how you want to join things),
and relational calculus just doesn't give a damn about joins and asks you to 
do cartesian products: `select ... from A1, A2, A3 where ... ;` here, comma is an implicit 
cartesian product.


Some small amount of practice:

HasMark(StudentId) :-
    Students(StudentId, _, GroupId),
    Plan(GroupId, CourseId, _),
    Marks(StudentId, CourseId, _),
    Courses(CourseId, :CourseName).
    
r(StudentId, StudentName, GroupName) :-
    Students(StudentId, StudentName, GroupId),
    Groups(GroupId, GroupName),
    Plan(GroupId, CourseId, _),
    Courses(CourseId, :CourseName),
    not HasMark(StudentId, CourseId).
