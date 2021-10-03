## Lecture 1

## Databases

They have structure and data

## Structure

-   schema, where we describe what we store, how we store and access rights. (we can
    store blobs of arbitrary data, but it is useful to know something about your data)

-   Explicit is better than implicit.

## Data

-   scale

    -   thousands (easy)
    -   millions (some effective algorithmic approach to help searching)
    -   billions (distributed)

-   OOP analogy. `schema + data` is like `class + instance`

## Types

## Simple types

we do not operate with parts of these types. we do not extract 3rd to 5th letters of
string naturally, we want to process it as a whole.

## Structured

we can mentally operate on parts of these data, we can decide something by a fraction
of the instance of the datatype datetime -> date + time address -> city + country +
... phone -> country + operator + number

## Entities

represented by class in OOP program

## Connecting entities

student's mark for a subject is a connection

## Constraints

They help to enforce consistency. We want to limit the data that is stored as hard as
we could, to process more errors on start.

## Value constraints

Student who is born `currentDate() + 10 years`. For example, age is a positive
integer, not just any integer. Probability - floating point number from 0 to 1.

## Record constraints

Two dates in an interval must be d1 < d2.

## Multiple record constraints

Two students cannot have the same passport numbers.

## Connection constraints

Every student must have exactly one connection to the group.

# Database history

1. Plain text comma separated file, constraints are difficult to enforce
2. Structured text typed column separated file, new metainfo is included to file, and
   if a record has fixed size, we can search for 10000th numbers without reading
   10000 lines.

    - Easy to read, hard to search, enforce constraints, no integrity check.
    - Examples: Excel

3. File structure model, folders become like indexes (put all of students from M3238
   into one folder), but still does not support every possible query.
4. Hierarchical - improvement over file structure model. Big disadvantage - no
   many-to-many relationships. What is primary - student or subject? In the first
   case, we quickly look up all the marks of 1 student, but it is tedious to parse
   all the marks for the subject - you have to visit every student.

5. Network data model is like a file system with symlinks. Primary hierarchical,
   secondary hierarchical. Student has marks, subject has links to its marks via
   symlinks.
6. Relational model. We store tables, many to many relationships become possible. You
   just have an intermediary table that has student id, subject id, and the mark.
   Subject and student became equal-righted.

    Different databases differ in how they are implemented.

    Advantages:

    - flexible data structure
    - mathemetical model underneath, there is an optimizer behind each query who may
      substitute your query with identical another Disadvantages:
    - can be really difficult to do fast (4-5 orders of magnitude)

7. Object data model, replaced with ORM in time.
8. NoSQL - Not only SQL. Relational database can do too much, we can narrow down the
   scenario to a more efficient implementation. But many problems arise like 'whether
   I will want to migrate to a different design tomorrow'. It requires additional
   understanding of your scenarious and is quite complex.

    NoSQL is really meant to improve execution speed. But it is enforced with our own
    code, and it is really easy to be mistaken.

### BOTTOM LINE

Your DB effectiveness does really depend on kinds of queries you are going to make to
it.

# Architecture

-   program -> driver -> network -> driver -> SUBD -> data storage
-   SUBD = query parser + query executor
-   query executor is extended with scheduler (optimizer) + memory management +
    statistics (as feedback for executor)

# Examples

## Commercial

### Oracle

Corporate flagship, has high throughput, but also high latency (because queries are inderdependant, and
latency and throughput should be reversely proportional but on practice they are not)

### DB2

### Microsoft SQL server

## Free

### MySQL

Pomotalo etu loshadku tak sebe.

### PostgreSQL

experimental project, but stable enough for production use

## Embedded - in memory

### SQLite

small, fast, low overhead, but slow and scales badly

### HSQLDB

main use case - testing

### Apache Derby

### MS Access

something that you'll need if Excel is not enough

---

# Practice 1

# SQL

is supported by almost anyone, but formally not supported by anyone. Standard is so gross and
large and there is not even a reference implementation.

## SQL 1 - 88

-   simple queries
-   simple joins
-   access rights
-   cursors - iterators on steroids, but they are heavily optimized under the hood

## SQL 2 - 92

-   more complex joins
-   dynamic (parametrized) queries
-   transactions isolation levels
-   Call Level Interface, but did not fly high

## SQL 3 - 99

-   standard refactored - required and unnecessary features
-   object language binding, sql right inside java code

## SQL 2003

"куча новых дополнений, нельзя сказать, что они нестандартные, потому что они в стандарте"

## SQL 2006, 2008, 2011

-   multiple bloated standard developments

# Examples

    CREATE DATABASE ctd;
    CREATE TABLE Groups(
        group_id int,
        group_name char(6)
    );

    CREATE TABLE Students(
        student_id int,
        group_id int,
        student_name varchar(1000)
    );

    // insert something into Groups

    INSERT INTO Students (group_id, student_id, student_name) values (
        0, 0, 'Vasya',
        0, 1, 'Anya',
        1, 2, 'Petya',
    )

    SELECT group_id, group_name FROM Groups;
    SELECT * FROM Groups;

The following examples work for O(n^2) without any sly optimizations, because
db has to iterate over all the pairs. (or at least for the size of the answer)

    select student_name, group_name from Students natural join Groups;

    select student_name, group_name from Students inner join Groups
    on Students.group_id = Groups.group_id;

The problem of non-unique identifiers. We need to do

    delete from Groups where group_name = 'M34371';

    alter table Groups add constraint group_id_unique
    unique(group_id)

## Lecture 2

## Physical vs conceptual model

Goal: automate conceptual -> physical -> sql

## Physical model

Tables, links, sometimes - helpers (views, procedures, triggers)
Table is defined by typed columns.
Row is an entry, composed of one value per column.

### Keys

Key is a set of fields that uniquely identifies the row.
Keys can be **natural** (subject domain) and **surrogate**(additional data for convenience):

-   passport number - natural key
-   id - surrogate key
    Keys can be complex - when two columns, series and passport number, are composed to form
    a unique identifier together.

Surrogate keys exist mainly to save memory, because you usually need to store them (to maintain
an information about a connection). Therefore they usually are very simple (autoincrement) and do
not change (because why, that may invalidate the connection).

Keys have implicit priorities. For the sake of convenience, we denote one PRIMARY key.
We can reference one table with different keys, but this only complicates things and
you could always replace with one.

### Links a.k.a. foreign keys

Link is a column in a table when it is also a key for some other table
Also known as `foreign key`.
Foreign key BY CONVENTION -> BY DEFAULT references PRIMARY key from the other table.
The will to store reference to secondary key is usually the sign of a bad design.

### Properties of the column

M - Mandatory
O - Optional
PK - Primary key
Kn - additional Key number b

## Model "entity-connection"

column is now named as an attribute,
physical type is now called `a domain`, it is still SOME physical
type underneath but it is closer to a subject domain.

### Connections

Connection has a name and a type (denoted by the ends).

```
--------    by default, no additional info

------|-    L can have one and only R    |
                                         | usually are not used together
------O-    L can have one or zero R     |

       /
--------    L can have many R
       \

       /
-----|--    L can have many R, but at least one
       \
```

### Associations

Generalization of connections. A single instance of an association now carries
attibutes (allows to store additional data about the connection) but has no keys.
Is already quite close to a full-bodied entity.

May have an arbitrary number of connections with other entities, but not with other
associations because they have no keys and wtf?? How to address pairs in this case?

Associations, connections and entities are rather interchangeable.

Connections is simple.
Entities if identifiable.
Everything in between is association.

### Complex limitations

When your association has multiple connections and one of the ends has a marker on it,
it becomes increasingly unobvious how to read it.

-   Chen limitation, look-across
-   Meris limitation, look-here
-   Generalization, discard an arbitrary subset and declare the rest Chen or Meris

#### Relationship between complex limitations

Connections: chen = meris = general
3-associations chen + meris = general
4+-associations chen + meris < general

### Weak entity

Does not have enough attributes to identify it (no primary key)
Has a connection with some other entity, that along with attributes
of the other entity there arises some compound key.

Weak entities and identifying connections are drawn with double borders.

### ERM -> PDM

-   increasing - manual
    beware of swap in meris interpretation
-   direct - automatable 0. entities -> tables
    1. associations -> weak entities
    2. weak entities -> tables,
       identificating connections -> new keys,
       additional keys to identify them as entities
    3. optional connections 1-1 require additional table

```
btw `constraint unique` and `key` are the same things
```

Skipped a gazillion of details on how to convert a connection to foreign-key-notation.

Bottom line: a picture is not just a picture, it is an accurate representation of a physical
model (pragmatic difficulties with generalized limitations though)

Store maps from domains to types anyway (db does not know name -> varchar(50))

### DDL (subset of SQL)

```
create table <name> (<column>[,<column>]*) <non-standartized>
column = <name> <type> (null|not null)? (default <value>)?

<name> generated [by default | always] as identity

(constraint <name>)? primary key (<column>[,<column>]*)
(constraint <name>)? unique (primary)? key (<column>[,<column>]*)
(constraint <name>)? foreign key (<column>[,<column>]*) references <tablename>(<column>[,<column>]*)

drop table <name>;

alter table WHATEVER;
```

### Data types - look at the presentation

## Lecture 3

#### Mathematical foundations for relations

Relationship on a database manner
Header - set `name : type`
Body - set of sets of `name : value`
Relationship will map to table

Relationship is a subset of a cartesian product.

Row in a table is an element of an extended cartesian product (because nulls).

Attributes have no natural order, because it is a set.

Strictly saying, rows in a table also do not have any kind of out-of-the-box order because
they are a set, but people sometimes rely on row order in SQL (formally incorrect, practically almost
always a bad thing).

Key, upkey.

Most SUBD can check key constraints.

-   Keys are unique by definition, and it is very convenient to NOT CHANGE KEY ever.
-   Because this key is probably stored elsewhere, and you have to change multiple occasions.
-   This is why it is not recommended to have natural keys, ever. They have tendency to change. Passport
    is unique, but people change it once in a while. Use surrogate keys - there is no reason to change them, ever.

#### Functional dependency

X, Y - set of attributes
X -> Y: for every `x` in X there is only one `y` in Y
All attributes are fuctionally dependant on upkey
This is not a FUNCTION, it is a dependency. Knowing passport number, you cannot know the name of the owner.
It is not a function, it is the property of the data.

There is no way to get one automatically, there is only domain knowledge.
Automatically they can only be proven wrong.

Trivial dependency - Y is a subset of X. They are useless, they are always correct and do not carry any useful info.

Summary

1. SUBD support primary\foreign keys
2. SUBD do not support arbitrary functional dependencies
3. More functional dependencies => less speed

Tasks

1. Morph arbitrary FD into key constraints
2. Find balance between quality \ speed

#### Formalism

1. Armstrong rules
   a. X subset of Y -> Y => X
   b. X -> Y => XZ => YZ
   c. X -> Y, Y -> Z => X -> Z
   d. X -> X
   e. X -> YZ => X -> Y, X -> Z
   f. X -> Y, Z -> D => XZ -> YD

2. Closure, S+
   {A -> B, B -> C} =close=> {A -> B, A -> C, B -> C, AB -> C + 19 trivial FD}

3. Weakness
   S is not stronger than P == S+ in P+
   S is equivalent to P => S not stronger P, P not stronger S

New task: find another set of FD, minimal size, equivalent to given

4. Estimate closure capacity
   attribute can go nowhere, to left side, to both sides => 3^n
   trivial + non-trivial = non-trivial
   O(3^n) \* non-trivial-dependencies is WAY TOO BIG A NUMBER for a common application

5. New hack - attribute closure

   attribute closure X\*\_S of attribute set X with FD set S - all attributes, functionally dependant on X

   how to build?
       X+\_S = X
       for every A -> B
           if A is in X+\_S then 
               add B to X+\_S
   theorem: X+\_S = X\*\_S. Prove by inclusion to both sides. 

6. from attribute closure to closure
   theorem: A -> B in S+ <=> B in A\*\_S

   corollary: X - upkey <=> X+ over S is the set of all attributes

## Unpleasant news

People have not found a way to build a minimal set of FD, equivalent to the given set. 
So they came with heuristics. 

We call a set of FD unreducible:
1. Every right part of FD contains 1 attribute. 
2. Every left part of FD is inclusion-minimal. 
3. S is inclusion-minimal. 

Mitigating news - every set of FD can me morphed into unreducible. 

1. Split every right part via [:@#### Formalism]'s `e` rule. 
2. Try minimizing every left part, dropping by one. Potentially brings many new rules. 
3. Try dropping rules themselves. 

It is good to try `minimizing` your initial set of FD's this way. But unredicuble set 
is in no way minimal.

## Lecture 4, normalization

The process of modifying schema to normal forms
Intuition on what's a `good schema`.
Help in finding design problems. 
Can be specifically denormalized if needed (normalize first, denormalize second).
There are 5 normal forms, each is more limiting. 
Remember, DB initially has no way to enforce restrictions to your data. It can only 
    check unique constraint by primary key and foreign key constraints. 

Define **projection** - a subset of attributes and its corresponding subset of 
rows. May contain LESS rows (because those two who were previously different now may  
become equal).

Define **join** - take two sets of attributes, X1..XnY1..Yn, Y1..Yn..Z1..Zn
Result: set of attributes X1..XnY1..YnZ1..Zn, 
Do so for every pair of rows from each set. 
Join is commutative and associative. 

Define **decomposition** - splitting set of attributes into two or more projections
(if into one - we lose data or it is identity projection).
Correct decomposition - when joining back together, we get what we splitted from. 

### Hit (Heat?) theorem
X -> Y => (XYZ) decomposed into (XY) (XZ) is a correct ont. 
Strictly saying: `R = projection_XY(R) join projection_XZ(R)`
Prove by inclusion both ways. 


### 1NF

1. No repeated groups of attributes (e.g. `phone1, phone2, phone3`)
    what to do if 4rth phone exists?
    hard way to find out SELECT FROM PEOPLE WHERE PHONE = <phone>
    even harder to find the pair of people with different phones
2. All attributes are atomic (== don't confuse with compound key, here collections are forbidden)
    forbids arrays [phone1, phone2, phone3]
    still a hard way to do select by phone
3. Relation has key <=> no repeated lines

How to do: duplicating data. 
From `phone1 phone2` make 2 records and a compound new key from previous + phone.
Anomalies: we cannot insert anything without a phone now :( and when deleting, we may delete extra info 
that is still required, we now need to change ALL the phones or we'll make a contradiction in db data. 

### 2NF

1. 1NF
2. non-key attributes functionally depend on the whole key (not the partial key).
Only happens when your keys are compound
`CourseId -> Exam` (\*)
`CourseId YearId -> LecturerId`

How to do: decompose by (\*). It will be correct, using Hit's theorem. 

Anomalies: still cannot insert phone without key attributes

### 3NF

1. non-key attributes functionally depend ON KEY DIRECTLY (non transitively).

How to do: decompose by last FD in the transitive chain. 

Anomalies: can be several intersecting keys

### NF Boice-Kodd

1. in every non-trivial FD X -> Y X is an up-key. 
NFBK is strictly stronger than 3NF, BK clause => 3NF
3NF, no intersecting keys => NFBK. 

How to do: decompose by one of the intersecting FD where left part is key. 

NFBK can be achieved from any relation. 

Problem: we can lose FD when morphing into NFBK. 
Statement: to 3NF we can always morph without losing info. 
Bottom line: 3NF is probably best in many cases, its problems are rare. 

### Multimeaning 
X ->> Y in R, if set Y does not depend on R \ X \ Y

### Feigin theorem 
Generalization of Hit's theorem 
R(XYZ) = `projection_XY(R) join projection_XZ(R)` <=> X ->> Y
Prove in two ways again. 

### 4NF

1. for every nontrivial FD X ->> Y|Z X is an upkey. 
4NF is NFBK. 

Bottom line: every relation can be decomposed into 4NF. 

### 5NF projection-joinable

joined dependency (JD)

For every non-trivial joined-dependency every `X_i` is upkey.
People can't find them quiclky, you'll more probably catch them when designing your db. 
JD manifest themselves in loops. 

Some helpful theorems:
Deit-Feigin-1: 3NF, keys simple -> 5NF
Deit-Feigin-2: NFBK, at least 1 simple key -> 4NF





