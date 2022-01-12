## Functional deps:

StudentId -> StudentName, GroupId, GroupName
GroupId -> GroupName
GroupName -> GroupId
CourseId -> CourseName
GroupId, CourseId -> LecturerId
LecturerId -> LecturerName
StudentId, CourseId -> Mark

## What can be sped up?

1. Checking key existance
    - in, exist, count
2. Searching for a key
    - searching itself
    - natural join requires searching implicitly
    - note: regular joins do not
#### If you are using BST as a structure for your index:

3. Extremum by some key
4. Range
5. count
6. `like` by prefix

## 'Recommendations' by GK
1. Indexes on keys
2. Indexes on foreign keys
3. Indexes on range queries (btree)
4. Indexes on join tables (both ways! id1->id2 and id2->id1)
5. String queries for batch operations

## Random notes go brrr
Surrogate keys are good! They are small and have high selectivity
Covering index is often used for a join table in a many-to-many relationship

using btree (SName, __**SId**__) // this is very cheap, 100 bytes + 4 bytes, 
allows to get values from index

hash (c1 .. cn) speeds up queries where we know c1 .. cn 
btree (c1 .. cn) speeds up queries where we know a prefix of c1 .. cn

id are used when joining. 

