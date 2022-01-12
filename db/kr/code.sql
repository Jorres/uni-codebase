Запишите следующие запросы в терминах реляционной алгебры и на языке SQL.

1. Информация о подходах команды :TeamId в соревновании :ContestId.
Вернуть RunId, SessionId, Letter, SubmitTime, Accepted.
```
proj{RunId, SessionId, Letter, SubmitTime, Accepted}(
    proj{TeamId = :TeamId && ContestId = :ContestId}(
        Sessions nj Runs nj Problems
    )
)

select 
    RunId, SessionId, Letter, SubmitTime, Accepted
from
    Sessions natural join Runs natural join Problems 
where
    TeamId = :TeamId and ContestId = :ContestId;
```

2. Успешные подходы в соревновании :ContestId.
Вернуть RunId, SessionId, Letter, SubmitTime.
```
proj{RunId, SessionId, Letter, SubmitTime}(
    sigma{Accepted = 1 && ContestId = :ContestId}(
        Sessions nj Runs nj Problems
    )
)

select
    RunId, SessionId, Letter, SubmitTime
from
    Sessions natural join Runs
where
    Accepted = 1 and ContestId = :ContestId;
```

3. Команды, не решившие ни одной задачи ни в одном соревновании.
Вернуть TeamName.
```
proj{TeamName}(
    proj{TeamId, TeamName}(Teams) diff proj{TeamId, TeamName}(
        sigma{Accepted = 1}(Sessions nj Runs)
    )
)

select TeamName from (
    select 
        TeamId, TeamName
    from
        Teams
    except
        select 
            TeamId, TeamName
        from
            Sessions natural join Runs
        where
            Accepted = 1
) as q;
```
4. Команды, не решившие ни одной задачи хотя бы в одном соревновании.
Вернуть TeamName.
5. Команды, не решившие ни одной задачи хотя бы в одном соревновании, в котором они участвовали.
Вернуть TeamName.
6. Сессии, имеющие подходы по всем задачам в соревновании.
Вернуть SessionId.
7. Сессии, в которых решены все задачи в соревновании
Вернуть SessionId.
8. Команды, решившие все задачи хотя бы в одном соревновании (возможно, в разных сессиях)
Вернуть TeamName.
9. Задачи, которые решили все команды участвовавшие в соревновании
Вернуть ContestId, Letter.


proj{TeamId}(
    sigma{ContestId = :ContestId}(
        Sessions nj Contests
    )
)
