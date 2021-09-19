O# Lection 1

Concurrent              vs     Distributed
* 1 machine             ||     many machines
* centralized           ||     geographically distributed
* shared memory         ||     sending messages
* has state and time    ||     no state and time
* full topology         ||     partial topology
* design for failures   ||     no design for failures
* less reliability      ||     more reliability
* easy                  ||     difficult

Distributed system is written when you cannot do the other thing. 

We have processes, events, "happens before". 

Message consists of events:
* `snd`
* `rcv`, where
* `snd` -> `rcv` in happens-before. 

a, b: a !-> b are called parallel.


Let's try to circumvent the lack of shared time. 
Logical clock: f: e, g in E, e -> g: f(e) < f(g), f outputs int numbers 
note the `->` and not `<->`, because order on numbers is inherently full
and the order on events is inherently partial. 

### Lamport's logical clock LLC
We store an int in each process, make ++ of the int when we send a message
and do `max(ourInt, receivedInt)` on receive. By construction, if e -> f in 
happens before, then f(e) < f(g) where f - Lamport's logical clock.
Is not unique!!!! When we have f(a) = 4 and f(b) = 5 we cannot say a -> b!

### Vector clock
A construct fully capturing happens-before on events. We store a vector with 
size equal to amount of nodes, on send we ++ our component, on receive we do 
by-component `max`. Constructively proven, left and right:
For processes i and j:
```
v[i](e) < v[i](g)  |  <=> e -> g.
v[j](e) < v[j](g)  |
```
Our component is a counter of events on our process.

### Direct dependency clocks
We can't afford sending O(nodes) numbers on each message. 

Def: e < f, snd(f) < rcv(f), f < g => e => g in direct dependency. 
Transitively closed, this gives us happens-before.
Achieved by storing n values, but ++ing and sending only our component.

### Matrix clocks
Literally store what we know and what other processes know about the time
in the system. Used in causal dependency algorithm. 

### Critical section
Define: Exit(CS[i]) -> Enter(CS[i + 1])
Of course we demand progress and honesty, or trivial solution of not letting
anyone into a CS works)

##### Centralized algorithm
We have a coordinator, and `request`, `release` and `ok` messages.
When someone wants to enter - sends `request`. Coordinator sends `ok` in 
an honest order, there is no sense in not making this honest. Every Exit
and Enter critical section events are in happens-before through the 
coordinator.

Centralized algorithm does not scale.

##### Distributed Lamport's CS algorithm
We maintain a priority queue of all processes willing to enter CS. 
We send `request` to everyone when we want to enter. 
Everyone around sends `ok` JUST TO CONFIRM.
We send `release` when we leave the CS.
We decide to enter if we received all `oks` and all their pairs(clock, id)
are greater than ours.

This algorithm relies heavily on FIFO, because:
Start: 0 and 1 want to enter, send `req`, then 0 sends `ok`, but his `ok`
arrives earlier than his `req` and so:
* 1 enters CS because no one else wants 
* and 0 enters CS because 0 < 1. 

##### Rickard and Agravala optimization
We combine `rel` and `ok`, basically send `ok` instead of `rel` (or `ok` in 
the sense of `ok` when we don't want cs OR when we are later than he anyway)
and we decide that we want to enter CS when we heard `ok` from all guys 
earlier than ourselves. Does not rely on FIFO.  


##### Cool optimal algo of dining philosophers
We construct a dependency graph, orient it the first time so that it does not
have loops (by node ids, for example) and call a resource a fork. A fork can 
be dirty or clean. 

Key facts
* there is a source in an oriented graph with no loops
* if we invert all edges around a source, no loops will appear

If we want to enter CS, we collect forks:
we ask for every fork we do not have
we give away our own dirty forks while collecting
we wash all the remaining dirty forks that were ours and eat ONLY
after we collected everything. 

Optimal in a sense - if we want to enter CS repeatedly, no message 
communication will occur. If only `m` out of `n` processes are willing
to enter CS, then only they will communicate.

##### Token ring algorithm
Processes are connected in a ring, token is ceaselessly transmitted round
robin, we have token => we enter CS, we don't have token => we wait, this 
is not honest and the delay can be up to NODES messages before we may enter, 
but it is dead simple.

##### Quorum based algorithms

Quorum - a family of subsets of a given set, where every two subsets intersect 
with a non-empty intersection. What it means? If we ask the quorum, and it 
permits, we cannot find another quorum that would also permits us to do something
while the resource is still busy. 

Nontrivial quorum - `breaking walls` quorum. Interesting - it is much less in size
than simple majority, it has only O(sqrt(N)) elems instead of O(N) in majority. 
Of course, this could be generalized to root of any degree of n in Int. 

```
|ooo o oo
|XoXXoXooX    X-s are quorum elements, the whole column and by 1 element from 
|oXoXXoXo     every other column
|oooXoooX
-----------
```

## Lection 2

Now we're going to hack the lack of global state.

Cut G - subset of E so that for any e, g: g in G, e < g => e in G.
Consistent cut - where `<` is changed to `->`.

Motivation - record system state, continue calculations from some recorded
snapshot. Can be only stopped and resumed at consistent cuts, because

                 │                otherwise some process B will receive 
O         A ─────┼─X───────────   the message twice - before restarting 
                 └──╲─────┐       and after restarting.
          B ──────────╲>X─┼──── 
                          │

##### Chandy-Lamport algorithm
Note: when we have a distributed algorithm in multiple instances, we just
supply an algorithm id (from the initiator, as some SHA256) and thus we 
separate parallel states of several algorithms.

All processes are initially WHITE. 
Initiator process becomes RED and sends a special token to all other processes.
When a process receives token, it becomes RED and sends the same token to all 
the others (we need to resend it to others because we use it to save messages).

The state would not be complete without storing messages that are `on route`. 
`RED->WHITE\RED` we don't save, they will be resend later after restarting. 
`WHITE->WHITE` we already processed. 
`WHITE->RED` we need to send. 

On receiver: when we become RED, we start recording messages from A 
and stop recording when we receive a red token from A. 

On sender: require confirmation for every message from our neighbor A, 
when the token comes from A, we remember all the messages that we sent
but didn't get processed (because all corresponding confirmation messages 
are later than this marker => A didn't process the messages yet).

Note: heavily relies on FIFO, otherwise it is possible that we send a token
and then a message, and receive a message and only then a token => storing 
algorithm would get confused and we could receive the same message twice.

##### Global system predicates and properties
Examples: we would like to know when our system is in deadlock or is stopped. 

We cannot have incostistent cuts there, incostistent cut can show 2 processes
in critical section when in reality it just missed the `release` message.

Stable predicate - if true on one cut, true on all subsequent.
Examples: token loss, deadlock.

Local predicate - predicate on one process.

If a general predicate is OR of local predicates, it is easy - just track 
local predicates and message the coordinator when yours is true.

AND of locals is difficult. 

#### Weak conjuntive predicate
We define an instable predicate true if it is true on at 
least one consistent cut.

##### Centralized
Send our vector clocks to the coordinator when our predicate succeeded. 

Note, that a consistent cut is always characterized by N vectors which 
are incomparable in pairs. If exist v1 < v2 => exists one inconsistent 
message between these processes. (+- 1 is quite important in these 
comparisons).

Starting colors: RED for all non-coord processes. 
Coordinator colors processed into RED and GREEN. Processes send their 
vector clocks when their predicate is true, after every message they send. 
When we receive a vector from RED, we invalidate all `true` processes that are less
in vector comparison (or it would be inconsistent cut) and make us GREEN. 

We didn't discuss what happens if a process wishes to tell that his predicate
is no longer valid :\ But if his predicate is invalid and no other 
messages take place we could just cut earlier...

Why builds consistent and why does not miss one, constructively proven. 

##### Decentralized
Every algorithm now has its own coordinator part, they send their message to 
this part (logically, not physically), this increments time. Every coordinator,
when GREEN, puts into queue, when RED, becomes GREEN and sends his current 
vector to all other guys who get invalidated by him, which in turn become RED 
again.

#### Special cases of stable predicates

##### Diffunding calculations

Solved with Dijkstra-Shalten algorithm. 
You have active processes (can send messages) and passive (can become active 
by receiving messages). 

We will arrange processes in a tree. It will be in a tree if passive, no 
children and no messages from me. Starting process sends messages and appends 
himself into a tree. 

Process behaviour when receiving a message, appends himself to a tree until 
he is free again and then he deletes himself from the tree. 

This tree collapses from leaves. 

##### Locally stable predicate

Interval - a pair of cuts. 
A consistent interval - no messages back across cuts, no messages straight 
through both cuts. 

Th: consistent interval => it has a consistent cut inside. 

Barrier-synchronized interval - when every e and g, where e is before the 
first cut and g is after the second, e -> g. 

Easily built through a coordinator, or twice-round-train-message, or every 
one sends everyone. 

Locally stable predicate - like stable, but on one subset of processes. 
Like a deadlock on some predefined subset of processes.

Coordinator can build a barrier-sync interval, then have a look at the left 
and at the right cuts, and even if they are inconsistent and their state is not 
changed, it means they are deadlocked (they were in some consistent cut).

## Lection 3

Ordering messages

1. Async transmission - that we can expect without making any effort on 
   ordering messages in a modern async networks. Internet has many routes...
   this kind of thing. Your messages could easily get rearranged.
2. FIFO order - when this is forbidden:

```
───────X──X─────────────── 
        ╲─│─\              
──────────v──v────────────
```

   Recovery algorithm: you enumerate the messages, and if you receive something
   for what you have not received a parent yet, you store it and retrieve later. 

3. Causal consistency order
   Like FIFO, but transitive

```
 ────────X──────X──────────────────
         ╲────   ╲─\       
 ─────────────╲────v─X─────────────
                  ╲──│─\  
 ────────────────────v──v──────────
```                      

  Centralized: pass every message through the coordinator, and he could easily
  have FIFO between himself and everyone else. 
  Decentralized: using matrix clock. When you receive a matrix, you determine 
  * FIFO - `received[you][sender] == yours[you][sender] + 1`
  * that nobody else sent you messages that didn't arrive yet:
  `yours[you][other] >= received[you][other]` for all other != you && other != sender
   
4. Synchronous order
  When no two parallel messages are allowed to run. 

```
---------------X-->---
                \/    
                /\    
---------------X-->---
```
  
  It is like message transmission is instantaneous, if you can order all the 
  messages passing through the system. 

  > Centralized: through coordinator and wait for ACK. Coordinator is only waiting
  > for at most one ACK at a time, then declares the message instantly delivered
  > and starts processing a new one.

  > Distributed: we construct a hierarchy from our processes, divide them into 
  > big and small. 
  > * big -> small: just sends, but blocks all receiving and sending until he got
  > a response from small. 
  > * small -> big: requests a permission to send a message, and then big -> small
  > part repeats itself - big, if decides to allow, freezes, while sending this
  > allowance. 

  Never a deadlock, all the processes are hierarchied. 
                                                                     
  Synchronous order is like linearizability - like all the operations are 
  executed into some shared memory. 

#### Broadcast and multicast
Broadcast has its own specific problem - total order. Messages from several 
broadcasts must be received by all recipients in the same order. 

```
 client --------X----------------------------------
                | \--       
 server --------1----\2---------------------------- received 1 then 2
                     / \--  
 server --------2---/-----\1----------------------- received 2 then 1
                | /    
 client --------Y/---------------------------------

```
It is like sending sword hits in different order - first hits 1, 2 dies, 
and on the other server first hits 2, 1 dies. Inconsistency! Bad!

Solutions:
* Centralized: broadcast is always done through a coordinator, who only 
  has to have FIFO between himself and targets, so that when he does 
  two consecutive broadcasts, first broadcast will be earlier than second.

* Lamport algorithm
    AAAAA SHIT

* Skin algorithm 
    AAAAA SHIT

## Lection 4

Distributed systems are bound to have hardware-level failures as well as 
software level ones, as we scale them to tens of thousands of machines.

Classes of failures:

1. Crash (node failure)
2. Link failure
3. Message omission
4. Byzantine failure

Each one is more difficult to overcome than the previous one. General idea:
error class `i` could easily mimic error class `i - 1`, but not vice-versa.

Link failures around one node === crash.
Message omission with 100% omission === link failure.
Byzantine failure can do what he pleases, respond selectively, mimics 
message omission.

We usually fight with 1 or 3, 2 is almost always like 3 and 4 is too domain 
specific. 

#### Synchronous and asynchronous system
Synchronous - there exists an upper bound for how long a message can travel.
Usually those are very specific networks with hardwired configuration that 
never changes that allows for educated guessing on timeout. Processor cores
could form a synchronous system, for instance. 

Asyncronous - no upper bound, message could be en route for unpredictable
amount of time. Much more abundant than synchronous, the whole internet is 
async.

Consensus - a model task that we will use because it is most hard to solve 
in the set of practical tasks worth solving. Having solved the consensus 
problem, we get many new primitives that allow for easier system design.

If there are no errors, consensus is dead simple - `N` nodes, every one 
proposes `value[i]`, waits for `N - 1` responses, applies a deterministic
function to `{ value[1] ... value[N] }` and yields that result. Some will 
do it earlier, some later, but every one of those will yield the same. 

Now the bad part. 

#### Fischer-Lynch-Paterson
In an asynchronous system, with the weakest error - node failure, with the 
weakest form of consensus - 1 bit consensus, it is impossible to reach
consensus with a determined algorithm using just a finite amount of time. 

It just renders the hope to build a perfect algorithm that would solve it
unusable. 


AT THIS POINT YOU SHOULD SERIOUSLY CONSIDER NOT PROCEEDING WITH DIGITIZING
YOUR NOTES BECAUSE YOU ALREADY HAVE THEM, EDIT THEM. YOU COULD HAVE PROCEEDED
IF YOU HAD MORE TIME, BUT NOW IT MIGHT BE A GOOD IDEA TO WATCH THROUGH THE 
SLIDES AND THROUGH TOLYA's NOTES. 

## Lections 4-12 are unreplicated :(


## Lection 13
Self-stabilisation
From any state to legal state in finite number of turns. 

Mutual exclusion algorithm as example. Legal state - only one process has 
privilege (to enter CS). 

Arrange processes in a ring, each machine has K, K >= N, states, 
knows its own and knows state of left machine from the ring. 

##### How to get privilege
* First in ring has privilege if state[1] == left[1];
* For all other: state[i] != left[i]

State change rule: only machine with a privilege can change its state. 

##### Transfer rule:
* First in ring: state = (state + 1) % K, notifies next in ring (right)
* For all other: state[i] = state[left[i]], notifies next in ring (right)

Legal transfers to legal via transfer rules (obvious)
From any state legal is achievable through finite turns. 
Note: at least one always have a privilege (privilege clause is true)

Note: first machine will always have a privilege in O(N^2) turns
The only thing that other processes do - copy state from left, 
and the X process will bring its value to Y through the ring.
```
           *********    
         **>-->-->-v**  
       **>-^       >-v**
       * |  1 round  | *
       **^  copying v<**
         **    <---<**  
           **X*S*Y**    
```
Note 2: at some time O(N^3) first machine will have a unique S
(only it brings new states to the system, and K >= N)

Once first machine is unique => N^2 more steps and we have a legal state. 
Privilege == Critical section, we may enter 

##### Building ostov tree
State - node has distance to root, and his parent
Root hardcoded, says dist = 0, parent = -1
The rest, periodically: query their neighbours, 
```
dist = min(distOther) + 1
parent = argmin(distOther)
```
