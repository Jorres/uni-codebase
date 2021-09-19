1) Software Engineering
defined as multiperson dev of multiversion programs
is different from programming in time, scaling and tradeoffs
in SE you work to produce a product, not just gain inner artists satisfaction.
product brings: users' attention, feedback loop, joy of helping people (feeling your impact), MONEY
SE is a team effort!

2) Pillars of SE productivity
health - you don't notice it till its gone | don't take for granted, avoid doing harm, try leading healthy lifestyle
         beware of slow decay
character - SE share common qualities. They are critical, curious, with a desire to improve. Humility, trust, respect
            bold, curious, smart, proactive, realistic, fast-learner\early adopter

note: this is the easiest thing to catch up on:
knowledge and skills - you have to invest in your knowledge portfolio. Regularly, ensuring your efforts are up-to-date
habits - easy to set up, hard to change later on

How to keep them under control, listen to experts and experiment! (but with health instead no experiments)

to be product oriented, most or all of your influencing factors should be product oriented.
    Influencing factors:
    team
    training
    experiments / projects
    feedback
    self-education

3) Productive programmer's philosophy
    1. Never run on autopilot, THINK, be self-critical 
    1. bigger picture
    1. be proactive - change what frustrates you
    2. Deal with failures professionally | admit -> mitigate -> fix -> learn, 10m$ story
    3. Don't live with broken windows | care and hopelessness are contagious |
        story about firefighters and carpet
    4. Love software that is `Good enough`
    5. Build up on your knowledge portfolio | many sources, critically analyze what u hear
    6. Be product oriented, prioritize your product and users over engineering problems

    Don't let yourself optimize your actions for perf reviews! this is fake productivity!
    but don't forget to snatch something useful once or twice)

4) Code review - last defense line (CL == change list, the diff)
code quality is not code style
CR is a very cheap error prevention, earliest possible except for when you do this yourself
CR provides personal growth

GOLDEN RULE: let the CL slip in if it does make your codebase even a little bit better than it was

CL can be `good enough`, don't strive for perfection, sometimes even let tactical tornado slip in
CL should be:
    1. focused on single thing, descriptive, easily understood like `what 
       it is for`
    2. functionality. Everything so difficult it can not be caught with linter. 
       DB, concurrency, cross-services invariants
    3. complexity. Is it reasonable, is there anything to EASILY improve?
       You have to prove author it is not simple. Incapsulate irreducible complexity.

    4. Apis. They are hard to change later! Is an API really functional?
    5. Tests, concise, easily read top-to-bottom, ONE THING PER TEST
    6. Documentation is like sex - any better than nothing
    8. Comments. They should not duplicate meaning from the code. They should provide 
       insight into why this was done in the first place.

    7. Naming. Use context, provide alternatives
    9. Language and domain specific guide-lines.
    
    Review fast (you block), review every line, approve easily with minor comments,
    use labels to indicate severity, don't be pedantic, CITE YOUR SOURCES,
    assume competence and goodwill, give guidance, make positive comments

    not okay: disagreement on big points -> escalate, use surrounding code, 
              don't be personal

5) Complexity in software systems
    CODE, misleading and outdated docs, complex ui all contribute. 

    money is helpless, throwing many qualified people on the project helps only short-
    term. Complexity accumulates, and the only question is `when` you'll get stuck?

    You need EXCELLENT SWE's and culture. It is a practical art, advice is helpful some
    times but quite rarely. 

    Main approach to fighting complexity: good, modular design that encapsulates complexity
    you're not using at the moment.

    Complexity = summ_i(time_i * complexity_i) for i in system_modules;

    Complexity symptoms: cognitive load, change amplification, unknown unknowns
    Generalized symptom: hard to change!
    Causes: dependencies, obscurity

    You can optimize between complexity and I WANT THIS FEATURE NOW.

6) Tactical and strategic programming
Tactical programming
    i want this feature now, ignore complexity issues, "quick fixes"
    for tactical teams there is no way back 90% time
    "tactical tornado" phenomenon
Strategic programming 
    prioritize over not ruining things at the moment, deprioritize current goals a bit
    to make things ETC in the future. But that does not mean premature optimization and YAGNI violation!
    Strategic teams make products which lifespan is infinitely greater than purely 
    tactical teams. 

But remember, you have to have your goals in your head at all times! Maybe the lifetime 
of the project is so small it is a good thing to be tactical tornadoes?

7) Modular design as the solution to complexity problems, brought by dependencies
module - abstract unit that provides functionality. Package, class, method could all be ones.
modules have interface part and implementation part.

decomposition into modules helps fight cognitive load by encapsulating complexity. 
your cognitive load is your module + interfaces of dependencies, NOT THEIR IMPLEMENTATIONS.

formal interface (function signature) + informal interface (docs) form a CONTRACT together

Pieces of advice:
    prefer stable and wide-known modules
    minimize number of nontrivial dependencies
    make simple interfaces (but deep enough, remember, you don't want to jiggle small
        modules around - that is actually HARD to use, not simple)

Reversibility - how easy it is to stop depending on this module?

8) Design By Contract (DBC) - doctrine that says  
`document for external users verify internally that your module
does no less and no more than it claims to do`.

Preconditions, postconditions, class invariants...

DBC allows for coding with trust. You may and must rely on contracts, otherwise you are
not really encapsulating anything because you have to lookup for implementation.

Even though `crash early`, you do not verify contracts of other modules in your own, 
that is coding without trust. 

9) DRY and shallow modules.
having duplicates of knowledge is gravely dangerous. If you need to change one thing, 
it is not a question of whether you will remember to change everything else. It is 
a question of when you'll forget. 
DRY - Don't Repeat Yourself - every piece of knowledge must have a single, 
unambiguous, authoritative representation in the system. 

DRY - refers to knowledge, not only to code. And vice versa, 
there are cases when there seemingly is duplication, but it is of code and not knowledge.

Something that is hard to use\find WILL BE DUPLICATED.

Crash early - allows you to locate an error with greater precision
Unit tests verify that if preconditions are met, postconditions and class invariants hold
true. Failure to stand up to preconditions is a bug, and is not your fault in `coding with
trust` paradigm.

10) Productive technical interviews
Policies are often shifted so as not to hire a wrong candidate. It is much cheaper
to miss somebody they would want than to hire an incompetent person.

It is up to you to know how to pass technical interviews even though they rarely correlate
with real-work. 
Typical interview structure 5 + 30 + 5

Human rationality and brain API.

Interviewer wants answers for `tick-`questions and his ultimate questions. 
3 types of interviewers: 
newbie - will do their best to keep it formal, they prove it to themselves
arrogant - will do their best to show they are better than you
typical - will try to find out whether it is pleasant to work with you
Important for you: first impression, be humble, be honest, responsible and at least 
not lazy. 

You need to talk during the interview. You need to give them a reason to hire you.
A hard question was probably hard enough for other candidates as well. You are 
compared against other real candidates. 

Behavioural questions - you need to communicate good things about you, specific, 
limit details.
Technical questions - test your solution, talk aloud, fix bugs, "think it works"

Luck - increase your chances to be lucky, do interviews often.

11) Deep and shallow modules.
deep module - powerful functionality, simple interface. Think GC or Unix FS.
shallow - does not provide enough functionality to compensate for introduced complexity.

even one-liner (think `string.isBlank()`) could be DEEP ENOUGH.
shallow: `setValueNullForAttribute(attr) {obj.setAttr(attr, null)};`. Inline such. 

Prioritize easy-to-read over easy-to-write.
Prioritize easy-to-use over easy-to-read interfaces. 

People tend to like small modules (because they often read them), 
but then they are often not deep enough. 

12) Module deepening techniques

1. Reasonable defaults - partial information hiding - from part of the users
2. New abstractions - what goes together comes together
3. Interface generalization. Beware of YAGNI - premature optimization though. 
   (consider `somewhat more general purpose`)
4. Merging modules (hide stuff that was explicit between modules before merge)
5. Crash early - don't try to mask a bug. 
6. Edge cases as interface part - example, empty vim selection may do nop. 
   That's better than throwing an exception, because you offer more - you 
   allow your user to generalize HIS code so it can use empty selections and
   pass them to you.

13) Automated developer-driven testing

Developer-driven are only unit tests. Load, stress and such we'll leave to QA. 
Tests as code are very different from prod code. They can ignore complexity, 
THEY WILL NOT BE CHANGED later, they do not require creativity, they don't 
require many knowledge. 

Why?
fewer defects, increased confidence, less debugging, test is a USER OF THE MODULE,
simpler code reviews

Alternatives? Diligent manual testing + GREAT SWE's 
This is probably only possible with a structure like: , where features are isolated. 

                      +-------+      +-------+   This is probably not a very functional 
                      |       +------|feat. 1|   project. 
                      |common |      +-------+
                      |       +--+            
                      +-------+  |   +-------+
                                 +---|feat. 2|
                                     +-------+

Manual testing stopped catching enough bugs. This is probably inevitable. 

What? 
Test everything you don't want to break. If you like it you should've put a 
CI test on it - the Beyonce rule. You can't test everything, so test common 
behaviours. `Line coverage` metric is evil but it helps in a devious way - 
you can tell whether code is covered or not from the first glance, but you 
cannot reason about quality of coverage even when it is 90%.

How?
unit: fast (otherwise won't be launched), not flaky, easy to write, easy to 
understand failures, can serve as low-level documentation
                                     
Good test coverage promote inverse broken windows effect. People keep writing!

Maintainable test - one that does not require additional attention after being
written.

You cannot test for implementation details. Or you write a fragile test. Test
through your public api. You can refactor all you want if you retain the same
public api.

Mocks got abused lately. many advantages, but fragile tests, because they 
are tighly coupled with implementation. `Change detectors` are showstoppers.

Prefer `fakes` over `mocks`.
Prever `behaviour`-driven over `method-`driven.

You can violate DRY in tests. They are designed to be stable, and should read
easily top-to-bottom.
