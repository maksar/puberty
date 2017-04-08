# How to Man Up Using Property-Based Testing

Modern software development is impossible without automated tests. These include basic Unit and Integration tests, complex Performance и Penetration tests, and many other ways of checking programs even before the manual testing process begins. Managers have long ago stopped contesting the value of automated tests. Teams practicing test automation and following TDD and BDD methodologies are faster to deliver working systems with fewer errors, which allows them to beat competition on the overcrowded market of commercial software system development.

There are areas of development where the main focal point is the quality and stability of the entire system or its individual parts. In such cases, developers must use more powerful and rigorous tools and methods for automated code testing. Excellent examples of these are verification methods, orthogonal by their nature: mutation testing and property-based testing.

![](/images/data-program-result.png?raw=true)

Mutation testing boils down to deliberately changing program code in all possible ways and later verifying the fact that launching tests using modified code caused an error. If the program code change does not cause any of the unit or integration test to “fall”, one can conclude that either the certain portion of the code is not used at all, or the tests do not contain an example triggering the launch of this portion of the code. In both cases, it is necessary to pay attention to the comprehensiveness of the tests, or code quality.

Property-based testing works in a different way: the code remains the same, the data changes. The essence of the method is to generate pseudo-random data supplied to the program input and subsequently check the set of properties (invariants) by verifying the result. An example of the most basic invariant is absence of exceptions during execution. That is, the program is executed without crashes, regardless of what has been applied to the input.

So, changes occur to either the data incoming to the program input, or the very structure of the program. Both methods are quite complex in use but very powerful tools. The purpose of this publication is to get acquainted with the basis and peculiarities of property-based testing by creating a Workflow-engine using `ELM` functional programming language.

## Strict Typing and Type Inference

`ELM` is a statically typed functional programming language, which means arguments and the function’s returned value in `ELM` are always described by specific data types. Moreover, in `ELM` all functions are “pure”, that is, they contain no “side effects”. In practice, this means that programs written in `ELM` are easier to verify: calling an arbitrary function with the same set of arguments always returns the same result, without changing the overall state of the system.

To a certain extent, these advantages are inherent to many functional languages. But nothing comes for free: developers need to be more disciplined and be able to think in a “functional” way. No more returning `null` from the function where it’s convenient. No more tricks with explicit type casting. Developers need to pay more attention to modeling states and processes within the business domain, as well as make sure that it’s impossible to express unacceptable states of the model in the code.

## Formulating the Problem

No more useless chatter, show me the code! OK, OK. Let’s look at the example of a rather simple but at the same time nontrivial engine of Workflows from my previous article [Keep calm and kill mutants](https://github.com/maksar/mentat):

---

- Each step has a **threshold** value.
- To **proceed** to the next step, it is necessary to get *threshold* number of votes.
- Power users can **force skip** one step.
- Users with at least *vote* permission can **reject** current step (previous step restarts).
- **Inactive** users cannot *vote* or *reject*.
- User can only vote **once** on the same step.

---

![](/images/workflow-animation.gif?raw=true)

For the source code to be easier to understand, I decided not to reinvent the wheel and attempted to convert code from [`ruby`](https://github.com/maksar/mentat/blob/master/version_6/workflow.rb) into `ELM` one-to-one. Of course, the result doesn’t look anything like the original since both the syntax and the approach are completely different. But hopefully this will provide an opportunity to appeal to the more familiar imperative `ruby` if difficulties arise.

## Data Types

Let's start with the definition of the most important type – `Workflow`. It’s a structure (record), consisting of a set of fields:

```elm
type alias Workflow =
    { stepsConfig : Array Int -- an array of thresholds for each step
    , currentStep : Int       -- step number, on which workflow currently is
    , votes : Array Bucket    -- an array of a vote buckets 
                              -- users, who voted on each of the steps
    }
```

Let’s define `User` in a similar way. Please note that `User` is a full-featured type, unlike `Workflow`, which is essentially a named data record. The first mention of `User` in the definition is the name of the type, and the second is the name of the type constructor. In most cases, developers make those names equal for convenience.

```elm
type User
    = User
        { name : String                  -- name of the person, used for
                                         -- identification purposes
        , active : Bool                  -- user status flag (keep in mind,
                                         -- that inactive users can't vote)
        , permissions : Array Permission -- array of user's permissions. Permission type
                                         -- defined as Permission = VOTE | FORCE | NONE
        }
```

## Implementation 

Next, let’s review one of the program functional abilities – getting approval from the user with `FORCE` permissions at any step of the process. It starts from calling `approve` function (for comparison here is a link to a corresponding [`ruby`](https://github.com/maksar/mentat/blob/master/version_6/workflow.rb#L10-L17) implementation). The function expects two arguments: `User` and `Workflow` and always returns `Workflow`. If the user is inactive (`not (User.active user)` expression is true), then the result remains unchanged. Otherwise, pattern matching takes place by `User.permission workflow.currentStep user` expression (calling the `permission` function from the `User` package with arguments `workflow.currentStep` and `user`). It looks weird, but it IDE (or any advanced text editor, really) can help, prompting the type signature of the `permission` function: `permission : Int -> User -> Permission`. The `Permission` type is an algebraic sum, and the compiler requires to explicitly define what to do in each case.

```elm
approve : User -> Workflow -> Workflow
approve user workflow =
    if not (User.active user) then
        workflow
    else
        case User.permission workflow.currentStep user of
            FORCE ->
                increment user workflow

            VOTE ->
                vote user workflow

            NONE ->
                workflow
```

Since the voting user has `FORCE` permission, program execution will continue by calling the `increment` function. The type signature of this function is the same as in `approve` – `user` comes first and `workflow` second. The choice of such an order of arguments is not accidental. The reason for this is that in `ELM`, as well as in some other functional languages, `pipeline` operators `|>` and `<|` are widely used. They make it easy to construct call chains, when the returned value of some expression is passed to the next one. So, `increment`, using the `step` helper function, creates a new copy of `newWorkflow`, with incremented number of `currentStep` field. `newWorkflow`, in turn, is also a copy of `workflow` from the function’s arguments, but with a modified `votes` field.

In an imperative language, we could simply update one of the elements of `votes` array. However, data structures in functional languages are usually immutable, so you cannot just add one more vote to the set of votes on the current step. A new set with an additional element must be constructed based on the existing one. The `DictSet.insert user bucket` expression performs this task, creating a new set based on the existing `bucket` by adding `user` to it. Note, that the existing set is not modified in any way; the `bucket` array does not know anything about the newly created `bucket` copy either. So the array has to be changed too. This is done with the `update` function from the `Array.Extra` package with `update : Int -> (a -> a) -> Array a -> Array a` type signature. The index of the element to change is passed as the first parameter; the array itself – as the last one. In between, `update` function receives modification callback – it takes an array element with type `a` and should return some value with the same type. This callback function will get `bucket` as input. Whatever it returns, will be placed in the array by the original index.

The `let - in` syntax may be a little hard to understand, but look at it in the following way: **let**’s say `newWorkflow` is defined as `{ workflow | votes = ...` – **then** the entire `increment` can be expressed in terms of `newWorkflow` as `step Forward newWorkflow`. So, in essence, this is just syntactic sugar, the explicit name for the part of the expression.

```elm
increment : User -> Workflow -> Workflow
increment user workflow =
    let
        newWorkflow =
            { workflow | votes =
                update workflow.currentStep (\bucket -> insert user bucket) workflow.votes }
    in
        step Forward newWorkflow

type Direction
    = Forward
    | Backward

step : Direction -> Workflow -> Workflow
step direction workflow =
    let
        count =
            case direction of
                Forward ->
                    1
                Backward ->
                    -1
    in
        { workflow | currentStep = workflow.currentStep + count }
```

Wow, feeling a little giddy from all of this... But at least we managed to grasp the essence of `ELM`’s syntax and how `workflow` works. Now it will be much easier to understand testing.

## Traditional Testing

The test structure repeats the scenario in the image at the beginning of the article: `workflow` containing three steps with thresholds 3, 2 and 2, respectively is created – `Workflow.init [ 3, 2, 2 ]`. Next, the pipe operator `|>` takes the result of the expression on the left, that is, the initialized workflow, and inserts it as the last argument into the expression on the right. So you get: `approve (create "User 1" True [ VOTE ]) (Workflow.init [ 3, 2, 2 ])`, which is just `approve`  function call with two arguments: `user` and `workflow`! Without using the pipe operator `|>` you would have to write a lengthy expression with an obscene amount of parentheses (hello, `lisp`). The logic of the tests themselves is seen from their descriptive part (the line after `test`). Before the final vote of “User 9”, `workflow` must be non-finished. It completes only after the final vote.

```elm
traditionalTest : Test
traditionalTest =
    let
        workflow =
            [ 3, 2, 2 ]
                |> Workflow.init
                |> approve (create "User 1" True [ VOTE ])
                |> approve (create "User 1" True [ VOTE ])
                |> approve (create "User 2" True [ VOTE ])
                |> approve (create "User 3" False [ VOTE ])
                |> approve (create "User 4" True [ VOTE ])
                |> approve (create "User 5" True [ NONE, FORCE ])
                |> reject (create "User 6" True [ NONE, NONE, VOTE ])
                |> approve (create "User 7" True [ NONE, VOTE ])
                |> approve (create "User 8" True [ NONE, VOTE ])

        finalUser =
            create "User 9" True [ NONE, VOTE, FORCE ]
    in
        describe "Real world workflow example"
            [ test "Initially, workflow is not finished" <|
              equal False (finished workflow)
            , test "But after approve it becomes finished" <|
              equal True (finished (approve finalUser workflow))
            ]
```

Well, so far we have not seen anything new as the test follows a standard structure: [arrange, act, assert](http://wiki.c2.com/?ArrangeActAssert). Let’s see what property-based testing can offer.

## Property-Based Testing

To begin with, we need to formulate a number of invariants relative to `workflow`. Let’s take the first thing that comes to mind: the step number of the workflow (`currentStep` field) should not be greater than the total number of steps, regardless of any operations with workflow. To check this, it is possible to use the array length of the `stepsConfig` field. For symmetry, we add another requirement: the non-negativity of the step number. Such an invariant in the code can be expressed as: `step >= 0 && step <= (length initWorkflow.stepsConfig)`. The condition `<=` is acceptable on the right boundary, since `workflow` transitioning to the completed state will technically be on the next-to-last step. Now we have to determine what `step` is and how to express the concept “whatever users are doing” in the code.

To verify how `workflow` behaves at the upper boundary, we can loosen up the statement “whatever users are doing” to “however many times different users `approve`” – since `reject` inevitably reduces the step number and `approve` only increases it. That is, if you have a list of voting users, then `step` can be expressed as `(foldl approve initWorkflow users).currentStep`. I agree it looks suspiciously short. I bet you can already imagine a cycle that goes through the list of users and calls the `approve` function at each step. However, the functional nature of the language and careful approach to methods design can simplify things. `foldl` is a widely used higher order function that produces the [fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) operation. Its three parameters are: the folding function `approve`, the initial value of `initWorkflow` and the list that you need to “fold” – `users`. The English equivalent is: starting with `initialWorkflow`, successively call the `approve` function with arguments from the `users` list, in order to get the final `workflow`. Give yourself a pat on the back for choosing the right order of the `approve` function arguments, where the user is received as the first parameter, just like the `fold` contract requires.

Now we have everything to formulate the first property-based test. Notice how easily readable DSL is: claim (`claim`) that the hypothesis is true (`true`) for (`for`) a set of random `users`. Syntax `\(parameters) -> expression` (or `\parameter -> expression` in the case of single argument) is nothing else but a declaration of the anonymous function.

```elm
claim "Workflow should remain in its bounds"
    |> true
        (\users ->
            member (foldl approve initWorkflow users).currentStep <|
                range 0 (length initWorkflow.stepsConfig)
        )
    |> for (list userProducer)
```
Before we delve into the essence of the expression `list userProducer`, let’s see at the test output. It is easy to see that a counterexample that refutes our invariant has been quickly found. I really wanted to believe in the code we wrote with such care, tested in the previous ruby incarnation with the help of mutation tests, but it didn’t turn out to be so great. Looking at the counterexample, it becomes obvious what the problem is: if the user has `FORCE` permission, the code does not do any checks and simply increases the number of the step!

```elm
✗ Workflow should remain in its bounds

    On check 3, found counterexample:
      [User { name = "", active = True, permissions = Array.fromList [VOTE] },
       User { name = "", active = True, permissions = Array.fromList [NONE,FORCE] },
       User { name = "", active = True, permissions = Array.fromList [NONE,NONE,FORCE] },
       User { name = "", active = True, permissions = Array.fromList [NONE,NONE,NONE,FORCE] }]
    Expected:   True
    But It Was: False
```

## Generating Random Data

The expression `list userProducer` has the type `Producer (List User)` that can be read as “Generator of random Lists of Users”. It is achieved by applying combinator `list` (with a type signature `list : Producer a -> Producer (List a)`) to the generator `userProducer` (“Generator of random User” with type `Producer User`). It is easy to build more complex generators on top of the existing ones. By the way, that is how `userProducer` was built:

```elm
userProducer : Producer User
userProducer =
    convert (uncurry3 create)
        toTuple
        (tuple3 ( string, bool, list permissionProducer ))
```

Juggling with tuples (`toTuple`, `tuple3`, `uncurry3`) is a necessary boilerplate machinery. The main thing here is that we “explain” how to construct a user with `create` function, utilizing two primitive generators: `string` and `bool`, as well as the nested generator – `list permissionProducer`. Another important peculiarity is the “explanation” of how to deconstruct a user into 3-component tuple. For this purpose we have the `toTuple` function, which by getting the `user` as input and returns the tuple with three elements. 

```elm
toTuple : User -> ( String, Bool, List Permission )
toTuple user =
    ( (name user), (active user), (permissions user) )
```

This “knowledge” of how to “break down” the user into primitive elements helps find bugs in the code so quickly. The names and amount of users, their permissions and all the other aspects of the counterexample seem very “minimal”: the blank user name, absence of other users in the list except for the really necessary ones, etc. The testing library has been simplifying every aspect of the initial counterexample until it was possible, breaking each `Producer` into parts, simplifying them and then collecting parts back.

## Fixing Code

Let’s rewrite the `approve` function to get rid of the error. Now the check is applied not only to the flag of user inactivity, but to the completeness of `workflow`, as well as to the fact of whether the user has voted on the current step.

```elm
approve : User -> Workflow -> Workflow
approve user workflow =
    if locked user workflow then
        workflow
    else
        case User.permission workflow.currentStep user of
            FORCE ->
                increment user workflow

            VOTE ->
                vote user workflow

            NONE ->
                workflow


locked : User -> Workflow -> Bool
locked user workflow =
    let
        currentBucket = currentStepVotes workflow
    in
        any identity
            [ finished workflow
            , User.inactive user
            , member user currentBucket
            ]
```

Thanks to the fix, the test run is now successful, there are no more errors. Let’s try to formulate more invariants for verification. The last bullet point `workflow` requirements states that the user should be able to vote only once at each of the steps, so let’s express this in the code. This time, let’s use a different syntactic construction: “`claim` description `that` expression `is` equivalent `for` generator”. That is, for each pair of user-operation the code attempts to apply this operation to `workflow`, at the step on which the user with the same name has already voted. The invariant is: current step number of the `workflow` should not be affected.

```elm
workflowVotedByUser : User -> Workflow
workflowVotedByUser user =
    let
        userWithSameName =
            create (name user) True [ VOTE, VOTE ]
    in
        initWorkflow |> twice (approve userWithSameName)


claim "Same user cannot vote twice"
    |> that (\( user, operation ) -> (workflowVotedByUser user |> apply operation user).currentStep)
    |> is (\( user, operation ) -> (workflowVotedByUser user).currentStep)
    |> for (tuple ( userProducer, operationProducer ))
```

The test execution shows that the invariant is broken: there is a pair of user-operation, which, when applied, causes workflow step number to decrease by one.

```elm
✗ Same user cannot vote twice

    On check 1, found counterexample:
      (User { name = "", active = False, permissions = Array.fromList [NONE,VOTE] },Reject)
    Expected:   1
    But It Was: 0
```

And indeed, the `reject` function contains an error. Using `locked` instead of `not User.active` fixes the error, and all tests are now green.

```elm
Running 4 tests. To reproduce these results, run: elm-test --seed 2042167370

TEST RUN PASSED

Duration: 5 ms
Passed:   4
Failed:   0
```

## Conclusion

With just two simple invariants we managed to uncover several non-trivial problems in `workflow` implementation. It’s difficult to find them using traditional methods: developers can, and will check whether or not it’s possible to vote a second time on the same step, but it is unlikely they will try to do `reject` immediately after the vote in the unit tests. By the way, in `ruby` implementation, tested by [mutant](https://github.com/mbj/mutant), the same defects have shown up, which proves once again that the “silver bullet” does not exist and relying on just one method of testing is naive.

There are libraries for property-based testing not only for languages with only functional paradigm: for `ruby` it’s [rantly](https://github.com/abargnesi/rantly) and [propr](https://github.com/kputnam/propr), for `python` – [hypothesis](https://github.com/HypothesisWorks/hypothesis-python), for Java – [JavaQuickCheck](https://java.net/projects/quickcheck/pages/Home), etc.

As I see it, the main obstacle for using property-based tests is not the complexity in finding good invariants (even though it's really not easy at all) and not even the lack of tools developed for some programming languages, but rather poor developers awareness and knowledge of the topic. Hopefully, our example with `workflow` managed to spark interest in and draw attention to the topic of the powerful property-based testing approach.

The source code in Elm can be found in a public [repository](https://github.com/maksar/elm-workflow). 
