# Как нашлись баги в программе без ошибок

Однажды я решил рассказать о [мутационном тестировании](https://en.wikipedia.org/wiki/Mutation_testing). Выдумал пример, написал код на Ruby, прогнал тесты и исправил ошибки. Программа работала, [статья вышла](https://github.com/maksar/mentat) — я был доволен.

Казалось, для рассказа об [инвариантном тестировании](http://hypothesis.works/articles/what-is-property-based-testing) понадобится новый пример, с новыми ошибками. Но лень меня спасла: я запустил инвариантное тестирование на старой задаче и, сюрприз, в коде нашлись новые баги.

Это история для опытных разработчиков, которые ленятся выйти за рамки стандартных unit тестов. О пагубной самоуверенности, инвариантном тестировании и синтаксисе [ELM](http://elm-lang.org). Читать будет сложно, но полезно. Поехали.

## Описание задачи

В статье о мутационном тестировании был простой, но нетривиальный пример — движок `workflow`. Вот краткое описание:

- На каждом шаге есть **пороговое число** голосов.
- Чтобы **перейти на следующий шаг**, голосов должно быть **не меньше порогового значения**.
- Пользователи с правом пропуска могут **пропустить один шаг** (начать следующий без голосования).
- Пользователи с правом голоса могут **отменить** текущий шаг (повторить предыдущий).
- **Неактивные** пользователи не могут *голосовать* или *отменять* шаг.
- Пользователь голосует только **один раз** на каждом шаге

![](/images/workflow-animation.gif?raw=true)

Менять логику я не буду — ошибки есть и тут. Но чтобы их найти я переписал код на `ELM`.

## Выбор языка

В Ruby есть библиотеки для инвариантного тестирования — [rantly](https://github.com/abargnesi/rantly) и [propr](https://github.com/kputnam/propr). Но я недавно изучил `ELM` и мне хотелось его где-нибудь применить. Так что выбрать было просто — я перевел исходный код на `Ruby` в `ELM` один-в-один. Конечно, синтаксис и подход изменились, но так я могу апеллировать к привычному императивному Ruby при возникновении сложностей.

Тут важно, что аргументы и возвращаемое значение функции в `ELM` описываются конкретными типами данных, а все функции не содержат «побочных эффектов». Функция с одним и тем же набором аргументов всегда возвращает один и тот же результат, без изменений в общем состоянии системы.

В `ELM` не выйдет вернуть из функции null, не пройдут и трюки с явным приведением типов. Придётся моделировать процессы из предметной области и заботиться, чтобы недопустимые состояния модели нельзя было выразить в коде. В общем, мыслить в «функциональном» стиле.

Зато он подойдет для инвариантного тестирования.
**ПОЧЕМУ?**

Хватит пустых слов, покажите мне код! Окей, окей. Откройте [исходный код](https://github.com/maksar/elm-workflow) на `ELM` в соседнем окне — я расскажу, что к чему.

## Определение типов данных

Начнем с определения самого главного типа: `workflow`. Это структура (`record`), состоящая из набора полей.

```elm
type alias Workflow =
    { stepsConfig : Array Int -- массив целых чисел, содержащий количество
                              -- необходимых голосов на каждом шаге
    , currentStep : Int       -- текущий номер шага, на котором находится данный workflow
    , votes : Array Bucket    -- массив множеств пользователей, иными словами список пользователей,
                              -- проголосовавших на каждом из шагов
    }
```

Похожим образом определим и «пользователя» (`User`). Обратите внимание, что `workflow` — это именованная структурой данных, а вот `User` — полноценный тип. Первое упоминание `User` — название типа, а второе — имя конструктора типа. Для удобства я назвал их одинаково

```elm
type User
    = User
        { name : String                  -- строковое имя пользователя, используется
                                         -- для идентификации (пример все-таки учебный)
        , active : Bool                  -- флаг активности пользователя (помним,
                                         -- что неактивные пользователи не могут принимать
                                         -- участия в голосовании)
        , permissions : Array Permission -- список (массив) прав пользователя, тип Permission
                                         -- определен как type Permission = VOTE | FORCE | NONE
        }
```

## Реализация

Вот один из возможных путей исполнения программы — получение одобрения от пользователя с расширенными правами на каком-либо шаге процесса. 

**Работа начинается с вызова функции `approve`** (вот соответствующая [`ruby` реализация](https://github.com/maksar/mentat/blob/master/version_6/workflow.rb#L10-L17)). Функция принимает два аргумента: `User` и `workflow`. Возвращает — всегда `workflow`. Если пользователь не активен `not (User.active user)`, то результат работы — неизмененный `workflow`. Иначе, происходит сопоставление с образцом (pattern matching) по результату вычисления выражения `User.permission workflow.currentStep user` — вызов функции permission из пакета `User` с аргументами `workflow.currentStep` и `user`. 

Выглядит странно, но помогает IDE, услужливо подсказывающая сигнатуру функции `permission`: `permission : Int -> User -> Permission`. Тип `Permission` — это алгебраическая сумма и компилятор требует явно определить что делать в каждом случае.

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

**Дальше вызываем функцию `increment`**. Сигнатура у этой функции такая же как и у `approve` – пользователь на первом месте, а `workflow` на втором. Через вспомогательную функцию `step`, `increment` конструирует новый экземпляр `newWorkflow`, где номер шага увеличен на единицу. `newWorkflow`, в свою очередь, тоже копия `workflow` из аргументов функции, но с измененным полем `votes`.

В `Ruby` мы бы могли просто обновить элемент этого массива. Но в `ELM` данные неизменяемые (immutable) — нельзя добавить голос в множество голосов шага. Нужно сконструировать новое множество на основании существующего, в котором добавлен новый элемент. Этим занимается конструкция `DictSet.insert user bucket`, которая и создает новое множество на основе существующего `bucket`, добавляя в него `User`.

Создание нового экземпляра множества никак не приводит к изменению старого, массив `bucket`-ов ничего не знает о созданной копии одного `bucket`-а. Так что приходится модифицировать и массив тоже. Делается это с помощью функции `update` из пакета `Array.Extra` с сигнатурой `update : Int -> (a -> a) -> Array a -> Array a﻿`. Индекс элемента, который подвергается изменению передается первым параметром, вторым — функция изменения элемента, а сам массив передается в конце. То есть такая функция, куда на вход поступит `bucket`, а то, что получится на выходе и будет вставлено в массив на его место. Конечно, возвращаемое значение должно быть того же типа.

Конструкция `let - in` непривычна для понимания, но если посмотрите на нее как на: допустим `newWorkflow` определен как `{ workflow | votes =...` — весь increment можно выразить в терминах `newWorkflow` как step `Forward newWorkflow` То есть это всего-лишь синтаксический сахар, явное именование части выражения.

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

Фух, прямо голова кругом пошла от всего этого. Зато немного разобрались в синтаксисе `ELM` и сути работы `workfow`-а. Сделайте перерыв, выпейте кофе и читайте дальше. Вникнуть в тестирование будет проще.

## Тестирование

Для традиционного тестирования структура теста повторяет сценарий на картинке в начале статьи: создается `workflow`, состоящий из трех шагов с порогами 3, 2 и 2 соответственно – `Workflow.init [ 3, 2, 2 ]`. 

Далее, оператор |> берет результат выражения слева, то есть проинициализированный `workflow`, и подставляет его последним аргументом в выражение справа. Получается: `approve (create "User 1" True [ VOTE ]) (Workflow.init [ 3, 2, 2 ])`, а это и есть вызов функции approve с двумя аргументами – пользователем и `workflow`.

Логика самих проверок видна из их описательной части (строка после `test`). До финального голосования пользователем User 9, `workflow` не должен быть завершенным, становится он таким только после финального голосования.

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

Ну, пока что мы не увидели ничего нового – тест подчиняется стандартной структуре: [arrange, act, assert](http://wiki.c2.com/?ArrangeActAssert).

**Для инвариантного тестирования** нужно сформулировать несколько инвариантов относительно какого-нибудь `workflow`. Например, номер шага на котором он находится (`currentStep`) не должен быть больше количества шагов (можно использовать длину массива из `stepsConfig`), что бы пользователи ни делали. Для симметрии, добавим еще требование о неотрицательности номера шага.

Такой инвариант в коде можно записать как условие `step >= 0 && step <= (length initWorkflow.stepsConfig)`. Условие <= на правой границе приемлемо, ведь переходя в завершенное состояние `workflow` формально будет находиться на следующем за последним шагом. Осталось определить что такое `step` и выразить в коде концепцию «что бы пользователи ни делали».

`reject` неизбежно приводит к уменьшению номера шага, а `approve` только к увеличению. Так что для проверки по верхней границе можно чуть ослабить утверждение «что бы пользователи ни делали» до «сколько бы разные пользователи ни `approve`-али». То есть если где-то взять список голосующих пользователей, то `step` можно выразить как `(foldl approve initWorkflow users).currentStep`.

Согласен, выглядит подозрительно коротко. Кто-то уже представил в голове цикл, который пробегает по пользователям и вызывает на каждом шаге функцию голосования. Но функциональная природа `ELM` и аккуратный подход к дизайну методов все упрощают.

`foldl` – это функция высшего порядка, которая производит операцию свертки. Ее три параметра это – функция преобразования approve, начальное значение `initWorkflow` и список, который требуется "свернуть" `users`. Переводя на человеческий язык: начиная с `initialWorkflow`, последовательно вызвать функцию `approve` с аргументами из списка `users`, чтобы получился итоговый `workflow`. Похвалим себя за правильный выбор порядка аргументов функции `approve` — пользователь передается первым аргументов, как того требует контракт «свертки».

Теперь мы готовы для формулирования первого `property-based` теста. Обратите внимание как натурально читается DSL: утверждаю (`claim`) что гипотеза верна (`true`) для (`for`) набора случайных пользователей. Синтаксис `\(параметры) -> выражение` (или `\параметр -> выражение в случае одного аргумента`) не что иное, как объявление анонимной функции.

```elm
claim "Workflow should remain in its bounds"
    |> true
        (\users ->
            member (foldl approve initWorkflow users).currentStep <|
                range 0 (length initWorkflow.stepsConfig)
        )
    |> for (list userProducer)
```

Запускаем. Видно, что довольно быстро нашелся контрпример, опровергающий сформулированный нами инвариант. Вот так — я протестировал эту программу вдоль и поперек мутационными тестами, а ошибки всё равно остались. Глядя на контр-пример, становится очевидно в чем проблема — если пользователь имеет FORCE привилегии, то код не делает никаких проверок и просто-напросто увеличивает номер шага!

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

## Разбор ошибки

Чтобы понять, что же пошло не так, надо углубиться в суть выражения `list userProducer`.

Выражение `list userProducer` имеет тип `Producer (List User)`, что можно прочитать как «Генератор списка пользователей». Он получается применением комбинатора `list` (имеющего сигнатуру типа `list : Producer a -> Producer (List a)`) к генератору `userProducer` (с типов `Producer User`). Имея генератор, можно легко построить на его основе более сложный. Так и был построен `userProducer`:

```elm
userProducer : Producer User
userProducer =
    convert (uncurry3 create)
        toTuple
        (tuple3 ( string, bool, list permissionProducer ))
```

Жонглирование кортежами (`tuple`) — всего-лишь вынужденная машинерия. Главное тут то, что мы «объясняем» как сконструировать пользователя при помощи функции `create`, используя два примитивных генератора: `string` и `bool`, а также результат другой комбинации — `list permissionProducer`. Еще одной важной особенностью является также «объяснение» — как, зная пользователя, извлечь кортеж его трех составляющих. Для этого служит функция `toTuple`, которая получив пользователя на вход, возвращает кортеж из трех элементов.

```elm
toTuple : User -> ( String, Bool, List Permission )
toTuple user =
    ( (name user), (active user), (permissions user) )
```
Именно «знание» о том, как «разобрать» пользователя на примитивы помогло найти ошибки в коде. Все остальные аспекты контрпримера выглядят маловероятными — пустое имя пользователя, отсутствие других пользователей в списке кроме действительно необходимых. Код библиотеки тестирования упрощал каждый аспект начального контрпримера до упора, разбивая каждый из `Producer`-ов на составные части, упрощая их и собирая обратно.

## Исправление кода

Перепишем функцию `approve` так, чтобы избавиться от ошибки. Теперь проверке подвергаются: флаг неактивности пользователя, завершенность `workflow` и факт того, голосовал ли пользователь на текущем шаге.

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

С таким исправлением реализации запуск тестов завершается успешно, никаких ошибок больше нет. Но я не хочу повторения истории с мутационным тестированием — давайте сформулируем еще инварианты для проверки. 

Последний пункт требований к `workflow` гласит, что пользователь должен иметь возможность голосовать только один раз на каждом из шагов — выразим это в коде. В этот раз используем другую синтаксическую конструкцию: «`claim` описание `that` выражение `is` эквивалент `for` генератор». То есть для каждой пары пользователь-операция код пытается применить эту операцию к `workflow`, на текущем шаге которого пользователь с таким же именем уже голосовал. Инвариант заключается в том, что шаг `workflow` измениться при этом не должен.

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

Запуск тестов показывает, что инвариант нарушен – найдена такая пара пользователь-операция, при применении которой шаг `workflow` уменьшается на единицу.

```elm
✗ Same user cannot vote twice

    On check 1, found counterexample:
      (User { name = "", active = False, permissions = Array.fromList [NONE,VOTE] },Reject)
    Expected:   1
    But It Was: 0
```

Функция `reject` тоже с ошибкой! Применение `locked` вместо `not User.active` исправляет ситуацию — все тесты теперь зеленые.

```elm
Running 4 tests. To reproduce these results, run: elm-test --seed 2042167370

TEST RUN PASSED

Duration: 5 ms
Passed:   4
Failed:   0
```

## Что с этим делать

Итак, у нас была программа, вдоль и поперек проверенная мутационным тестированием. Всего два простых инварианта — и в ней обнаружилось несколько нетривиальных проблем в реализации `workflow`. Найти их традиционными способами трудно — разработчик может и проверит, что голосовать второй раз на одном и том же шаге не получится, однако вряд ли попытается сделать `reject` сразу после голосования в коде unit тестов. 

Наивно полагаться только на один способ проверки. Если на проекте важна стабильность работы системы, то тестируйте её и стандартными тестами, и мутационными, и инвариантными. Придётся разобраться в автоматическом тестировании или даже переписать свой код на другом языке — перечитайте эту статью, изучите [прошлую](https://github.com/maksar/mentat).

Продвинутые тесты — это сложно и вы уж сами решайте, нужно ли тратить на это силы. Проекты бывают разные, но жизнь слишком коротка, чтобы фиксить баги, которые вы пропустили при разработке.

