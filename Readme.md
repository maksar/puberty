How to make your inner-programmer beard longer with property-based testing.

Основниые разделы
  [x] Введение
  [ ] Строгая типизация и вывод типов
  [ ] Как при помощи строгой типизации и вывода типов можно генерировать тестовые данные.
  [ ] Как работает property-based тестирование и для чего оно нужно
  [ ] Разбор рельного примера использования


Введение

Современная разработка программного обеспечения не обходится без использования многих видов автоматических тестов. Сюда входят как [самые | зачем уточнение?] базовые Unit и Integration тесты, так и более сложные Performance и Penetration тесты и многие другие. Польза от автоматизации процессов тестировани давно перестала быть предметом обсуждения в менеджерских кругах. Команды, практикующие автоматизацию тестирования, следующие методологиям TDD и BDD могут быстрее поставлять работающие системы со значительно меньшим количеством ошибок. Что делает их, в конечном счете, более конкурентно способными на перегретом рынке коммерческой разработки программных систем.

Существуют такие предметные области, где качество и стабильность работы всей системы или [избранных | плохое слово] ее частей – первоочередная задача. В таких случаях разработчики могут (и должны) применять более мощные и строгие инструменты и методы автоматических проверок кода. Отличными примерами таких формальных методов верификации программ являются два ортогональных друг-другу подхода – мутационное тестирование (mutation testing) и инвариантное тестирование (property-based testing).

КАРТИНКА Data -> Program -> Result

[Суть метода мутационного тестирования заключается в намеренном изменении кода программы всеми возможными способами и последущей верификации того, что логика ее работы изменилась | странная формулировка]. Если изменение кода программы не привело к "падению" какого-либо unit или integration теста, то можно сделать вывод либо о том, что изменяемый участок кода вообще бесполезен (не используется), либо тесты не содержат примера, приводящего к задействованию этого участка кода. В любом случае, [ситуацию исползуют | используют + "ситуация"] как индикатор обратить внимание на всесторонность тестов либо качество кода.

Инвариантное тестирование, [действует с другой стороны | формулировка]. Суть в генерации псевдо-случайных данных, поступающих на вход программе и последующей проверке набора свойств (инвариантов) на результате. Примером самого простого инварианта может служить отсутствие исключительных ситуаций в процессе работы программы [расплывачато]. То есть [что бы ни было подано на вход – программа отрабатывает без аварийного завершения | уточняй].

[То есть | повторение, да и без него норм] получается, что мы меняем либо данные, поступающие на вход программы, либо саму структуру программы и смотрим что из этого вышло. Оба способа являются достаточно сложными в применении, но очень мощными инструментами в арсенале инженера по разработке программных систем. Цель этой публикации – познакомиться с сущностью и особенностями property-based тестирования на примере создания Workflow движка с использованием функционального языка программирования ELM.


Строгая типизация и вывод типов

ELM относится к статически типизированным, функциональным языках программирования. Применительно к статье, это означает, что аргументы и возвращаемое значение функции в ELM всегда принадлежат к [какому-то одному типу данных | скорее к заранее определенным типам данных]. Более того, в ELM все функции являются "чистыми", то есть не содержат так называемых "побочных эффектов". [На практике это означает, что программы на ELM легче верифицировать, так как можно быть абсолютно уверенным в том, что вызывая произвольную функцию о одним и тем же списком параметров – она всегда вернет один и тот же результат, не внеся при этом никаких изменений с состояние системы. | я бы прям с вкик забрал определение чистых функций, там оно явно более строгое]

Такие преимущества свойственны многим функциональным языкам [в той или иной степени | можно без уточнения]. Однако существует и цена, которую приходится платить – от разработчиков требуется высокая дисциплина и умение мыслить в "функциональном" стиле. Больше не удасться вернуть из функции `null` там, где это удобно, не пройдут и трюки с приведением типов... Приходится уделять намного больше внимания доменной модели предметной области, моделировать пространство состояний и процессы через систему типов.

Хватит пустых слов, покажите мне код! Окей, окей. Мы рассмотрим пример довольно простого (но в то же время не тривиального) движка Workflow-ов из моей предыдущей статьи: http://www.i-programmer.info/programming/methodology-a-testing/10006-keep-calm-and-kill-mutants.html

ПРАВИЛА РАБОТЫ ДВИЖКА ИЗ ПРЕДЫДУЩЕЙ СТАТЬИ + GIF

С целью упрощения понимания исходного кода, я решил не "изобретать велосипед" и попытался перевести код на `ruby` в `ELM` один-в-один. Конечно, "перевод" очень значительно отличается от "оригинала", все же и синтаксис совершенно иной и подход. Однако хочется верить, что это даст возможность опелировать к более привычному императивному ruby при возникновении сложностей.

Итак, присутупая к реализации начем с определения самого главного типа – `Workflow`. [Это структура (record), состоящая из набора полей: | классы хочу, я рубист]

```elm
type alias Workflow =
    { stepsConfig : Array Int -- массив целых чисел, содержащий количество необходимых голосов на каждом шаге
    , currentStep : Int -- текущий номер шага, на котором находится данный workflow
    , votes : Array (GenericSet User) -- массив множеств пользователей, простыми словами список пользователей, проголосовавших на каждом из шагов
    }
```

Похожим образом определим и "пользователя" (User). Обратите внимание, что User это полноценный тип (в отличие от Workflow, который по сути является именованной структурой данных). Первое упоминание `User` – это, собственно, название типа, а второе – это имя конструктора типа. [В большинстве случаем программисты осознанно делают их одинаковыми. | может все же convention?]
```elm
type User
    = User
        { name : String -- строковое имя пользователя, используется для идентификации (пример все-таки учебный)
        , active : Bool -- флаг активности пользователя (помним, что неактивные пользователи не могут принимать участия в голосовании)
        , permissions : Array Permission -- список (массив) прав пользователя, сам типа Permission определен как type Permission = VOTE | FORCE | NONE
        }
```

Далее, рассмотрим один из возможных путей исполнения программы – получение одобрения от пользователя с расширенными правами на каком-либо шаге процесса. Работа начинается с вызова функции `approve` (для сравнения – ссылка на соответствующую `ruby` реализацию). Функция принимает два аргумента: `User` и `Workflow` и всегда возвращает `Workflow`. Если пользователь не активен (`not (User.active user)`), то результат работы – неизмененный `workflow`. Иначе, просходит сопоставление с образцом по результату вычисления выражения `User.permission workflow.currentStep user` – вызов функции `permission` из пакета `User` с аргументами `workflow.currentStep` и `user`. Выглядит странно, но помогает IDE [почему? стат типизация, аннотации, бла-бла-бла], услужливо посказывающая сигнатуру функции `permission`: `permission : Int -> User -> Permission`. Тип `Permission` [является алгебраической суммой | я на руби пишу, а не профессор матемматики] и компилятор требует он нас явно определить что делать в каждом случае.
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

Так как мы договорились, что голосующий пользователь имеет расширенный набор прав, то ход исполнения программы продолжится вызовом функции `increment`. Сигнатура у этой функции такая же как и у `approve` – пользователь на первом месте, а `workflow` на втором. Выбор такого порядка следования аргументов не случаен. Дело в том, что в `ELM` (впрочем как и в других функциональных языках) широкое применение нашли `pipeline` операторы – `|>` и `<|`. С их помощью удобно выстраивать цепочки вызовов, когда возвращаемое значение одной функции передается в следующую. Итак, `increment`, используя вспомогательную функцию `step`, конструирует новый экземпляр `newWorkflow`, где номер шага увеличен на единицу. `newWorkflow`, в свою очередь, тоже копия `workflow`-а из аргументов функции, но с измененным полем `votes`. В императивном языке мы бы могли просто обновить один из элементов этого массива... Однако в функциональных языках данные, как правило, не мутируемы – поэтому нельзя просто так взять и добавить голос в множество голосов шага. Нужно сконструировать новое множество на основании существующего, в котором добавлен новый элемент. Этим занимается конструкция `GenericSet.insert user bucket`, которая и создает новое множество на основе существуещего `bucket`, добавляя в него `user`. Примечательно, что создание нового экземпляра множества никак не приводит из изменению старого, массив `bucket`-ов тоже ничего не знает о созданной копии `bucket`-а. Так что приходится модифицировать и массив тоже. Делается это с помощью функции `update` из пакета `Array.Extra` с сигнатурой `update : Int -> (a -> a) -> Array a -> Array a﻿`. Индекс элемента, который подвергается изменению передается первым параметром, сам массив передается в третьем, а второй – это функция изменения элемента. То есть такая функция, куда на вход поступит `bucket`, а то, что получится на выходе и будет вставлено в массив на его место (возвращаемое значение, понятное дело, должно быть того же типа). Конструкция `let - in` может быть слегка непривычна для понимания, но если посмотреть на нее как на: допустим `newWorkflow` определен как ... - тогда весь `increment` можно выразить в терминах `newWorkflow` как ... То есть по сути это всего-лишь синтаксический сахар, явное именование части выражения.
```elm
increment : User -> Workflow -> Workflow
increment user workflow =
    let
        newWorkflow =
            { workflow | votes = update workflow.currentStep (\bucket -> GenericSet.insert user bucket) workflow.votes }
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

Фух, прямо голова кругом пошла от всего этого... Но зато немного разобрались в синтаксисе `ELM` и сути работы `workfow`-а. Вникнуть в тестирование будет гораздо легче.
Нетрудно заметить, что структура теста повторяет сценарий на картинке в начале статьи: создается `workflow`, состоящий из трех шагов с порогами 3, 2 и 2 соответственно – `Workflow.init [ 3, 2, 2 ]`. Далее, оператор `|>` берет результат выражения слева, то есть проинициализированный `workflow` и подставляет его последним операндом в выражение справа. То есть получается: `approve (create "User 1" True [ VOTE ]) (Workflow.init [ 3, 2, 2 ])`, а это и есть вызов функции `approve` с двумя аргументами – пользователем и `workflow`-ом! Без использования `|>` (обычно его называют `pipe`) пришлось бы писать огромное выражение с просто неприличным количестом круглых скобок (привет, `lisp`). Логика самих проверок видна из их описательной части. До финального голосования пользователем "User 9", `workflow` не должен быть завершенным, становится он таким только после финального голосования.
```elm
traditionalTests : Test
traditionalTests =
    let
        workflow =
            Workflow.init [ 3, 2, 2 ]
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
        ElmTest.suite "Real world workflow example"
            [ ElmTest.test "Initially, workflow is not finished" <|
                ElmTest.assert (not (finished workflow))
            , ElmTest.test "But after approve it becomes finished" <|
                ElmTest.assert (finished (approve finalUser workflow))
            ]
```
