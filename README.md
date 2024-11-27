# Io

## Лабораторная работа №3

- **Студент:** Наумов Павел
- **Группа:** P3318
- **ИСУ:** 367428

---

**Цель:** получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

- В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- Обязательно должна быть реализована линейная интерполяция (отрезками, `link`);
настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

- Какие алгоритмы использовать (в том числе два сразу);
частота дискретизации результирующих данных;
и т.п.;


входные данные должны задаваться в текстовом формате на подобии `.csv` (к примеру `x;y\n` или `x\ty\n`) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию `x`;

выходные данные должны подаваться на стандартный вывод;

программа должна работать в потоковом режиме (пример -- `cat | grep 11`), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

## Детали

1. **Структура проекта:**

   - Модульная организация с разделением на компоненты ввода/вывода и алгоритмы интерполяции
   - Асинхронная обработка данных, при помощи сообщений между процессами
   - Потоковая обработка входных данных

3. **Ключевые функции:**

   - Парсинг аргументов командной строки - `fp_lab3.erl`
   - Потоковая обработка входных данных - `input.erl`
   - Линейная интерполяция - `linear.erl`
   - Интерполяция методо Лагранжа - `lagrange.erl`
   - Потоковый вывод результатов - `output.erl`

4. **Алгоритмы интерполяции:**

   - **Линейная интерполяция с окном из двух точек**

   - **Интерполяция Лагранжа с окном из 4 точек**

## Релевантные части кода:

Линейная интерполяция

```erlang
loop(Window, Step, Output) ->
    receive
        {X, Y} ->
            case length(Window) > 0 of
                true ->
                    [{X1, Y1}] = lists:nthtail(length(Window) - 1, Window),
                    Output ! {"linear", linear({X1, Y1}, {X, Y}, Step)},
                    loop([{X1, Y1}, {X, Y}], Step, Output);
                false ->
                    loop([{X, Y}], Step, Output)
            end;
        eof ->
            Output ! eof,
            ok;
        _ ->
            loop(Window, Step, Output)
    end.

linear({X1, Y1}, {X2, Y2}, Step) ->
    Steps = round((X2 - X1) / Step),
    Xs = lists:map(fun(I) -> X1 + I * Step end, lists:seq(0, Steps)),
    lists:map(fun(X) -> calc({X1, Y1}, {X2, Y2}, X) end, Xs).

calc({X1, Y1}, {X2, Y2}, X) ->
    Y = (Y1 * (X2 - X) + Y2 * (X - X1)) / (X2 - X1),
    {X, Y}.
```

Методом Лагранжа

```erlang
loop(Window, Step, Output) ->
    receive
        {X, Y} ->
            case length(Window) > 2 of
                true ->
                    NewWindow = Window ++ [{X, Y}],
                    Points = lists:nthtail(length(NewWindow) - 4, NewWindow),
                    Output ! {"lagrange", lagrange(Step, Points)},
                    loop(Points, Step, Output);
                false ->
                    loop(Window ++ [{X, Y}], Step, Output)
            end;
        eof ->
            Output ! eof,
            ok;
        _ ->
            loop(Window, Step, Output)
    end.

lagrange(Step, Points) ->
    [{X0, _} | _] = Points,
    {Xn, _} = lists:last(Points),
    Steps = round((Xn - X0) / Step),
    Xs = lists:map(fun(I) -> X0 + I * Step end, lists:seq(0, Steps)),
    lists:map(fun(X) -> calc(Points, X) end, Xs).

calc(Points, X) ->
    Li = fun(Xi) ->
            NonEqual = lists:filter(fun({Xj, _}) -> Xj =/= Xi end, Points),
            Top = lists:foldl(fun({Xj, _}, Acc) -> Acc * (X - Xj) end, 1, NonEqual),
            Bottom = lists:foldl(fun({Xj, _}, Acc) -> Acc * (Xi - Xj) end, 1, NonEqual),
            Top / Bottom
         end,

    Y = lists:foldl(fun({Xi, Yi}, Acc) -> Acc + Yi * Li(Xi) end, 0, Points),
    {X, Y}.
```

## Пример работы

1. **Запуск с обоими алгоритмами:**

   ```bash
   ./_build/default/bin/fp_lab3 -algorithm linear,lagrange -step 1
   ```

3. **Ввод(обозначен символом >)/вывод:**

   ```text
    Enter lines with two floats separated by space (Ctrl+D to exit)
    >0 0.00
    >1.571 1
    linear:
    0.00	1.00	2.00
    0.00	0.64	1.27
    >3.142 0
    linear:
    1.57	2.57	3.57
    1.00	0.36	-0.27
    >4.712 -1
    linear:
    3.14	4.14	5.14
    0.00	-0.64	-1.27
    lagrange:
    0.00	1.00	2.00	3.00	4.00	5.00
    0.00	0.97	0.84	0.12	-0.67	-1.03
   ```

## Выводы

Я научился работать с процессами в Erlang и делать потоковые программы
