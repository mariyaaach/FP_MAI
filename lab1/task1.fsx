open System

// Заданные параметры
let a = 0.1
let b = 0.6
let step = 0.1
let eps = 1e-10

// Функция, используя встроенные функции F#
let builtinFunction x = Math.Exp(Math.Cos(x)) * Math.Cos(Math.Sin(x))

// Наивный метод ряда Тейлора для e^(cosx)
let taylorSeriesNaive x =
    let mutable sum = 1.0 // Первый член ряда
    let mutable term = 1.0 // Текущий член ряда
    let mutable n = 0
    while Math.Abs(term) > eps do
        term <- term * Math.Cos(x) / float(n + 1)
        sum <- sum + term
        n <- n + 1
    (sum, n)

// Улучшенный метод ряда Тейлора, используя предыдущий член
let taylorSeriesSmart x =
    let mutable sum = 1.0
    let mutable term = 1.0
    let mutable n = 0
    while Math.Abs(term) > eps do
        term <- term * Math.Cos(x) / float(n + 1)
        sum <- sum + term
        n <- n + 1
    (sum, n)

printfn "x\t\tBuiltin\t\t\SmartTaylor\t#terms\tNaiveTaylor\t#terms"
for x in [a..step..b] do
    let builtin = builtinFunction x
    let (smartSum, smartTerms) = taylorSeriesSmart x
    let (naiveSum, naiveTerms) = taylorSeriesNaive x
    let formatted = sprintf "%-8f\t%-12f\t%-12f\t%-8d\t%-12f\t%-8d" x builtin smartSum smartTerms naiveSum naiveTerms
    printfn "%s" formatted
