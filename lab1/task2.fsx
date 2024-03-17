open System

// Определение функций уравнений и их производных
let f1 (x:float) = Math.Sin(Math.Log(x)) - Math.Cos(Math.Log(x)) + 2.0 * Math.Log(x)
let f2 (x:float) = x - 2.0 + Math.Sin(1.0 / x)
let f3 (x:float) = Math.Exp(x) + Math.Log(x) - 10.0 * x

let df1 (x:float) = 1.0 / x * (Math.Cos(Math.Log(x)) + Math.Sin(Math.Log(x))) + 2.0 / x
let df2 (x:float) = 1.0 - Math.Cos(1.0 / x) / (x*x)
let df3 (x:float) = Math.Exp(x) + 1.0 / x - 10.0

// Метод дихотомии
let rec dichotomy f (a:float) (b:float) eps =
    if Math.Abs(b - a) < eps then (a + b) / 2.0
    else
        let c = (a + b) / 2.0
        if f a * f c <= 0.0 then dichotomy f a c eps
        else dichotomy f c b eps

// Общий метод итераций
let iterationMethod phi (x0:float) eps =
    let rec loop x =
        let next = phi x
        if Math.Abs(next - x) < eps then next
        else loop next
    loop x0

// Метод Ньютона (особый случай метода итераций)
let newton f df (x0:float) eps =
    let phi x = x - f(x) / df(x)
    iterationMethod phi x0 eps 

let eps = 1e-6 

// Примерная функция phi для метода итераций (для f1, f2 уже определена, для f3)
let phi1 (x:float) = x - f1(x) / df1(x) // Преобразование метода Ньютона в файл форму для итераций 
let phi2 (x:float) = 2.0 - Math.Sin(1.0 / x) 
let phi3 (x:float) = x - f3(x) / df3(x) // Преобразование метода Ньютона в файл форму для итераций 

// Верхний заголовок таблицы
printfn "------------------------------------------------------------------------------------------------"
printfn "| Уравнение                          | Метод     | Отрезок/Н.П. | Результат            |"
printfn "------------------------------------------------------------------------------------------------"

// Вспомогательная функция для вывода строк таблицы
let printRow equation method range (result:float) =
    printfn "| %-33s | %-9s | %-10s | %-19f |" equation method range result

// Вывод результатов для f1
printRow "f1: sin(lnx) - cos(lnx) + 2lnx = 0" "Дихотомия" "1.0-3.0"   (dichotomy f1 1.0 3.0 eps)
printRow "f1: sin(lnx) - cos(lnx) + 2lnx = 0" "Итерации"  "x0=2.0"    (iterationMethod phi1 2.0 eps)
printRow "f1: sin(lnx) - cos(lnx) + 2lnx = 0" "Ньютон"    "x0=2.0"    (newton f1 df1 2.0 eps)

// Вывод результатов для f2
printRow "f2: x - 2 + sin(1/x) = 0" "Дихотомия" "1.2-2.0" (dichotomy f2 1.2 2.0 eps)
printRow "f2: x - 2 + sin(1/x) = 0" "Итерации"  "x0=1.5"  (iterationMethod phi2 1.5 eps)
printRow "f2: x - 2 + sin(1/x) = 0" "Ньютон"    "x0=1.5"  (newton f2 df2 1.5 eps)

// Вывод результатов для f3
printRow "f3: e^(x) + ln(x) - 10x = 0" "Дихотомия" "3.0-4.0" (dichotomy f3 3.0 4.0 eps)
printRow "f3: e^(x) + ln(x) - 10x = 0" "Итерации"  "x0=3.5"  (iterationMethod phi3 3.5 eps)
printRow "f3: e^(x) + ln(x) - 10x = 0" "Ньютон"    "x0=3.5"  (newton f3 df3 3.5 eps)

printfn "------------------------------------------------------------------------------------------------"
