module Functions ( stringIsNumber, stringDigitToInt,
                    getDate,
                    stringGoToDay, stringGoToMonth, stringGoToYear
                 )
        where


import Data.Time.Clock
import Data.Time.Calendar
import Data.Char

-- Blok funkcji uzytych w Main

-- Funkcja zwracajaca dzisiejsza date, stosowana do urodzin
getDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getDate = getCurrentTime >>= return . toGregorian . utctDay


-- sprawdza czy string jest liczba
stringIsNumber :: [Char] -> Bool
stringIsNumber []           = False
stringIsNumber (x:[])     = if isNumber x then True
                            else False
stringIsNumber (x:xs)     = if isNumber x then stringIsNumber xs
                            else False

-- zwraca stringa jako Int, podawac tablice w postaci odwrotnej (reverse)
stringDigitToInt []     = 0
stringDigitToInt (x:xs) = (digitToInt x)  + ( ( stringDigitToInt (xs) ) * 10)


-- przyblizone funkcje daty urodzin
stringGoToDay [] = 0
stringGoToDay (x:xs) = if stringIsNumber (x:xs) then stringDayToInt (stringDigitToInt (reverse (x:xs) ) )
                       else 0

stringDayToInt x   = if x > 0 && x <= 31 then x
                        else 0

stringGoToMonth [] = 0
stringGoToMonth (x:xs) = if stringIsNumber (x:xs) then stringMonthToInt (stringDigitToInt (reverse (x:xs) ) )
                       else 0

stringMonthToInt x   = if x > 0 && x <= 12 then x
                        else 0

stringGoToYear [] = 0
stringGoToYear (x:xs) = if stringIsNumber (x:xs) then (stringDigitToInt (reverse (x:xs) ) )
                       else 0
