module Main where

import System.IO
import System.Exit

import Functions

import DataBase
import Person
import Group

import Hibernate


main = do
            programPrint
            menuPrint
            let db = newDB
            showMainMenu db


programPrint = putStr ("Witaj w bazie kontaktów\n\n")

newDB:: DataBase
newDB = createDB
createDB :: DataBase
createDB = DataB 1 [] 1 []


my_FILE = "my_db.txt"

--showMainMenu :: MyDataB -> MyDataB
showMainMenu db = do
                    --menuPrint
                    arg <- menuReadChoice
                    menuAction arg db


-- GLOWNE MENU PROGRAMU
menuPrint = putStr
                    (
                    "DB zapisywana w pliku " ++ my_FILE ++ "\n" ++
                    "Polecenia:" ++ "\n" ++
                    "help - wyswietl polecenia"  ++ "\n" ++
                    "fill - wypelnij db przykladowymi danymi"  ++ "\n" ++
                    "clear - wyczysc db"  ++ "\n" ++
                    "load - wczytaj db z pliku"  ++ "\n" ++
                    "save - zapisz db do pliku"  ++ "\n" ++
                    " Polecenia dla osob" ++ "\n" ++
                    "\ta - dodaj rekord osoby" ++ "\n" ++
                    "\te - modyfikuj rekord osoby" ++ "\n" ++
                    "\tr - usun rekord osoby" ++ "\n" ++
                    "\ts - pokaz wszystkie osoby w db" ++ "\n" ++
                    "\t\ts-sn - pokaz osobe o zadanym nazwisku" ++ "\n" ++
                    "\t\ts-p  - pokaz osobe o zadanym nr tel" ++ "\n" ++
                    "\t\ts-b  - pokaz osoby obchodzace dzisiaj urodziny" ++ "\n" ++
                    " Polecenia dla grup kontaktow" ++ "\n" ++
                    "\tg-c - tworz grupe kontaktow" ++ "\n" ++
                    "\tg-m - scal grupy kontaktow" ++ "\n" ++
                    "\tg-s - wyswietl grupy kontaktow w db" ++ "\n" ++
                    "\t\tg-sp - wyswietl osoby w grupie kontaktow" ++ "\n" ++
                    "\tg-r - usun grupe kontaktow w db" ++ "\n" ++
                    "\tg-ap - dodaj do grupy kontaktow osobe" ++ "\n" ++
                    "\tg-rp - usun osobe z grupiy kontaktow" ++ "\n" ++
                    "q - wyjscie z programu" ++ "\n"
                    )
                    -- >> hFlush stdout

menuReadChoice = getLine
--hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getChar


-- PodMenu programu dluuugasne
-- mialo byc podzielone na pod menu
-- lecz potrzebowalem miec dostep do podMenu :(

menuAction "help" db = do

                    menuPrint
                    let db' = db

                    --glowne menu
                    showMainMenu db'

menuAction "fill" db = do
                    putStr ("Wczytuje gotowa grupe osob do DB" ++ "\n")
                    let db' = DataB 4 (
                                (Per { id_Person = 1 , name = "Stefan" , sname = "Bajer", firm = "Poso",
                                 phon = "536-432-545", day = 2, month = 3, year = 1980} ):
                                (Per { id_Person = 2 , name = "Olek" , sname = "Nowy", firm = "Hom",
                                 phon = "777-777-777", day = 22, month = 11, year = 1988} ):
                                (Per { id_Person = 3 , name = "Piotr" , sname = "Rana", firm = "Park",
                                 phon = "555-444-543", day = 13, month = 1, year = 1976} ):
                                []
                                )
                                3
                                (
                                (Gro { id_Group = 1, name_Group = "Praca", id_People = [1,2]   }) :
                                (Gro { id_Group = 2, name_Group = "Rodzina", id_People = [3,2] }) :
                                [] )

                    putStr ("Osoby w DB" ++ "\n")
                    putStr ("id \t imie \t nazwisko \t firma \t telefon \t data urodzenia" ++ "\n")
                    let my_db = db_ShowPeople db'
                    putStr (my_db )

                    --glowne menu
                    showMainMenu db'

menuAction "clear" db = do
                    putStr ("Czyszcze DB" ++ "\n")

                    let db' = newDB
                    --glowne menu
                    showMainMenu db'


menuAction "save" db = do
                    putStr ("zapisuje DB do pliku" ++ "\n")


                    outh <- openFile my_FILE WriteMode
                    hPutStrLn outh (saveDB db)
                    --hPutStrLn outh (foldr (\a b -> a (';' : b)) "\n" (map shows (db_ShowPeople db))
                    hClose outh

                    let db' = db
                    --glowne menu
                    showMainMenu db'


menuAction "load" db = do
                    putStr ("wczytuje DB z pliku" ++ "\n")

                    handle <- openFile my_FILE ReadMode

                    contents <- hGetContents handle
                    putStr ( "new Id Person = " ++ show (getPeopleId contents) ++ "\t")
                    --putStr ( show init (getPeopleList contents) ++ "\n")
                    putStr ( "new Id Group = " ++ show (getGroupId contents) ++ "\n")
                    --putStr ( show init (getGroupList contents) ++ "\n")
                    let db =  loadDB contents

                    hClose handle

                    putStr ( "Wczytano i utworzono pomyslnie struktury db" ++ "\n")
                    let db' = db

                    --glowne menu
                    showMainMenu db'

menuAction "g-c" db = do
                    putStr ("Stworz grupe kontaktow do DB" ++ "\n")
                    my_group <- addGroup
                    --let my_group = (Gro { id_Group = 0 , name_Group = "testowa" ,id_People = [] })
                    let db' = db_AddGroup db my_group
                    putStr ("Grupa kontaktow stworzona pomyslnie" ++ "\n")

                    --glowne menu
                    showMainMenu db'

menuAction "g-s" db = do
                    putStr ("Grupy w DB" ++ "\n")
                    putStr ("id \t nazwa \t" ++ "\n")
                    let my_db = db_ShowGroups db
                    putStr (my_db )
                    let db' = db

                    --glowne menu
                    showMainMenu db'

menuAction "g-r" db = do
                    putStr ("Podaj ID usuwanej grupy (wartosc liczbowa)" ++ "\n")
                    my_group_id <- getLine

                    if stringIsNumber my_group_id then
                        do
                            let group_id = stringDigitToInt (reverse my_group_id)

                            if db_ExistGroup db group_id  then
                                do

                                    let db'  = db_RemGroup db group_id
                                    putStr ("Grupa kontaktow usunieta pomyslnie" ++ "\n")

                                    --glowne menu
                                    showMainMenu db'

                                else do
                                    putStr ("Grupa o podanym ID nieistnieje" ++ "\n")
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'
                        else do
                            putStr ("Nieprawidłowa wartość ID" ++ "\n")
                            let db' = db

                            --glowne menu
                            showMainMenu db'
--end

menuAction "g-m" db = do
                    putStr ("Podaj ID pierwszej grupy (wartosc liczbowa)" ++ "\n")
                    my_group_id_1 <- getLine
                    putStr ("Podaj ID drugiej grupy (wartosc liczbowa)" ++ "\n")
                    my_group_id_2 <- getLine

                    if (stringIsNumber my_group_id_1) && (stringIsNumber my_group_id_2) && ( my_group_id_1 /=  my_group_id_2) then
                        do
                            let group_id_1 = stringDigitToInt (reverse my_group_id_1)
                            let group_id_2 = stringDigitToInt (reverse my_group_id_2)

                            if (db_ExistGroup db group_id_1) && (db_ExistGroup db group_id_2)  then
                                do

                                    let db_m = db_MerGroup db group_id_1 group_id_2
                                    -- merguje grupy tworzac nowa, nie usuwam 1 i 2
                                    let db_r = db_RemGroup db_m group_id_1
                                    -- usuwam 1 grupe
                                    let db'  = db_RemGroup db_r group_id_2
                                    -- usuwam 2 grupe
                                    putStr ("Grupa kontaktow scalona pomyslnie" ++ "\n")

                                    --glowne menu
                                    showMainMenu db'

                                else do
                                    putStr ("Grupa o podanym ID nieistnieje" ++ "\n")
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'
                        else do
                            putStr ("Nieprawidłowa wartość jednego z ID" ++ "\n")
                            let db' = db

                            --glowne menu
                            showMainMenu db'

menuAction "g-sp" db = do
                    putStr ("Podaj ID grupy (wartosc liczbowa)" ++ "\n")
                    my_group_id <- getLine

                    if stringIsNumber my_group_id then
                        do
                            let group_id = stringDigitToInt (reverse my_group_id)

                            if db_ExistGroup db group_id then
                                do
                                    putStr ("Osoby w Grupie kontaktow" ++ "\n")
                                    putStr ("id \t imie \t nazwisko \t firma \t telefon \t data urodzenia" ++ "\n")
                                    let my_db = db_ShowPeopleInGroup db group_id
                                    putStr (my_db )
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'
                                else do
                                    putStr ("Grupa o podanym ID nieistnieje" ++ "\n")
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'
                        else do
                            putStr ("Nieprawidłowa wartość ID" ++ "\n")
                            let db' = db

                            --glowne menu
                            showMainMenu db'




menuAction "g-rp" db = do
                    putStr ("Usuniecie osoby z grupy kontaktow" ++ "\n")
                    putStr ("Podaj ID osoby (wartosc liczbowa)" ++ "\n")
                    my_person_id <- getLine
                    putStr ("Podaj ID grupy (wartosc liczbowa)" ++ "\n")
                    my_group_id <- getLine

                    if (stringIsNumber my_person_id) && (stringIsNumber my_group_id) then
                        do
                            let group_id = stringDigitToInt (reverse my_group_id)

                            if db_ExistGroup db group_id then

                                do
                                    let person_id = stringDigitToInt (reverse my_person_id)

                                    if db_ExistPersonInGroup db group_id person_id then
                                        do
                                            let db' = (db_RemPersonToGroup db group_id person_id)
                                            putStr ("Usunieto osobe z grupy kontakow" ++ "\n")

                                            --glowne menu
                                            showMainMenu db'

                                        else do
                                            putStr ("Osoba o podanym ID nie istnieje" ++ "\n")
                                            let db' = db

                                            --glowne menu
                                            showMainMenu db'
                                --end
                                else do
                                    putStr ("Grupa o podanym ID nie istnieje" ++ "\n")
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'

                        else do
                            putStr ("Nieprawidłowa wartość jednego ID" ++ "\n")
                            let db' = db

                            --glowne menu
                            showMainMenu db'
--end

menuAction "g-ap" db = do
                    putStr ("Dodaj osobe do grupy kontaktow" ++ "\n")
                    putStr ("Podaj ID osoby (wartosc liczbowa)" ++ "\n")
                    my_person_id <- getLine
                    putStr ("Podaj ID grupy (wartosc liczbowa)" ++ "\n")
                    my_group_id <- getLine

                    if (stringIsNumber my_person_id) && (stringIsNumber my_group_id) then
                        do
                            let person_id = stringDigitToInt (reverse my_person_id)

                            if db_ExistPerson db person_id then
                                do
                                    let group_id = stringDigitToInt (reverse my_group_id)

                                    if db_ExistGroup db group_id then
                                        do
                                            let db' = (db_AddPersonToGroup db group_id person_id)
                                            putStr ("Dodano osobe do grupy kontakow" ++ "\n")

                                            --glowne menu
                                            showMainMenu db'

                                        else do
                                            putStr ("Grupa o podanym ID nie istnieje" ++ "\n")
                                            let db' = db

                                            --glowne menu
                                            showMainMenu db'
                                --end
                                else do
                                    putStr ("Osoba o podanym ID nie istnieje" ++ "\n")
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'

                        else do
                            putStr ("Nieprawidłowa wartość jednego ID" ++ "\n")
                            let db' = db

                            --glowne menu
                            showMainMenu db'

menuAction "a" db = do
                    putStr ("Dodaj osobe do DB" ++ "\n")
                    my_person <- addPerson
                    --let my_person = (Per { id_Person = 3 , name = "Bolo" , sname = "Molo", firm = "RAso", phon = "467-657-756", day = 21, month = 2, year = 1971})
                    let db' = db_AddPerson db my_person
                    putStr ("Osoba dodana pomyslnie do DB" ++ "\n")

                    --glowne menu
                    showMainMenu db'

menuAction "r" db = do
                    putStr ("Usun osobe z DB" ++ "\n")
                    putStr ("Podaj ID osoby (wartosc liczbowa)" ++ "\n")
                    my_person_id <- getLine

                    if stringIsNumber my_person_id then
                        do
                            let person_id = stringDigitToInt (reverse my_person_id)

                            if db_ExistPerson db person_id then
                                do
                                    let db_r = db_RemPerson db person_id
                                    -- usuwam osobe
                                    let db' = (db_RemPersonFromAllGroups db_r person_id)
                                    -- usuwam osobe z grup, jezeli jest w nich zapisana
                                    putStr ("Osoba usunieta pomyslnie z DB" ++ "\n")

                                    --glowne menu
                                    showMainMenu db'

                                else do
                                    putStr ("Nie ma osoby w db o takim ID" ++ "\n")
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'

                        else do
                            putStr ("Nieprawidłowa wartość ID" ++ "\n")
                            let db' = db

                            --glowne menu
                            showMainMenu db'


menuAction "s-sn" db = do
                    putStr ("Podaj nazwisko poszukiwanej osoby" ++ "\n")
                    my_sname <- getLine
                    putStr ("Osoby w DB" ++ "\n")
                    putStr ("id \t imie \t nazwisko \t firma \t telefon \t data urodzenia" ++ "\n")
                    let my_db = db_ShowPeopleSname db my_sname
                    putStr (my_db )
                    let db' = db

                    --glowne menu
                    showMainMenu db'

menuAction "s-p" db = do
                    putStr ("Podaj telefon poszukiwanej osoby" ++ "\n")
                    my_phon <- getLine
                    putStr ("Osoby w DB" ++ "\n")
                    putStr ("id \t imie \t nazwisko \t firma \t telefon \t data urodzenia" ++ "\n")
                    let my_db = db_ShowPeoplePhon db my_phon
                    putStr (my_db )
                    let db' = db

                    --glowne menu
                    showMainMenu db'

menuAction "s-b" db = do
                    putStr ("Osoby obchodzace dzisiaj urodziny\n")
                    (year, month, day) <- getDate
                    putStr ("Dzisiaj jest: " ++ (show  day) ++ "." ++ (show month) ++ "." ++ (show year) ++ "\n")
                    putStr ("id \t imie \t nazwisko \t firma \t telefon \t data urodzenia" ++ "\n")
                    let my_db = db_ShowPeopleBirthday db day month
                    putStr (my_db )
                    let db' = db

                    --glowne menu
                    showMainMenu db'

menuAction "s" db = do
                    putStr ("Osoby w DB" ++ "\n")
                    putStr ("id \t imie \t nazwisko \t firma \t telefon \t data urodzenia" ++ "\n")
                    let my_db = db_ShowPeople db
                    putStr (my_db )
                    let db' = db

                    --glowne menu
                    showMainMenu db'


menuAction "e" db = do
                    putStr ("Modyfikuj osobe w DB" ++ "\n")
                    putStr ("Podaj ID osoby (wartosc liczbowa)" ++ "\n")
                    my_person_id <- getLine

                    if stringIsNumber my_person_id then
                        do
                            let person_id = stringDigitToInt (reverse my_person_id)

                            if db_ExistPerson db person_id then
                                do
                                    putStr ("Stare dane osoby" ++ "\n")
                                    let my_old_person = db_ShowPerson db person_id
                                    putStr (my_old_person)
                                    my_person <- addPerson
                                    let db' = db_EditPerson db my_person person_id
                                    putStr ("Osoba zmodyfikowana pomyslnie do DB" ++ "\n")

                                    --glowne menu
                                    showMainMenu db'


                               else do
                                    putStr ("Nie ma osoby w db o takim ID" ++ "\n")
                                    let db' = db

                                    --glowne menu
                                    showMainMenu db'

                        else do
                            putStr ("Nieprawidłowa wartość ID" ++ "\n")
                            let db' = db

                            --glowne menu
                            showMainMenu db'

menuAction "q" db = exitSuccess

menuAction _   db = do
                    putStr ("niepoprawny argument.\n")
                    let db' = db

                    --glowne menu
                    showMainMenu db'
