module Person   (Person(Per, id_Person, name, sname, firm, phon, day, month, year),

                getId, getDay, getMonth,
                showId, showName, showSname, showFirm, showPhon, showDay, showMonth, showYear,
                showPerson,
                existPersonWithList, editPersonWithList,

                addPerson, remPersonWithList)
             where

import Functions

type Name = String
type Sname = String
type Firm = String
type Phon = String

-- data osoby
--

data Person =
    Per   {
                id_Person   ::      Int,
                name        ::      Name,
                sname       ::      Sname,
                firm        ::      Firm,
                phon        ::      Phon,
                day         ::      Int,
                month       ::      Int,
                year        ::      Int
            } deriving (Show, Read, Eq)


-- podstawowe operacje
getId       ( Per i _ _ _ _ _ _ _) = i
showId      ( Per i _ _ _ _ _ _ _) = show i
setId id    ( Per i n s f p d m y) = ( Per id n s f p d m y)

showName    ( Per _ n _ _ _ _ _ _) = n
showSname   ( Per _ _ s _ _ _ _ _) = s
showFirm    ( Per _ _ _ f _ _ _ _) = f
showPhon    ( Per _ _ _ _ p _ _ _) = p
showDay     ( Per _ _ _ _ _ d _ _) = show d
getDay      ( Per _ _ _ _ _ d _ _) = d
showMonth   ( Per _ _ _ _ _ _ m _) = show m
getMonth    ( Per _ _ _ _ _ _ m _) = m
showYear    ( Per _ _ _ _ _ _ _ y) = show y

showPerson  x = showId x ++ "\t" ++ showName x ++ "\t" ++ showSname x ++ "\t" ++ showFirm x ++ "\t" ++ showPhon x ++ "\t" ++ showDay x ++ "\t" ++ showMonth x ++ "\t" ++ showYear x

existPersonWithList [ ]    my_ID = False
existPersonWithList (x:xs) my_ID = if getId x == my_ID then True
                                   else existPersonWithList xs my_ID

addPerson :: IO Person
addPerson = do
                    putStr ("Podaj imie:\n")
                    my_name <- getLine
                    putStr ("Podaj nazwisko:\n")
                    my_sname <- getLine
                    putStr ("Podaj firme:\n")
                    my_firm <- getLine
                    putStr ("Podaj telefon:\n")
                    my_phon <- getLine
                    putStr ("Podaj date urodzenia:\n")
                    putStr ("dzien:\n")
                    day <- getLine
                    let my_day = stringGoToDay day
                    putStr ("miesiac:\n")
                    month <- getLine
                    let my_month = stringGoToMonth month
                    putStr ("rok:\n")
                    year <- getLine
                    let my_year = stringGoToYear year
                    return (Per { id_Person = 0 , name = my_name , sname = my_sname, firm = my_firm, phon = my_phon, day = my_day, month = my_month, year = my_year})

editPerson :: Person -> IO Person
editPerson x = do
                    putStr ("Podaj imie:\n")
                    my_name <- getLine
                    putStr ("Podaj nazwisko:\n")
                    my_sname <- getLine
                    putStr ("Podaj firme:\n")
                    my_firm <- getLine
                    putStr ("Podaj telefon:\n")
                    my_phon <- getLine
                    putStr ("Podaj date urodzenia:\n")
                    putStr ("dzien:\n")
                    day <- getLine
                    let my_day = stringGoToDay day
                    putStr ("miesiac:\n")
                    month <- getLine
                    let my_month = stringGoToMonth month
                    putStr ("rok:\n")
                    year <- getLine
                    let my_year = stringGoToYear year
                    return (Per { id_Person = getId x  , name = my_name , sname = my_sname, firm = my_firm, phon = my_phon, day = my_day, month = my_month, year = my_year})

remPersonWithList :: [Person] -> Int -> [Person]
remPersonWithList [ ]    my_person_id = []
remPersonWithList (x:xs) my_person_id =  if getId x == my_person_id then xs
                                         else x : remPersonWithList xs my_person_id

editPersonWithList :: [Person] -> Person -> Int -> [Person]
editPersonWithList [ ]    my_person my_person_id = []
editPersonWithList (x:xs) my_person my_person_id =
                                            if getId x == my_person_id
                                                then (( setId my_person_id my_person) : xs)
                                            else x : editPersonWithList xs my_person my_person_id
