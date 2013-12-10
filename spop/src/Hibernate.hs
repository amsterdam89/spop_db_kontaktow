module Hibernate (saveDB,
                loadDB,
                getPeopleId, getGroupId
                --getPeopleList, getGroupList

                )
        where

import Functions
import Person
import Group
import DataBase


--WAZNA FUNKCJA
fileSplit :: String -> Char -> [String]
fileSplit [] delim = [""]
fileSplit (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = fileSplit cs delim

d_EndRec = '&'
d_strEndRec = "&"
d_EndLine ='\n'
d_strEndLine ="\n"
d_Sep   = '%'
d_strSep = "%"


--loadDB :: String -> [String]
--loadDB :: String -> DataBase
loadDB x =  let db = (DataB {
                        pid = ( getPeopleId x ),
                        pdb = ( getPeople ( getPeopleArg ( init (getPeopleList x) ) ) ),
                        gid = ( getGroupId x ),
                        gdb = ( getGroup          ( getGroupArg ( init (getGroupList x  ) ) )  )
                        }
                      )
            in db


-- Podział danych
getRec x = (fileSplit x d_EndRec)

-- [Person]
getPeopleList :: String -> [String]
getPeopleList x = fileSplit ( (getRec x) !! 0) d_EndLine
getPeopleArg :: [String] -> [[String]]
getPeopleArg []     = []
getPeopleArg (x:xs) = (fileSplit x d_Sep) : getPeopleArg xs

-- Id Int
getPeopleId :: String -> Int
getPeopleId x   = stringDigitToInt (reverse (init ( (getRec x) !! 1 ) ) )

-- [Group]
getGroupList :: String -> [String]
getGroupList x  = fileSplit ( (getRec x) !! 2 ) d_EndLine
getGroupArg :: [String] -> [[String]]
getGroupArg []     = []
getGroupArg (x:xs) = (fileSplit x d_Sep) : getGroupArg xs

-- Id Int
getGroupId :: String -> Int
getGroupId x    = stringDigitToInt  (reverse  (init ( (getRec x) !! 3 ) ) )

-- Podział na argumenty
getPeople :: [[String]] -> [Person]
getPeople []     = []
getPeople (x:xs) = (Per { id_Person = (stringDigitToInt (reverse(x !! 0) ) ),
                                     name   = (x !! 1),
                                     sname  = (x !! 2),
                                     firm   = (x !! 3),
                                     phon   = (x !! 4),
                                     day    = (stringDigitToInt (reverse(x !! 5) ) ),
                                     month  = (stringDigitToInt (reverse(x !! 6) ) ),
                                     year   = (stringDigitToInt (reverse(x !! 7) ) )
                                    } ) : getPeople xs


getGroup :: [[String]] -> [Group]
getGroup []     = []
getGroup (x:xs) = (Gro {
                             id_Group = (stringDigitToInt (reverse(x !! 0) ) ),
                             name_Group = (x !! 1),
                             id_People =  getListId (tail(tail x)) } )
                             : (getGroup xs)
-- Lista osob nalezacych do grupy
getListId []     = []
getListId (x:xs) = (stringDigitToInt (reverse x)) : (getListId xs)


-- Blok zapisywania DB do pliku
saveDB :: DataBase -> String
saveDB db = (db_PeopleToFile db) ++ (db_GroupToFile db)

db_PeopleToFile :: DataBase -> String
db_PeopleToFile (DataB id_P [] _ _) = d_strEndRec ++ (show id_P) ++  d_strEndLine ++ d_strEndRec
db_PeopleToFile (DataB id_P (x:xs) id_G y)= showId x ++ d_strSep ++ showName x ++ d_strSep ++ showSname x ++ d_strSep
                                           ++ showFirm x ++ d_strSep ++ showPhon  x ++ d_strSep ++ showDay x ++ d_strSep
                                           ++ showMonth x ++ d_strSep ++ showYear x ++ d_strEndLine
                                           ++ db_PeopleToFile (DataB id_P xs id_G y)

db_GroupToFile :: DataBase -> String
db_GroupToFile (DataB _ _ id_G []) = d_strEndRec ++ (show id_G) ++ d_strEndLine ++ d_strEndRec
db_GroupToFile (DataB id_P x id_G (y:ys) ) =  showIdG y ++ d_strSep ++ showNameG y ++ d_strSep
                                            ++ db_ListPeopleToFile y ++ d_strEndLine
                                            ++ db_GroupToFile (DataB id_P x id_G ys )

-- Lista osob w grupie
db_ListPeopleToFile :: Group -> String
db_ListPeopleToFile ( Gro _ _ [])      = ""
db_ListPeopleToFile ( Gro _ _ (x:[]) ) = (show x)
db_ListPeopleToFile ( Gro id n (x:xs) ) = (show x) ++ d_strSep ++ db_ListPeopleToFile ( Gro id n xs )

