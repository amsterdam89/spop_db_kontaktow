module DataBase  (DataBase(DataB, pid, pdb, gid, gdb ) ,

                        db_ExistGroup, db_ExistPersonInGroup,
                        db_ShowGroups, db_ShowPeopleInGroup,
                        db_AddGroup, db_AddPersonToGroup,
                        db_RemPersonFromAllGroups,
                        db_RemPersonToGroup , db_MerGroup,
                        db_RemGroup,

                        db_ShowPeopleSname, db_ShowPeoplePhon, db_ShowPeopleBirthday,
                        db_ExistPerson, db_ShowPerson, db_EditPerson,
                        db_AddPerson, db_RemPerson, db_ShowPeople)
                where



import Group
import Person

-- data opisujaca db
-- prosta struktura skladajaca sie z
-- pid jeszcze nieuzyte id dla wstawianej osoby
-- pdb lista osob w db
-- gid jeszcze nieuzyte id dla wstawianej grupy
-- gdb lista grup w db

data DataBase =
    DataB {
                pid :: Int ,
                pdb :: [Person] ,
                gid :: Int ,
                gdb :: [Group]
            }

-- funkcje Person and Group

db_ExistPersonInGroup :: DataBase -> Int -> Int -> Bool
db_ExistPersonInGroup  (DataB id_P x id_G [] ) group_id person_id       = False
db_ExistPersonInGroup  (DataB id_P x id_G (y:ys) ) group_id person_id   = if getIdG y == group_id then existPersonInGroup y person_id
                                                                          else db_ExistPersonInGroup  (DataB id_P x id_G ys ) group_id person_id
-- funkcje Group
db_ExistGroup :: DataBase -> Int -> Bool
db_ExistGroup (DataB id_P x id_G [] ) my_ID     = False
db_ExistGroup (DataB id_P x id_G (y:ys) ) my_ID = existGroupWithList (y:ys) my_ID

db_AddGroup :: DataBase -> Group -> DataBase
db_AddGroup (DataB id_P x id_G y ) (Gro id_Group name_Group id_People) = (DataB id_P x (id_G + 1) ( (Gro id_G name_Group id_People): y ) )

db_RemGroup :: DataBase -> Int -> DataBase
db_RemGroup (DataB id_P x id_G [] )     my_group_id = (DataB id_P x id_G [] )
db_RemGroup (DataB id_P x id_G (y:ys) ) my_group_id = (DataB id_P x id_G (remGroup (y:ys) my_group_id) )

db_AddPersonToGroup :: DataBase -> Int -> Int -> DataBase
db_AddPersonToGroup (DataB id_P x id_G (y:ys) ) my_group_id my_id = (DataB id_P x id_G  ( addPersonToListGroup (y:ys) my_group_id my_id  ) )

db_RemPersonToGroup :: DataBase -> Int -> Int -> DataBase
db_RemPersonToGroup (DataB id_P x id_G y ) group_id person_id = (DataB id_P x id_G ( remPersonToListGroup y group_id person_id ) )

db_RemPersonFromAllGroups :: DataBase -> Int -> DataBase
db_RemPersonFromAllGroups (DataB id_P x id_G y ) person_id = (DataB id_P x id_G ( remPersonFromAllGroups y person_id ) )

-- funkcje show grupa
db_ShowGroups :: DataBase -> String
db_ShowGroups (DataB _ _ _ []) = "---\n"
db_ShowGroups (DataB id_P x id_G (y:ys) )=  showIdG y ++ "\t" ++ showNameG y ++ "\n" ++ db_ShowGroups (DataB id_P x id_G ys)

db_ShowPeopleInGroup :: DataBase -> Int -> String
db_ShowPeopleInGroup (DataB id_P x id_G [] ) group_id = "---\n"
db_ShowPeopleInGroup (DataB id_P x id_G (y:ys) ) group_id = if (getIdG y) == group_id then showPeop y x
                                                            else db_ShowPeopleInGroup (DataB id_P x id_G ys ) group_id


-- merge
db_MerGroup :: DataBase -> Int -> Int  -> DataBase
db_MerGroup (DataB id_P x id_G [] ) group_id_1 group_id_2 = (DataB id_P x id_G [] )
db_MerGroup (DataB id_P x id_G y )  group_id_1 group_id_2 = (DataB id_P x (id_G +1) ( ( merGroup y group_id_1 group_id_2 id_G ) : y ) )

-- funkcje Person
db_ExistPerson :: DataBase -> Int -> Bool
db_ExistPerson (DataB id_P [] id_G y) my_ID     = False
db_ExistPerson (DataB id_P (x:xs) id_G y) my_ID = existPersonWithList (x:xs) my_ID

db_ShowPerson :: DataBase -> Int -> String
db_ShowPerson (DataB _ [] _ _) _ = "---\n"
db_ShowPerson (DataB id_P (x:xs) id_G y) my_ID = if getId x == my_ID then showId x ++ "\t" ++ showName x ++"\t" ++ showSname x ++ "\t" ++ showFirm x ++ "\t" ++ showPhon  x ++ "\t" ++ showDay x ++ "." ++ showMonth x ++ "." ++ showYear x ++ "\n"
                                                 else db_ShowPerson (DataB id_P (xs) id_G y) my_ID

db_AddPerson :: DataBase -> Person -> DataBase
db_AddPerson (DataB id_P x id_G y) (Per id_Person name sname firm phon day month year) = (DataB (id_P + 1) (  (Per id_P name sname firm phon day month year):(x) ) id_G y)

db_RemPerson :: DataBase -> Int -> DataBase
db_RemPerson (DataB id_P (x:xs) id_G y) my_person_id = (DataB id_P ( remPersonWithList (x:xs) my_person_id ) id_G y)


db_EditPerson :: DataBase -> Person -> Int -> DataBase
db_EditPerson (DataB id_P (x:xs) id_G y) my_person my_ID= (DataB id_P ( editPersonWithList (x:xs) my_person my_ID ) id_G y)

-- funkcje person show

db_ShowPeople :: DataBase -> String
db_ShowPeople (DataB _ [] _ _) = "---\n"
db_ShowPeople (DataB id_P (x:xs) id_G y)= showId x ++ "\t" ++ showName x ++ "\t" ++ showSname x ++ "\t" ++ showFirm x ++ "\t" ++ showPhon  x ++ "\t" ++ showDay x ++ "." ++ showMonth x ++ "." ++ showYear x ++ "\n" ++ db_ShowPeople (DataB id_P xs id_G y)

db_ShowPeopleSname :: DataBase -> String -> String
db_ShowPeopleSname (DataB _ [] _ _) _ = "---\n"
db_ShowPeopleSname (DataB id_P (x:xs) id_G y) my_sname = if my_sname == showSname x then showId x ++ "\t" ++ showName x ++"\t" ++ showSname x ++ "\t" ++ showFirm x ++ "\t" ++ showPhon  x ++ "\t" ++ showDay x ++ "." ++ showMonth x ++ "." ++ showYear x ++ "\n" ++ db_ShowPeopleSname (DataB id_P xs id_G y) my_sname
                                                         else db_ShowPeopleSname (DataB id_P xs id_G y) my_sname

db_ShowPeoplePhon :: DataBase -> String -> String
db_ShowPeoplePhon (DataB _ [] _ _) _ = "---\n"
db_ShowPeoplePhon (DataB id_P (x:xs) id_G y) my_phon = if my_phon == showPhon x then showId x ++ "\t" ++ showName x ++"\t" ++ showSname x ++ "\t" ++ showFirm x ++ "\t" ++ showPhon  x ++ "\t" ++ showDay x ++ "." ++ showMonth x ++ "." ++ showYear x ++ "\n" ++ db_ShowPeoplePhon (DataB id_P xs id_G y) my_phon
                                                         else db_ShowPeoplePhon (DataB id_P xs id_G y) my_phon


db_ShowPeopleBirthday:: DataBase -> Int -> Int -> String
db_ShowPeopleBirthday (DataB _ [] _ _) _ _ = "---\n"
db_ShowPeopleBirthday (DataB id_P (x:xs) id_G y) day month = if (day == getDay x) && (month == getMonth x)   then showId x ++ "\t" ++ showName x ++"\t" ++ showSname x ++ "\t" ++ showFirm x ++ "\t" ++ showPhon  x ++ "\t" ++ showDay x ++ "." ++ showMonth x ++ "." ++ showYear x ++ "\n" ++ db_ShowPeopleBirthday (DataB id_P xs id_G y) day month
                                                         else db_ShowPeopleBirthday (DataB id_P xs id_G y) day month

