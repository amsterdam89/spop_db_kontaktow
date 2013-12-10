module Group (Group(Gro, id_Group, name_Group, id_People),
                getIdG,

                addGroup, remGroup,
                showIdG, showNameG, showPeop,
                addPersonToListGroup,
                remPersonToListGroup,
                remPersonFromAllGroups,
                merGroup,
                getPeopOnId ,
                existGroupWithList, existPersonInGroup
            )
            where

import Person


-- data mieszczaca grupy,
-- zdecydowalem sie na przetrzymywanie tylko id osob
data Group =
    Gro     {
                id_Group      ::  Int,
                name_Group    :: String,
                id_People     :: [Int]
            } deriving (Show, Read, Eq)

-- pomocnicza funkcja do wycinania duplikatow przy merge z grup
rmDup :: [Int] -> [Int]
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (\y -> not(x == y)) xs)

-- podstawowe funkcje
getIdG       ( Gro i _ _) = i
showIdG      ( Gro i _ _) = show i
setIdG id    ( Gro i n id_p) = ( Gro id n id_p)

showNameG    ( Gro _ n _) = n

showPeop :: Group -> [Person] -> String
showPeop    ( Gro _ _ [] )    y = "---\n"
showPeop    ( Gro i n (x:xs)) y = ( showPeopOnId y x ) ++ showPeop ( Gro i n xs) y

getPeopOnId :: Group -> [Int]
getPeopOnId    ( Gro _ _ x) = x

showPeopOnId :: [Person] -> Int -> String
showPeopOnId []     id = ""
showPeopOnId (y:ys) id = if id == Person.getId y then (showPerson y ) ++ "\n"
                        else showPeopOnId ys id

-- reszta funkcji
addPersonToListGroup :: [Group] -> Int -> Int -> [Group]
addPersonToListGroup [] my_group_id my_id     =  []
addPersonToListGroup (y:ys) my_group_id my_id = if getIdG y == my_group_id then ( (addPersonToGroup y my_id) : ys )
                                                else ( y : ( addPersonToListGroup ys my_group_id my_id ) )

remPersonToListGroup :: [Group] -> Int -> Int -> [Group]
remPersonToListGroup [] group_id person_id      = []
remPersonToListGroup (y:ys) group_id person_id  = if getIdG y == group_id then ( ( remPersonToGroup y person_id) : ys )
                                                  else ( y : ( remPersonToListGroup ys group_id person_id ) )

remPersonFromAllGroups :: [Group] -> Int -> [Group]
remPersonFromAllGroups [ ] person_id     = []
remPersonFromAllGroups (y:ys) person_id  = (remPersonToGroup y person_id) : ( remPersonFromAllGroups ys person_id )

addPersonToGroup :: Group -> Int -> Group
addPersonToGroup  ( Gro i n x) y = ( Gro i n ( ( y ) : x))

remPersonToGroup :: Group -> Int -> Group
remPersonToGroup ( Gro i n x ) person_id = ( Gro i n (remIdPersonWithList x person_id ))

remIdPersonWithList :: [Int] -> Int -> [Int]
remIdPersonWithList []     person_id = []
remIdPersonWithList (x:xs) person_id = if x == person_id then xs
                                       else x : (remIdPersonWithList xs person_id)


merGroup :: [Group] -> Int -> Int -> Int -> Group
--merGroup (Gro i n x) group_id_1 group_id_2       = ( Gro i n x )
merGroup (y:ys) group_id_1 group_id_2 my_new_ID    = ( Gro { id_Group = my_new_ID,
                                                        name_Group = ( showNameG (findGroup (y:ys) group_id_1) ++ " " ++ showNameG (findGroup (y:ys) group_id_2) ) ,
                                                        id_People = ( rmDup ( (getPeopOnId (findGroup (y:ys) group_id_1) ) ++
                                                                 (getPeopOnId (findGroup (y:ys) group_id_2) ) ) ) } )

findGroup :: [Group] -> Int -> Group
--findGroup ( Gro i n x ) group_id                = ( Gro i n x )
findGroup (y:ys) group_id                       = if getIdG y == group_id then  y
                                                  else (findGroup ys group_id)

existGroupWithList [ ]    my_ID = False
existGroupWithList (x:xs) my_ID = if getIdG x == my_ID then True
                                   else existGroupWithList xs my_ID

existPersonInGroup ( Gro i n [] )    person_id = False
existPersonInGroup ( Gro i n (x:xs)) person_id = if x == person_id then True
                                                 else existPersonInGroup ( Gro i n xs) person_id


addGroup :: IO Group
addGroup = do
                    putStr ("Podaj nazwe grupy:\n")
                    my_name_group <- getLine
                    return (Gro  { id_Group = 0 , name_Group = my_name_group ,id_People = [] })


remGroup :: [Group] -> Int -> [Group]
remGroup []     my_group_id = []
remGroup (y:ys) my_group_id = if getIdG y == my_group_id then ys
                              else (y: (remGroup ys my_group_id))
