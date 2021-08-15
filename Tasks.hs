{-
    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Text.Printf
import Data.List
import Data.Char
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}

-- Task 1

conv :: Value->Float
conv x = if (x == "") then 0 else (read x :: Float)

change :: Row->Row
change (x0:(x1:(x2:(x3:(x4:(x5:(x6:(x7:x)))))))) = x0:((printf "%.2f"(((conv x1) + (conv x2) + (conv x3) + (conv x4) + (conv x5) + (conv x6))/4 + (conv x7))):[]) 

compute_exam_grades :: Table -> Table
compute_exam_grades (x:xs) = ["Nume", "Punctaj Exam"]:(map change xs)

-- Task 2
-- Number of students who have passed the exam:

op :: Row -> Int -> Int
op line acc = if ((conv (line !! 1)) >= 2.5) then 1 + acc else acc

get_passed_students_num :: Table -> Int
get_passed_students_num x = foldr op 0 (tail(compute_exam_grades x)) 
                            
-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage x = (fromIntegral (get_passed_students_num x) :: Float) / (fromIntegral ((length x) - 1) :: Float)

-- Average exam grade
op_aux :: Row -> Float -> Float
op_aux line acc = (conv (line !! 1)) + acc

aux :: Table -> Float
aux x = foldr op_aux 0 (tail(compute_exam_grades x))

get_exam_avg :: Table -> Float
get_exam_avg x = (aux x) / (fromIntegral ((length x) - 1) :: Float)

-- Number of students who gained at least 1.5p from homework:
op_aux2 :: Row -> Int -> Int
op_aux2 line acc = if ((conv (line !! 2)) + (conv (line !! 3)) + (conv (line !! 4)) >= 1.5) then 1 + acc else acc

get_passed_hw_num :: Table -> Int
get_passed_hw_num x = foldr op_aux2 0 (tail x)

-- Task 3 -get_avg_responses_per_qs x = [["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"]]
op_Q1 :: Row -> Float -> Float
op_Q1 line acc = (conv (line !! 1)) + acc

op_Q2 :: Row -> Float -> Float
op_Q2 line acc = (conv (line !! 2)) + acc

op_Q3 :: Row -> Float -> Float
op_Q3 line acc = (conv (line !! 3)) + acc

op_Q4 :: Row -> Float -> Float
op_Q4 line acc = (conv (line !! 4)) + acc

op_Q5 :: Row -> Float -> Float
op_Q5 line acc = (conv (line !! 5)) + acc

op_Q6 :: Row -> Float -> Float
op_Q6 line acc = (conv (line !! 6)) + acc

g :: Table -> (Row -> Float -> Float) -> String
g x op = printf "%.2f" (foldr op 0 (tail x) / fromIntegral ((length x) - 1) :: Float)

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs x = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"]:(((g x op_Q1):[])++((g x op_Q2):[])++((g x op_Q3):[])++((g x op_Q4):[])++((g x op_Q5):[])++((g x op_Q6):[])):[]

-- Task 4

conv_second :: Value->Float
conv_second x = if (x == "") then 0 else (read x :: Float)

find_Q :: Table -> Int -> Float -> Int
find_Q (xs:x) q m = if (conv_second (xs !! q) == m) then 1 + find_Q x q m else find_Q x q m
find_Q [] q m = 0

get_exam_aux :: Table->Int->[Value]
get_exam_aux x y = ((("Q"++show y):[])++((show (find_Q (tail x) y 0)):[])++((show (find_Q (tail x) y 1)):[])++((show (find_Q (tail x) y 2)):[]))

get_exam_summary :: Table->Table
get_exam_summary x = [["Q","0","1","2"]]++((get_exam_aux x 1):[])++((get_exam_aux x 2):[])++((get_exam_aux x 3):[])++((get_exam_aux x 4):[])++((get_exam_aux x 5):[])++((get_exam_aux x 6):[])

-- Task 5

comparable_task5 :: Row -> Row -> Bool
comparable_task5 x y = if (conv(x !! 1) < conv(y !! 1)) then True else (if (conv(x !! 1) == conv(y !! 1)) then (if ((x !! 0) < (y !! 0)) then True else False) else False)


insertionSort_prim :: (Row -> Row -> Bool)->Table -> Table
insertionSort_prim comparable [x] = [x]
insertionSort_prim comparable (x:xs) = insert_prim comparable x (insertionSort_prim comparable xs)

insert_prim :: (Row -> Row -> Bool)->Row -> Table -> Table
insert_prin comparable x [] = [x]
insert_prim comparable x (y:[]) = if (comparable x y) then x:y:[] else y:x:[]
insert_prim comparable x (y:ys) = if (comparable x y) then x:y:ys else y : insert_prim comparable x ys

get_ranking :: Table -> Table
get_ranking x = ["Nume","Punctaj Exam"]:(insertionSort_prim comparable_task5 (tail (compute_exam_grades x)))

-- Task 6

formula :: Value->Value->Value->Value->Value->Value->Float
formula x1 x2 x3 x4 x5 x6 = ((conv x1) + (conv x2) + (conv x3) + (conv x4) + (conv x5) + (conv x6))/4

modul :: Value->Value->Value->Value->Value->Value->Value->Float
modul x1 x2 x3 x4 x5 x6 x7 = if ((formula x1 x2 x3 x4 x5 x6) > (conv x7)) then ((formula x1 x2 x3 x4 x5 x6) - (conv x7)) else ((conv x7) - (formula x1 x2 x3 x4 x5 x6))    

change1 :: Row->Row
change1 (x0:(x1:(x2:(x3:(x4:(x5:(x6:(x7:x)))))))) = x0:(printf "%.2f" (formula x1 x2 x3 x4 x5 x6):[])++(printf "%.2f" (conv x7):[])++((printf "%.2f"(modul x1 x2 x3 x4 x5 x6 x7)):[])

get_exam_diff_table_aux :: Table -> Table
get_exam_diff_table_aux (x:xs) = map change1 xs


---["Nume","Punctaj interviu","Punctaj scris","Diferenta"]:

comparable_task6 :: Row -> Row -> Bool
comparable_task6 x y = if (conv(x !! 3) < conv(y !! 3)) then True else (if (conv(x !! 3) == conv(y !! 3)) then (if ((x !! 0) < (y !! 0)) then True else False) else False)

get_exam_diff_table :: Table -> Table
get_exam_diff_table x = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"]:(insertionSort_prim comparable_task6 (get_exam_diff_table_aux x))

---ETAPA2---

split_aux :: String -> String
split_aux x = if (last x == ',') then x++"," else x

split_newline ::String -> [String]
split_newline x = foldr op [] x
        where
            op x [] = [[x]]
            op '\n' acc = []:acc
            op x (y:ys) = (x:y):ys

split_comma ::String -> [String]
split_comma x = foldr op [] (split_aux x)
        where
            op ',' acc = []:acc
            op x [] = [[x]]
            op x (y:ys) = (x:y):ys

read_csv_aux :: [String] -> [[String]]
read_csv_aux [] = []
read_csv_aux (x:xs) = ((split_comma x):[])++(read_csv_aux xs) 

read_csv :: CSV -> Table
read_csv x = read_csv_aux (split_newline x)


concat_line :: [String] -> String
concat_line [] = []
concat_line (x:xs) = if (xs == []) then x++(concat_line xs) else x++","++(concat_line xs) 

write_csv :: Table -> CSV
write_csv [] = []
write_csv (x:xs) = if (xs == []) then (concat_line x)++(write_csv xs) else (concat_line x)++"\n"++(write_csv xs) 


--Task1

get_column_index :: Int -> String -> [String] -> Int
get_column_index index a [] = -1
get_column_index index a x  = if (a == (head x)) then index else get_column_index (index+1) a (tail x) 


as_list_aux :: Int -> Table -> [String]
as_list_aux pos [] = []
as_list_aux pos (x:xs) = ((x !! pos):[])++(as_list_aux pos xs)

as_list :: String -> Table -> [String]
as_list a (x:xs) = as_list_aux (get_column_index 0 a x) xs  

--Task2
check_is_number :: Int -> Int -> String -> Int -> Bool
check_is_number acc_digit acc_comma [] z 
                                   |(acc_digit + acc_comma == z && z > 0) = True
                                   |otherwise = False
check_is_number acc_digit acc_comma (x:xs) z
        | (isDigit x) = (check_is_number (acc_digit + 1) acc_comma xs z) 
        | otherwise = if (x == '.') then (check_is_number acc_digit (acc_comma + 1) xs z) else check_is_number acc_digit acc_comma xs z  

check :: String -> String -> Bool
check x y = if ((check_is_number 0 0 x (length x)) && (check_is_number 0 0 y (length y))) then (if (conv x) < (conv y) then True else False) else if (x < y) then True else False


comparable_tsort :: Int -> Row -> Row -> Ordering
comparable_tsort index x y = if ((check (x !! index) (y !! index)) == True) then LT else (if ((x !! index) == (y !! index)) then (if ((x !! 0) <= (y !! 0)) then LT else GT) else GT)



tsort :: String -> Table -> Table
tsort a (x:xs) = x:(sortBy comparable_tsort_first xs)
                  where comparable_tsort_first row1 row2 = comparable_tsort (get_column_index 0 a x) row1 row2

--Task3

vmap :: (Value -> Value) -> Table -> Table
vmap functiontie [] = []
vmap functiontie (x:xs) = ((map functiontie x):[])++(vmap functiontie xs) 

--Task4

op_grade :: Value -> Float -> Float
op_grade a acc = acc + (conv a) 

get_hw_grade_total :: Row -> Row
get_hw_grade_total (x:(y:xs)) = x:((printf "%.2f" (foldr op_grade 0 xs)):[])

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap get_hw_grade_total header (x:xs) = header:(map get_hw_grade_total xs)  

--Task5

column_names :: Row -> Row -> Bool
column_names [] [] = True
column_names (x:xs) (y:ys) = if (x == y) then (column_names xs ys) else False

vunion :: Table -> Table -> Table
vunion (x:xs) (y:ys) = if (column_names x y) then (x:xs)++ys else (x:xs)


--Task6
complete_the_row :: Int -> Row
complete_the_row 0 = []
complete_the_row len = [""]++(complete_the_row (len - 1))

hunion_aux :: Int -> Table -> Table -> Table
hunion_aux len [] [] = []
hunion_aux len (x:xs) [] = ((x++(complete_the_row len)):[])++(hunion_aux len xs [])  
hunion_aux len (x:xs) (y:ys) = ((x++y):[])++(hunion_aux len xs ys)

hunion :: Table -> Table -> Table
hunion x y = hunion_aux (length (head x) - 1) x y

--Task7

find_pair :: String -> Int-> Int -> Table -> [String]
find_pair a index length [] = (complete_the_row length)
find_pair a index length (y:ys) = if (a == (y !! index)) then (tail y) else (find_pair a index length ys)

tjoin_helper :: Int -> Int -> Table -> Table -> Table
tjoin_helper index_x index_y [] y = []
tjoin_helper index_x index_y (x:xs) y = ((x++(find_pair (x !! index_x) index_y (length (head y) - 1) y)):[])++(tjoin_helper index_x index_y xs y)

tjoin :: String -> Table -> Table -> Table
tjoin coloana x y = tjoin_helper (get_column_index 0 coloana (head x)) (get_column_index 0 coloana (head y)) x y

--Task8

cartesian_line_X_table :: (Row -> Row -> Row) -> Row -> Table -> Table
cartesian_line_X_table function linie [] = []
cartesian_line_X_table function linie (x:xs) = ((function linie x):[])++(cartesian_line_X_table function linie xs)

cartesian_aux :: (Row -> Row -> Row) -> Table -> Table -> Table
cartesian_aux function [] y = []
cartesian_aux function (x:xs) y = (cartesian_line_X_table function x y)++(cartesian_aux function xs y)

cartesian ::  (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian function header (x:xs) (y:ys) = header:(cartesian_aux function xs ys)

--Task9

get_a_row :: [String] -> Row -> Row -> Row
get_a_row [] header x = []
get_a_row (coloana:coloana_s) header x = ((x !! (get_column_index 0 coloana header)):[])++(get_a_row coloana_s header x) 


get_rows :: [String] -> Table -> [String] -> Table
get_rows columns [] y = []
get_rows columns (x:xs) y = ((get_a_row columns y x):[])++(get_rows columns xs y) 

projection :: [String] -> Table -> Table
projection columns x = get_rows columns x (head x)


--Etapa3---

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

type EdgeOp = Row -> Row -> Maybe Value
type FilterOp = Row -> Bool

--3.1--
data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
     show (CSV a) = show a
     show (Table b) = write_csv b
     show (List c) = show c

--3.2---
---[String] - primeste header-ul
---In primul parametru de la FilterOp se adauga fiecare linie din tabel
---In FilterCondition se primeste o conditie 
class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval String where
    --gasesc unde pe header se afla b
    --caut valoarea de pe row si compar cu a
    feval header (Eq b a) = y
      where y row = if (row !! (get_column_index 0 b header) == a) then True else False
    feval header (Lt b a) = y
      where y row = if (row !! (get_column_index 0 b header) < a) then True else False
    feval header (Gt b a) = y
      where y row = if ((row !! (get_column_index 0 b header)) > a) then True else False
    feval header (In b a) = y
      where y row = if ((row !! (get_column_index 0 b header)) `elem` a) then True else False
    feval header (FNot condition) = y
      where y row = not(((feval header condition)) row) 
    feval header (FieldEq b a) = y
      where y row = if ((row !! (get_column_index 0 b header)) == (row !! (get_column_index 0 a header))) then True else False

instance FEval Float where
    feval header (Eq b a) = y
      where y row = if (conv (row !! (get_column_index 0 b header)) == a) then True else False
    feval header (Lt b a) = y
      where y row = if (conv (row !! (get_column_index 0 b header)) < a) then True else False
    feval header (Gt b a) = y
      where y row = if (conv (row !! (get_column_index 0 b header)) > a) then True else False
    feval header (In b a) = y
      where y row = if (conv (row !! (get_column_index 0 b header)) `elem` a) then True else False
    feval header (FNot condition) = y
      where y row = not (((feval header condition)) row)      
    feval header (FieldEq b a) = y
      where y row = if ((row !! (get_column_index 0 b header)) == (row !! (get_column_index 0 a header))) then True else False

--- aceasta functie se foloseste la filter
--- creeaza un nou tabel in functie de Filtre 
filter_func :: FilterOp -> Table -> Table
filter_func func [] = []
filter_func func (x:xs)  
                    |(func x == True) = x:(filter_func func xs) 
                    |otherwise = filter_func func xs

new_line_in_graph :: Row -> Row -> Row
new_line_in_graph x xs 
               |((head x) <= (head xs)) = (((head x):[])++((head xs):[])) 
               |otherwise = (((head xs):[])++((head x):[]))
fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a

check_relation_between_lines :: EdgeOp -> Row -> Table -> Table
check_relation_between_lines func x [] = []
check_relation_between_lines func x (xs:xss) 
                |((func x xs) /= Nothing) = ((new_line_in_graph x xs)++(fromMaybe (func x xs):[])):(check_relation_between_lines func x xss) 
                |otherwise  = (check_relation_between_lines func x xss)

graph_creator :: EdgeOp -> Table -> Table
graph_creator func [] = []
graph_creator func (x:xs) = (check_relation_between_lines func x xs)++(graph_creator func xs)

get_table :: Query -> Table
get_table a = read_csv(show (eval a))

class Eval a where
    eval :: a -> QResult

instance Eval Query where
    eval (FromCSV a) = Table (read_csv a)
    eval (ToCSV (FromCSV b)) = CSV b
    eval (AsList a (FromCSV b)) = List (as_list a (read_csv b))
    eval (Sort str (FromCSV b)) = Table (tsort str (read_csv b))
    eval (ValueMap func (FromCSV b)) = Table (vmap func (read_csv b))
    eval (RowMap func str_list (FromCSV b)) = Table (rmap func str_list (read_csv b))
    eval (VUnion (FromCSV a) (FromCSV b)) = Table (vunion (read_csv a) (read_csv b))
    eval (HUnion (FromCSV a) (FromCSV b)) = Table (hunion (read_csv a) (read_csv b))
    eval (TableJoin str (FromCSV a) (FromCSV b)) = Table (tjoin str (read_csv a) (read_csv b))
    eval (Cartesian func str_list (FromCSV a) (FromCSV b)) = Table (cartesian func str_list (read_csv a) (read_csv b))
    eval (Projection str_list (FromCSV a)) = Table (projection str_list (read_csv a))
    eval (Filter condition a) = Table ((head (get_table a)):(filter_func (feval (head (get_table a)) condition) (tail (get_table a))))
    eval (Graph edge_op a) = Table (["From","To","Value"]:(graph_creator edge_op (tail (get_table a))))

my_edge_aux :: Row -> Row -> Int
my_edge_aux [] [] = 0
my_edge_aux (x:xs) (y:ys)
                         |(x == y) = 1 + (my_edge_aux xs ys) 
                         |otherwise = (my_edge_aux xs ys)

my_edge :: Row -> Row -> Maybe Value
my_edge x y
        |(((head x) /= "") && ((head y) /= "")) = (if (p >= 5) then Just (show p) else Nothing)
        |otherwise = Nothing           
          where p = (my_edge_aux (tail x) (tail y)) 

Table x = (eval (Graph my_edge (FromCSV lecture_grades_csv)))
similarities_query = (Sort "Value" (FromCSV (write_csv x)))


---Typos--
---minn :: Int -> Int -> Int -> Int
---minn a b c = if (a <= b && a <= c) then a else (if (b <= a && b <= c) then b else c)
--- functie care gaseste diferenta dintre doua string-uri
editDistance_aux :: String -> String -> Int -> Int -> Int
editDistance_aux xs ys x_len y_len = my_distance_matrix ! (x_len, y_len)
  where my_distance_matrix = array ((0,0),(x_len,y_len)) [((i,j),distance i j) | i <- [0..x_len], j <- [0..y_len]]
        list1 = listArray (1, x_len) xs
        list2 = listArray (1, y_len) ys
        ---distance a b reprezinta "diferenta" dintre doua siruri cu lungimea a si b
        distance 0 0 = 0
        --- cazuri in care un string este vid, se returneaza lungimea celui de al doilea sir, 
        --- pentru ca toate caracterele trebuie modificate
        distance 0 a = a
        distance b 0 = b
        --- se aplica formula programarii dinamice pentru distanta Levenshtein
        distance a b = if (list1 ! a /= list2 ! b)
          then (1 + (minn (my_distance_matrix ! (a, b - 1)) (my_distance_matrix ! (a - 1, b)) (my_distance_matrix ! (a - 1, b - 1)))) 
          else my_distance_matrix ! (a - 1, b - 1) 
          where minn a b c = if (a <= b && a <= c) then a else (if (b <= a && b <= c) then b else c)
---se apeleaza functia
editDistance :: String -> String -> Int
editDistance xs ys = editDistance_aux xs ys (length xs) (length ys)

-- se cauta corespondent pentru fiecare cuvant
get_pair_aux :: String -> Int -> String -> Int -> Table -> String
get_pair_aux res min word b [] = res
get_pair_aux res min word b (y:ys) = if (r < min) then (get_pair_aux (y!!b) r word b ys) else (get_pair_aux res min word b ys)
                                  where r = if (word == (y !! b)) then 0 else (editDistance word (y!!b))

-- se cauta corespondent pentru fiecare cuvant, se considera ca diferenta maxima poate fi 10000000
get_pair :: String -> Int -> Table -> String
get_pair line b y = get_pair_aux "" 10000000 line b y

fix_line_aux :: Int -> Int -> Int -> [String] -> Table -> [String]
fix_line_aux a b c [] y = []
fix_line_aux a b c (line:lines) y = if (a == c) then (get_pair line b y):(fix_line_aux a b (c+1) lines y) else line:(fix_line_aux a b (c+1) lines y)   

-- functie care inlocuieste cuvantul cu cel corect
fix_line :: Int -> Int -> [String] -> Table -> [String]
fix_line a b line y = fix_line_aux a b 0 line y

helper_aux :: Int -> Int -> Table -> Table -> Table
helper_aux a b [] y = []
helper_aux a b (x:xs) y  = (fix_line a b x y):(helper_aux a b xs y)   

helper :: String -> Table -> Table -> CSV
helper column x y = write_csv ((head x):(helper_aux (get_column_index 0 column (head x)) (get_column_index 0 column (head y)) (tail x) (tail y)))

correct_table :: String -> CSV -> CSV -> CSV
correct_table column first second = helper column (read_csv first) (read_csv second)


---Grades Table ---

change_new :: Row->Float
change_new (x:xs) = (foldr op_grade 0 (init xs))/4 + (conv (last xs))

get_hw_grade_total_2 :: Row -> Float
get_hw_grade_total_2 (x:xs) = (foldr op_grade 0 xs)

-- punctaj teme
hw_grade :: [String] -> Table -> Float
hw_grade x [] = -1
hw_grade x (y:ys) = if ((x !! 0) == (y !! 0)) then (get_hw_grade_total_2 y) else (hw_grade x ys)

-- punctaj examen
exam_grade :: [String] -> Table -> Float
exam_grade x [] = -1
exam_grade x (z:zs) = if ((x !! 0) == (z !! 0)) then (change_new z) else (exam_grade x zs)

get_lecture :: Float -> Float -> [String] -> Float
get_lecture sum len [] = (2*(sum/(len - 1)))
get_lecture sum len (t:ts) = get_lecture (sum + (conv t)) len ts

-- punctaj curs
lecture_grade :: [String] -> Table -> Float
lecture_grade x [] = -1
lecture_grade x (t:ts) = if ((x !! 1) == (t !! 0)) then (get_lecture 0 (fromIntegral (length t) :: Float) (tail t)) else (lecture_grade x ts)

add_aux :: Float -> String
add_aux a = if (a == -1) then [] else (printf "%.2f" a) 

-- functie care calculeaza nota finala
final_mark :: Float -> Float -> Float -> String
final_mark a b c = if (a + b < 2.5) then ("4.00") else (if (c < 2.5) then ("4.00") 
                       else (if (a + b < 5) then (printf "%.2f" (a + b + c)) else (printf "%.2f" (5 + c))))

-- campurile lipsa din tabel, au fost inlocuite cu "-1", 
-- iar aceasta functie "repara" acest lucru in tabelul final
fix :: Float -> Float
fix a = if (a /= -1) then (read (printf "%.2f" a) :: Float) else 0  

add :: Float -> Float -> Float -> [String]
add a b c = (((add_aux a):[])++((add_aux b):[])++((add_aux c):[]))++((final_mark (fix a) (fix b) (fix c)):[])

-- functie care creeaza tabelul rezultat folsind fiecare tabel dat ca input
grades_aux :: Table -> Table -> Table -> Table -> Table
grades_aux [] y z t = []
grades_aux (x:xs) y z t = (((x !! 0):[])++(add (hw_grade x y) (lecture_grade x t) (exam_grade x z))):(grades_aux xs y z t)
                    
grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades email_map hw_grades exam_grades lecture_grades = write_csv (tsort "Nume" (
  ["Nume","Punctaj Teme","Punctaj Curs","Punctaj Exam","Punctaj Total"]:grades_aux (tail (read_csv (correct_table "Nume" email_map_csv hw_grades_csv))) (tail (read_csv hw_grades)) (tail (read_csv exam_grades)) (tail (read_csv lecture_grades))))