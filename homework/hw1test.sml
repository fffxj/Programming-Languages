(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

                                                                  
(* my challenge problems tests *)

val test12_0 = remove_duplicates([2,4,2,1,2,5,7,3,5,7,9,7,8,6,4,7,2,4,2,5,0])
               = [0,1,2,3,4,5,6,7,8,9]

val test12_1 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31)
                                           ,(2011,4,28)],[2,3,4,2,3])
               = 3  
                                       
val test12_2 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31)
                                          ,(2011,4,28)],[2,3,4,2,3])
               = [(2012,2,28),(2011,3,31),(2011,4,28)] 

val test13 = not (reasonable_date((0,2,28))) andalso
             not (reasonable_date((2016,13,28))) andalso
             not (reasonable_date((2016,2,40))) andalso
             reasonable_date((2016,1,1)) andalso
             reasonable_date((2016,12,31)) andalso
             reasonable_date((2016,2,28))
