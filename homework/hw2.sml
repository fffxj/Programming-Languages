(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
             
(* 1-a *)
fun all_except_option (str, strs) =
  let
      fun remove_from_list(str_to_remove, strs_remaining) =
        case strs_remaining of
            [] => []
          | x::xs' => if (same_string(x, str_to_remove))
                      then xs'
                      else x :: remove_from_list(str_to_remove, xs')
      val filtered_strs = remove_from_list(str, strs)
  in
      if filtered_strs = strs
      then NONE
      else SOME filtered_strs
  end
  

(* 1-b *)
fun get_substitutions1 (substitutions, str) =
  case substitutions of
      [] => []
    | x::xs' => case all_except_option(str, x) of
                    NONE => get_substitutions1(xs', str)
                  | SOME y => y @ get_substitutions1(xs', str)


(* 1-c *)
fun get_substitutions2 (substitutions, str) =
  let fun aux (substitutions, acc) =
        case substitutions of
            [] => acc
          | x::xs' => case all_except_option(str, x) of
                          NONE => aux(xs', acc)
                        | SOME y => aux(xs', acc @ y)
  in
      aux(substitutions, [])
  end

      
(* 1-d *)
fun similar_names (substitutions, name) =
  let 
      val {first=fst, middle=mid, last=lst} = name
      val similar_first_names = get_substitutions2(substitutions, fst)
      fun generate_names (first_names) =
        case first_names of
            [] => []
          | x::xs' => {first=x, middle=mid, last=lst} :: generate_names(xs')
  in
      name :: generate_names(similar_first_names)
  end

      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun same_card(c1 : card, c2 : card) =
  c1 = c2
           
(* 2-a *)              
fun card_color (s, r) =
  case s of
      Clubs => Black
    | Diamonds => Red
    | Hearts => Red
    | Spades => Black


(* 2-b *)
fun card_value (s, r) =
  case r of
      Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num n => n


(* 2-c *)
fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | x::xs' => if same_card(c, x)
                then xs'
                else x :: remove_card(xs', c, e)


(* 2-d *)                                     
fun all_same_color (cs) =
  case cs of
      [] => true
    | _::[] => true
    | x::y::tl => card_color(x) = card_color(y) andalso all_same_color(y::tl)

                                                                      
(* 2-e *)
fun sum_cards (cs) =
  let fun aux (xs, acc) =
        case xs of
            [] => acc
          | x::xs' => aux(xs', card_value(x) + acc)
  in
      aux(cs, 0)
  end
      

(* 2-f *)
fun score (hcs, goal) =
  let 
      val sum = sum_cards(hcs)
      val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
  in
      if all_same_color(hcs)
      then preliminary_score div 2
      else preliminary_score
  end


(* 2-g *)
fun officiate (cards, moves, goal) =
  let 
      fun aux(card_list, move_list, hold_list) =
        case move_list of
            [] => score(hold_list, goal)
          | (Discard c)::tl
            => aux(card_list, tl, remove_card(card_list, c, IllegalMove)) 
          | Draw::tl => case card_list of
                            [] => score(hold_list, goal)
                          | x::xs' => if sum_cards(x::hold_list) > goal
                                      then score(x::hold_list, goal)
                                      else aux(xs', tl, x::hold_list)
  in
      aux(cards, moves, [])
  end      
