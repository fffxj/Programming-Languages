(* 1 *)
fun is_older(date1 : int * int * int, date2 : int * int * int) =
  (#1 date1 < #1 date2)
  orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
  orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)


(* 2 *)
fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else
      let
          val tl_number_in_month = number_in_month(tl dates, month)
      in
          if #2 (hd dates) = month
          then 1 + tl_number_in_month
          else tl_number_in_month
      end


(* 3 *)
fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* 4 *)
fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else
      let
          val tl_dates_in_month = dates_in_month(tl dates, month)
      in
          if #2 (hd dates) = month
          then (hd dates) :: tl_dates_in_month
          else tl_dates_in_month
      end


(* 5 *)
fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)



(* 6 *)
fun get_nth(strings : string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n - 1)


(* 7 *)
fun date_to_string(date : int * int * int) =
  let
      val name_of_months = ["January", "February", "March", "April", "May",
                            "June", "July", "August", "September", "October",
                            "November", "December"]
  in
      get_nth(name_of_months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^
      Int.toString(#1 date)
  end


(* 8 *)
fun number_before_reaching_sum(sum : int, numbers : int list) =
  if hd numbers >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)


(* 9 *)
fun what_month(day : int) =
  let
      val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      1 + number_before_reaching_sum(day, days_in_month)
  end


(* 10 *)
fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)


(* 11 *)
fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else
      let
          fun oldest_nonempty(dates : (int * int * int) list) =
            if null (tl dates)
            then hd dates
            else
                let
                    val tl_oldest = oldest_nonempty(tl dates)
                in
                    if is_older(hd dates, tl_oldest)
                    then hd dates
                    else tl_oldest
                end
      in
          SOME (oldest_nonempty(dates))
      end


(* 12 *)
(* fn sort: sort from small to large *)
fun sort(months : int list) =
  let
      fun merge(x : int, ys : int list) =
        if null ys
        then x :: []
        else if x < hd ys
        then x :: ys
        else (hd ys) :: merge(x, tl ys)
  in
      if null months
      then []
      else if null (tl months)
      then months
      else
          let
              val tl_sorted = sort(tl months)
          in
              if hd months < hd tl_sorted
              then months
              else merge(hd months, tl_sorted)
          end
  end

fun remove_duplicates(months : int list) =
  let
      val sorted_months = sort months
  in
      if null sorted_months
      then []
      else if null (tl sorted_months)
      then sorted_months
      else
          let
              val tl_remove_duplicates = remove_duplicates(tl sorted_months)
          in
              if hd sorted_months = hd tl_remove_duplicates
              then tl_remove_duplicates
              else (hd sorted_months) :: tl_remove_duplicates
          end
  end


fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
  number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
  dates_in_months(dates, remove_duplicates(months))


(* Challenge Problem *)

(* 13 *)
fun reasonable_date(date : int * int * int) =
  let
      fun is_leapyear(year : int) =
        year mod 4 = 0 andalso year mod 100 <> 0 orelse year mod 400 = 0
      val leap_month_limit = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      val normal_month_limit = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      fun get_nth(numbers : int list, n : int) =
        if n = 1
        then hd numbers
        else get_nth(tl numbers, n - 1)
  in
      if #1 date < 1 orelse #2 date < 1 orelse #2 date > 12 orelse
         #3 date < 1 orelse #3 date > 31
      then false
      else if is_leapyear(#2 date)
      then #3 date <= get_nth(leap_month_limit, #2 date)
      else #3 date <= get_nth(normal_month_limit, #2 date)
  end
