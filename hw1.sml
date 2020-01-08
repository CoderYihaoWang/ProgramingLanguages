(* 
    Author:        Yihao Wang
    Last modified: 9/1/2020
 *)

(* 1. val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older (first : int * int * int, second : int * int * int) = 
    if #1 first <> #1 second
    then #1 first - #1 second < 0
    else if #2 first <> #2 second
    then #2 first - #2 second < 0
    else  #3 first < #3 second

(* 2. val number_in_month = fn : (int * int * int) list * int -> int *)
fun number_in_month (dates : (int * int * int) list, month : int) = 
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month (tl dates, month)
    else number_in_month (tl dates, month)

(* 3. val number_in_months = fn : (int * int * int) list * int list -> int *)
fun number_in_months (dates : (int * int * int) list, months : int list) = 
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

(* 4. val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month (dates : (int * int * int) list, month : int) = 
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

(* 5. val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months (dates : (int * int * int) list, months : int list) = 
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

(* 6. val get_nth = fn : string list * int -> string *)
fun get_nth (items : string list, index : int) = 
    if index = 1
    then hd items
    else get_nth (tl items, index - 1)

(* 7. val date_to_string = fn : int * int * int -> string *)
fun date_to_string (year : int, month : int, day : int) = 
    let val month_strings = ["January", "February", "March", "April", "May", "June",
                             "July", "August", "September", "October", "November", "December"]
    in get_nth (month_strings, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
    end

(* 8. val number_before_reaching_sum = fn : int * int list -> int *)
fun number_before_reaching_sum (sum : int, nums : int list) = 
    if null nums orelse hd nums >= sum
    then 0
    else 1 + number_before_reaching_sum (sum - hd nums, tl nums)

(* 9. val what_month = fn : int -> int *)
fun what_month days : int = 
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in  1 + number_before_reaching_sum (days, days_in_months)
    end

(* 10. val month_range = fn : int * int -> int list *)
fun month_range (first : int, last : int) =
    if last < first
    then []
    else what_month first :: month_range (first + 1, last)

(* 11. val oldest = fn : (int * int * int) list -> (int * int * int) option *)
fun oldest (dates : (int * int * int) list) = 
    if null dates
    then NONE
    else 
        let fun oldest_non_empty (dates : (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else 
                    let val current_oldest = oldest_non_empty (tl dates) (* avoid computing this more than once *)
                    in  if is_older (hd dates, current_oldest)
                        then hd dates
                        else current_oldest
                    end 
        in  SOME (oldest_non_empty dates)
        end

(* The function unique : int list -> int list is a helper function for both 12-1 and 12-2
    This function could be made a nested function in each of the two challenge functions,
    and doing so will reduce the amount of global variables.
    However, to make it nested in two functions will increase the amount of code and will
    require the function unique to be copied and pasted.
    Here I cannot think of a way to avoid using global variables and avoid repeating myself
    at the same time. So I choose to make this helper function a global one.
    Anyway, the function unique is very general and one may expect to use it elsewhere in the
    program. In practice, functions like this should have already been provided via libraries 
 *)
(* val unique = fn : int list -> int list *)
(* remove duplicated entries in an int list *)
fun unique (items : int list) = 
    (* remove_duplicates : int * int list -> int list *)
    (* return a version of 'items' which does not contain 'target' *)
    let fun remove_duplicates (target : int, items : int list) = 
            if null items
            then []
            else if target = hd items
            then remove_duplicates (target, tl items)
            else hd items :: remove_duplicates (target, tl items)
    in  if null items
        then []
        else hd items :: remove_duplicates (hd items, unique (tl items))
    end

(* 12-1. val number_in_months_challenge = fn : (int * int * int) list * int list -> int *)
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    number_in_months (dates, unique months)


(* 12-2. val dates_in_months_challenge = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =  
    dates_in_months (dates, unique months)

(* 13. val reasonable_date = fn : int * int * int -> bool *)
fun reasonable_date (year : int, month : int, day : int) = 
    let val days_in_months = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        (* an int version of the function get_nth *)
        fun get_nth_int (items : int list, index : int) = 
            if index = 1
            then hd items
            else get_nth_int (tl items, index - 1)
    in  (* first validate the inputs without considering leap years *)
        year > 0                                                            (* validate year  *)
        andalso month > 0 andalso month < 12                                (* validate month *)
        andalso day > 0 andalso day <= get_nth_int (days_in_months, month)  (* validate day   *)
        (* and then deal with leap years *)
        andalso
        (month <> 2                       (* not Feb: ok *)
            orelse day < 29               (* not Feb the 29th: ok*)
                                          (* otherwise, the year has to be a leap year *)
            orelse (year mod 4 = 0 andalso (year mod 100 <> 0 orelse year mod 400 = 0)))
    end