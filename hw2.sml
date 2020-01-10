(* 
    Author        : Yihao Wang
    Last modified : 10/1/2020
 *)




(* ----- start: Dan Grossman, Coursera PL, HW2 Provided Code ----- *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
(* ----- end: Dan Grossman, Coursera PL, HW2 Provided Code ----- *)




(* ----- start of problem 1 ----- *)
(* a. val all_except_option = fn : string * string list -> string list option *)
fun all_except_option (x, xs) = 
    let fun is_in (_, []) = false
          | is_in (s, x::xs) = same_string (s, x) orelse is_in (s, xs)
        fun all_except (_, []) = []
          | all_except (s, x::xs) = 
                if same_string (s, x) then xs else x::all_except(s, xs) (* assumes only 1 occurance *)
    in  if is_in (x, xs) then SOME (all_except (x, xs)) else NONE
    end


(* b. val get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1 ([], _) = [] 
  | get_substitutions1 (x::xs, s) = 
        case all_except_option (s, x) of
            NONE     => get_substitutions1 (xs, s)
          | SOME lst => lst @ get_substitutions1 (xs, s) 


(* c. val get_substitutions2 = fn : string list list * string -> string list *)
fun get_substitutions2 (subs, s) = 
    let fun aux ([], _, acc) = acc
          | aux (x::xs, s, acc) = 
                case all_except_option (s, x) of
                    NONE     => aux (xs, s, acc)
                  | SOME lst => aux (xs, s, acc @ lst)
    in  aux (subs, s, [])
    end


(* d. val similar_names = fn : string list list * {string, string, string} -> {string, string, string} list *)
fun similar_names (subs, {first=x, middle=y, last=z}) = 
    let fun substitute ([], _) = []
          | substitute (s::ss, {first=x, middle=y, last=z}) = 
                {first=s, middle=y, last=z}::(substitute (ss, {first=x, middle=y, last=z}))
    in  {first=x, middle=y, last=z}
        ::substitute (get_substitutions2 (subs, x), {first=x, middle=y, last=z})
    end


(* ----- end of problem 1 ----- *)




(* ----- start: Dan Grossman, Coursera PL, HW2 Provided Code ----- *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(* ----- end: Dan Grossman, Coursera PL, HW2 Provided Code ----- *)




(* ----- start of problem 2 ----- *)
(* a. val card_color = fn : card -> color *)
fun card_color (s, _) = 
    case s of
        Diamonds => Red
      | Hearts   => Red
      | _        => Black


(* b. val card_value = fn : card -> int *)
fun card_value (_, r) = 
    case r of 
        Num i => i
      | Ace   => 11
      | _     => 10


(* c. val remove_card = fn : card list * card * exn -> card list raise exn *)
fun remove_card (cards, card, e) = 
    let fun is_in ([], _) = false
          | is_in (x::xs, c) = x = c orelse is_in (xs, c)
    in  if   is_in (cards, card)
        then case cards of
                x::xs => if x = card then xs else x::remove_card(xs, card, e) (* only removes the first occurance *)
              | _ => [] (* will never reach this branch *)
        else raise e
    end


(* d. val all_same_color = fn : card list -> bool *)
fun all_same_color (x::nk::xs) = 
        (card_color x) = (card_color nk) andalso all_same_color (nk::xs) 
  | all_same_color _ = true (* zero or only one card is always of the same color *)


(* e. val sum_cards = fn : card list -> int *)
fun sum_cards cards =
    let fun aux ([], acc) = acc
          | aux (x::xs, acc) = aux (xs, acc + (card_value x))
    in  aux (cards, 0)
    end


(* f. val score = fn : card list * int -> int *)
fun score (cards, goal) =
    let val sum = sum_cards cards
        val pre = if goal >= sum then goal - sum else 3 * (sum - goal)
    in  if all_same_color cards then pre div 2 else pre
    end


(* g. val officiate = fn : card list * move list * int -> int *)
fun officiate (cl, mv, gl) = 
    (* move (held: card list, cards: card list, moves: move list, goal: int) -> int *)
    let fun move (held, _, [], gl) = score (held, gl)       (* no more moves *)
          | move (held, [], Draw::_, gl) = score (held, gl) (* drawing from an empty card list *)
          | move (held, c::cs, Draw::ms, gl) =              (* successfal drawing *)
                if   sum_cards (c::held) > gl
                then score (c::held, gl)
                else move (c::held, cs, ms, gl)
          | move (held, cl, Discard d::ms, gl) = move (remove_card(held, d, IllegalMove), cl, ms, gl)
                                                            (* discarding a card from held cards *)
    in  move ([], cl, mv, gl)
    end


(* ----- end of problem 2 ----- *)




(* ----- start of problem 3 ----- *)

(* two helper functions used by both challenge functions in this problem *)
(* returns a list of all possible sums of a deck of card, in which each Ace can be 1 or 11 *)
fun sums_cards cards = 
    (* this function return a tuple comprised of a sum which is the same as the old version, 
       and the number of Aces *)
    let fun aux ([], sum, n) = (sum, n)
          | aux (x::xs, sum, n) = 
                if   card_value x = 11
                then aux (xs, sum + (card_value x), n + 1)
                else aux (xs, sum + (card_value x), n)
        (* this function takes in the tuple returned by the above function, 
           and makes a list of all possible sums *)
        fun get_sums (sum, n) =
                if   n = 0 then sum::[]
                else sum::(get_sums (sum - 10, n - 1))
    in  get_sums (aux (cards, 0, 0))
    end

(* this function returns the smallest number in a list. 
   The caller must make sure that the passed in list is non-empty *)
fun get_smallest (x::[]) = x
  | get_smallest (x::nk::xs) = 
        if x < nk then get_smallest (x::xs) else get_smallest (nk::xs)
  | get_smallest _ = ~1 (* should never reach this branch *)


(* a-1. val score_challenge = fn : card list * int -> int *)
fun score_challenge (cards, goal) = 
    (* get a sum and a goal, returns the score *)
    let fun get_score (sum, goal) = 
            let val pre = if goal >= sum then goal - sum else 3 * (sum - goal)
            in  if all_same_color cards then pre div 2 else pre
            end
        (* calls get_score on a list of sums, and returns a list of scores *)
        fun get_scores ([], _) = []
          | get_scores (s::ss, goal) = 
                get_score (s, goal)::(get_scores (ss, goal))
    in  get_smallest (get_scores (sums_cards cards, goal)) 
    end


(* a-2. val officiate_challenge = fn : card list * move list * int -> int *)
fun officiate_challenge (cl, mv, gl) = 
    (* almost the same as the old version, except for substitute score_challenge for score
       and get_smallest (sums_cards (...)) for sum_cards (...) *)
    let fun move (held, _, [], gl) = score_challenge (held, gl)
          | move (held, [], Draw::_, gl) = score_challenge (held, gl)
          | move (held, c::cs, Draw::ms, gl) =  
                if   get_smallest (sums_cards (c::held)) > gl
                then score_challenge (c::held, gl)
                else move (c::held, cs, ms, gl)
          | move (held, cl, Discard d::ms, gl) = move (remove_card(held, d, IllegalMove), cl, ms, gl)
    in  move ([], cl, mv, gl)
    end


(* b. val careful_player = fn : card list * int -> move list *)
(* This solution implements the following strategy:
    if the goal is already achieved exactly, then end game.
    Otherwise, if there are no cards in the card list,
    the player must attempt to draw and end game, as discarding any card only makes her farther from the goal.
    If the card list is not empty, then have a look at the next card in the card list,
    if its value is smaller than the gap between the current score and the goal,
    then draw it, as doing so will always make the player closer to the goal,
    this guarentees the requirement that if the gap is greater than 10 then the next move must be a draw.
    If the value of next card is greater than the gap, however,
    then have a look at the held card, if there exist one or more cards
    which if discarded will make the gap larger than the next card's value,
    then discard the smallest one of them and draw a card.
    This will guarantee that if discarding one card and drawing one card can lead to the goal
    then the player must do so.
    If no cards at hand can achieve this, then just stop game, as discarding any card can only make
    the player farther from the goal
 *)
fun careful_player (cl, gl) = 
    (* get_closest_option : find the card in the list,
       which is the smallest among all cards that are greater than or equal to a given target
       return NONE if no such card exists *)
    let fun get_closest ([], _) = NONE
          | get_closest (c::[], target) = 
                if card_value c >= target then SOME c else NONE
          | get_closest (c::nk::cs, target) = 
                let val cv = card_value c
                    val nv = card_value nk
                in  if   cv < target andalso nv < target
                    then get_closest (cs, target)
                    else if cv < target orelse (nv >= target andalso cv >= nv)
                    then get_closest (nk::cs, target)
                    else get_closest (c::cs, target)
                end
        (* implements the strategy described above *)
        fun play (held, cl, mv, gl) = 
                let val gap = gl - (sum_cards held) in
                if  gap = 0 then mv else
                case cl of
                    [] => mv @ (Draw::[])
                  | c::cs => 
                        if   card_value c <= gap
                        then play (c::held, cs, mv @ (Draw::[]), gl) else
                        case get_closest (held, card_value c - gap) of
                             SOME cd => play (c::remove_card (held, cd, IllegalMove), cs, 
                                                mv @ (Discard cd :: (Draw::[])), gl)
                           | NONE    => mv
                end
    in  play ([], cl, [], gl)
    end
