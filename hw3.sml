(*
	Author       : Yihao Wang
	Last modified: 12/1/2020
*)




(* ----- start: Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** for the challenge problem only ****)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* ----- end: Coursera Programming Languages, Homework 3, Provided Code *)




(* 1. val only_capitals = fn : string list -> string list *)
val only_capitals = List.filter (Char.isUpper o (fn l => String.sub (l, 0)))


(* 2. val longest_string1 = fn : string list -> string *)
val longest_string1 = foldl (fn (s, acc) => if String.size s > String.size acc then s else acc) ""


(* 3. val longest_string2 = fn : string list -> string *)
val longest_string2 = foldl (fn (s, acc) => if String.size s >= String.size acc then s else acc) ""


(* 4. val longest_string_helper = fn : (int * int -> bool) -> string list -> string
	  val longest_string3 = fn : string list -> string
	  val longest_string4 = fn : string list -> string
 *)
val longest_string_helper = 
		fn f => foldl (fn (s, acc) => if f (String.size s, String.size acc) then s else acc) ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)


(* 5. val longest_capitalized = fn : string list -> string *)
val longest_capitalized = longest_string1 o only_capitals


(* 6. val rev_string = fn : string -> string *)
val rev_string = String.implode o List.rev o String.explode


(* 7. val first_answer = fn : ('a -> 'b option) -> 'a list -> 'b *)
fun first_answer f l = 
	case l of
		[]    => raise NoAnswer
	  | x::xs => case f x of
	  				NONE   => first_answer f xs
				  | SOME v => v


(* 8. val all_answers = fn : ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f l =
	case l of
		[] => SOME []
	  | x::xs => case f x of 
					NONE     => NONE
				  | SOME ans => case all_answers f xs of 
									NONE      => NONE
								  | SOME ans' => SOME (ans @ ans')


(* 9-a. val count_wildcards = fn : pattern -> int *)
val count_wildcards = g (fn () => 1) (fn _ => 0)


(* 9-b. val count_wild_and_variable_lengths = fn : pattern -> int *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size


(* 9-c. val count_some_var = fn : (string * pattern) -> int *)
fun count_some_var (s, p) = 
	g (fn () => 0) (fn n => if n = s then 1 else 0) p


(* 10. val check_pat = fn : pattern -> bool *)
val check_pat = 
	let fun get_names p = 
			case p of
				Variable x           => x::[]
	          | TupleP ps            => List.foldl (fn (s, acc) => get_names s @ acc) [] ps
			  | ConstructorP (_, pt) => get_names pt
	          | _                    => []
		fun is_unique (x::xs) = not (List.exists (fn a => a = x) xs) andalso is_unique xs
		  | is_unique _       = true
	in  is_unique o get_names
	end


(* 11. val match = fn : (valu * pattern) -> (string * valu) list option *)
fun match (_                  , Wildcard            ) = SOME []
  | match (v                  , Variable s          ) = SOME [(s, v)]
  | match (Unit               , UnitP               ) = SOME []
  | match (Const a            , ConstP b            ) = if   a = b   then SOME []      else NONE
  | match (Constructor (s2, v), ConstructorP (s1, p)) = if   s1 = s2 then match (v, p) else NONE
  | match (Tuple vs           , TupleP ps           ) = if   List.length ps = List.length vs 
														then all_answers match (ListPair.zip (vs, ps)) 
														else NONE
  | match _                                           = NONE

(* 12. val first_match = fn : valu * pattern list -> (string * valu) list option *)
fun first_match v pl = 
	SOME (first_answer (fn p => match (v, p)) pl)
	handle NoAnswer => NONE

(* 13. val typecheck_patterns = fn : ((string * string * typ) list) * (pattern list) -> typ option *)
(* The auto-grader generates the following information, which I still haven't found a way to fix:
	typecheck_patterns: Your function fails 
		when there is no typ that all the patterns in the list can have 
		due to a constructor given an argument with wrong type 
		[incorrect answer]
 *)
fun typecheck_patterns (lst, pl) = 
	    (* get_datatype: converting a constructor name to the corresponding datatype name *)
	let fun get_datatype (ctr, [])            = raise NoAnswer
	      | get_datatype (ctr, (c, d, t)::xs) = if ctr = c then d else get_datatype (ctr, xs) 
		(* pattern_to_typ: converting a pattern to the corresponding typ (the widest one) *) 
		fun pattern_to_typ p =
			case p of 
		        UnitP               => UnitT
		  	  | ConstP _            => IntT
		  	  | ConstructorP (s, _) => Datatype (get_datatype (s, lst))
		  	  | TupleP pl           => TupleT (List.map pattern_to_typ pl)
		  	  | _                   => Anything
		(* get_lenient: compare two typs, get the compatible typ option of both, NONE if they are not compatible  *)
		fun get_lenient ts = 
			case ts of
 				(Anything, other)        => SOME other
		  	  | (other, Anything)        => SOME other
		  	  | (UnitT, UnitT)           => SOME UnitT
		  	  | (IntT, IntT)             => SOME IntT
		  	  | (Datatype x, Datatype y) => if x = y then SOME (Datatype x) else NONE
		  	  | (TupleT x, TupleT y)     => 
		  			if (List.length x) <> (List.length y) then NONE
					else let val  lenients = List.map get_lenient (ListPair.zip (x, y))
							 val  valid = List.foldl (fn (x, acc) => case x of NONE => false | _ => acc) true lenients
					 	 in  if   valid
						  	 then SOME (TupleT (List.map (fn a => case a of SOME t => t | _ => raise NoAnswer) lenients)) 
							 else NONE
					 	 end
		  	  |  _                => NONE
	in List.foldl (fn (x, acc) => case acc of NONE => NONE | SOME t => get_lenient (x, t))
				  (SOME Anything) (List.map pattern_to_typ pl)
				  handle NoAnswer => NONE
	end
