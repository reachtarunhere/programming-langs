(* Coursera Programming Languages, Homework 3, Provided Code *)

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

fun only_capitals lst = List.filter (fn x => Char.isUpper(String.sub(x, 0))) lst

fun longest_string1 lst = foldl (fn (s, acc) => if String.size(s) > String.size(acc) then s else acc) "" lst

fun longest_string2 lst = foldl (fn (s, acc) => if String.size(s) >= String.size(acc) then s else acc) "" lst

fun longest_string_helper cmp lst = foldl (fn (s, acc) => if cmp(String.size s, String.size acc) then s else acc) "" lst

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v


fun all_answers f lst =
    let
	val applied_lst = map f lst
	val NONEinLst = foldl (fn (x, acc) => case x of NONE => true | _ => acc) false (* fn to check if applied lst has any NONE *)
	val CombineSOMElst = foldl (fn (SOME v, acc) => v @ acc) [] (* take SOME lst1 SOME lst2 .. and return lst *)
    in
	if NONEinLst applied_lst then NONE else SOME (CombineSOMElst applied_lst)
    end

	

		
	
				    
(**** for the challenge problem only ****)
				    

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
