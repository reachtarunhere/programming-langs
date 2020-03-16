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


val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn() => 1) String.size

fun count_some_var (s, p) = g (fn () => 0) (fn x => if s=x then 1 else 0) p

fun check_pat p =
    let
	fun get_strings p =
	    case p of
		Variable x => [x]
	      | TupleP ps => foldl (fn (p, acc) => get_strings p @ acc) [] ps
	      | ConstructorP(_, p) => get_strings p
	      | _ => []

	fun all_unique lst =
	    case lst of
		[] => true
	     |  x::xs' => (not (List.exists (fn y => x=y) xs')) andalso all_unique(xs')
    in
	all_unique (get_strings p)
    end

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i=j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if (length vs) <> (length ps) then NONE else
				    (case all_answers match (ListPair.zip(vs, ps)) of
					  NONE => NONE
					| bindings => bindings)
      | (Constructor (s2, v), ConstructorP (s1,p) ) => if s1=s2 then match(v, p) else NONE
      | _ => NONE


fun first_match v ps = SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
				    
(**** for the challenge problem only ****)
				    

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
