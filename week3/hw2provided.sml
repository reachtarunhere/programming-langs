(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* fun all_except_option(s, lst) = *)
(*     let *)
(* 	fun filter_lst lst = *)
(* 	    case lst of *)
(* 		[] => [] *)
(* 	      | x :: xs' => if same_string(x, s) then filter_lst xs' else x :: filter_lst xs' *)

(* 	val filtered_lst = filter_lst lst *)
(*     in *)
(* 	if filtered_lst = lst then NONE else SOME filtered_lst *)
(*     end				   *)

(* Not sure if it is okay to compare lists for equality. also not tail recursive. The version below is actually used *)

(* Now a tail recursive version that does not use list equality either *)
fun all_except_option(s, lst) =
    let
	fun tail_helper (lst, out_lst, found_yet) = (* could have used just multiple arity fn but then args would not have been clear in function call *)
	    case (lst, out_lst, found_yet) of
		([], out_lst, found_yet) => if found_yet then SOME out_lst else NONE
	      | (x::xs', out_lst, found_yet) => tail_helper (xs', if same_string(s, x) then out_lst else x::out_lst, same_string(s, x) orelse found_yet)
    in
	tail_helper(lst, [], false)
    end
	

fun get_substitutions1(subs, s) =
    case subs of
	[] => []
      | x :: xs' => case all_except_option(s, x) of
		       NONE => get_substitutions1(xs', s)
		     | SOME lst => lst @ get_substitutions1(xs', s)


fun get_substitutions2(subs, s) =
    let
	fun tail_helper(subs, output_lst) =
	    case subs of
		[] => output_lst
	      | x :: xs' => tail_helper(xs', case all_except_option(s, x) of
						 NONE => output_lst
					       | SOME lst => output_lst @ lst)
    in
	tail_helper(subs, [])
    end


fun similar_names(subs, full_name) =
    let
	val {first=orig_f, middle=orig_m, last=orig_l} = full_name
	fun replace_first_name(replacements) =
	    case replacements of
		[] => []
	      | x :: xs' => {first=x, middle=orig_m, last=orig_l} :: replace_first_name xs
											
    in
	full_name :: replace_first_name(get_substitutions2(subs, orig_f))
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
