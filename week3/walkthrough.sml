datatype mytype = OneInt of int | Pizza;

val something = Pizza;
(* Now let's try treating Pizza as a function with zero args *)
(* val something_else = Pizza (); *)
(* Can't do the above since they are values not functions like constructors that take args *)

(* fun f x = *)
(*     case x of *)
(* 	Pizza => "hello" *)
(*      |  OneInt(i) => i + 1  *)

(* Obviously you can't return different type from the same function like above *)

fun f x =
    case x of
	Pizza => 0
     |  OneInt(i) => i + 1


datatype exp = Constant of int | Negate of exp | Add of exp * exp | Mul of exp * exp

fun eval exp =
    case exp of
	Constant i => i
      | Negate e => ~ (eval e)
      | Add(e1, e2) => (eval e1) + (eval e2)
      | Mul(e1, e2) => (eval e1) * (eval e2)


fun largest_const exp =
    case exp of
	Constant i => i
      | Negate e => largest_const e
      | Add(e1, e2)  => Int.max((eval e1), (eval e2))
      | Mul(e1, e2)  => Int.max((eval e1), (eval e2))

fun all_const exp =
    case exp of
	Constant i => [i]
      | Negate e => all_const e
      | Add(e1, e2) => (all_const e1) @ (all_const e2)
      | Mul(e1, e2) => (all_const e1) @ (all_const e2)
