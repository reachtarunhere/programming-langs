val x = 32;
val x = x + 1;

fun append(xs: int list, ys: int list) =
    if null xs
    then ys
    else  (hd xs):: append(tl xs, ys)
	
fun sum_list(xs: int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs)


fun list_max(xs: int list) =
    if null xs
    then NONE
    else
	let
	    fun non_empty_max(xs: int list) =
		if null (tl xs)
		then hd xs
		else
		    let
			val remaining_max = non_empty_max(tl xs)
		    in
			if remaining_max > hd xs
			then remaining_max
			else hd xs
		    end
	in
	    SOME (non_empty_max xs)
	end
			
			
    
			  
