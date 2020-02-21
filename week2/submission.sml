fun is_older(date1: (int*int*int), date2: (int*int*int)) =
    let
	val year1 = #1 date1
	val year2 = #1 date2
	val month1 = #2 date1
	val month2 = #2 date2
	val day1 = #3 date1
	val day2 = #3 date2
    in
	if year1 > year2
	then false
	else
	    if year1 < year2
	    then true
	    else
		if month1 > month2
		then false
		else
		    if month1 < month2
		    then true
		    else
			if day1 > day2
			then false
			else
			    if day1 < day2
			    then true
			    else false
    end



(* it sucks to write stuff without else if *)
	    
