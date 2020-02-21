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

fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
	if month = #2 (hd dates)
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

			    
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
	if #2 (hd dates) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

			   
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(xs: string list, n: int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)


fun date_to_string(date: (int*int*int)) =
    let
	val month_names = ["January", "February", "March", "April", "May",
		       "June", "July", "August", "September", "October",
		       "November", "December"]
			  
    in
	get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum(sum: int, xs: int list) =
    if sum <= 0
    then ~1
    else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)


fun what_month(day_of_year: int) =
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day_of_year, days_in_month) + 1
    end


fun month_range(doy1: int, doy2: int) =
    if doy1 > doy2
    then []
    else what_month(doy1) :: month_range(doy1+1, doy2)

    
	
