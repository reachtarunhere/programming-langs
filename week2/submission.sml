fun is_older(date1: (int*int*int), date2: (int*int*int)) =
    if #1 date1 <> #1 date2
    then (if #1 date1 < #1 date2 then true else false)
    else if #2 date1 <> #2 date2
    then (if #2 date1 < #2 date2 then true else false)
    else (if #3 date1 < #3 date2 then true else false)	    


fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else if month = #2 (hd dates)
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

			    
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
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


fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else
	let
	    fun non_empty_oldest(dates: (int*int*int) list) =
		if null (tl dates)
		then hd dates
		else
		    let
			val oldest_in_tl = non_empty_oldest(tl dates)
			val current_date = hd dates
		    in
			(if is_older(current_date, oldest_in_tl) then current_date else oldest_in_tl)
		    end
	in
	    SOME (non_empty_oldest dates)
	end


fun unique_months(months: int list) =
    (* take a unique list of possible months and find and keep if each exists in the supplied list -> 12 passes over provided list -> o(n) *)
    let
	fun element_in_lst(x: int, lst: int list) =
	    if null lst
	    then false
	    else (hd lst = x) orelse element_in_lst(x, tl lst)

	fun filter_months(months_in_year: int list) =
	    if null months_in_year
	    then []
	    else if element_in_lst(hd months_in_year, months)
	    then (hd months_in_year) :: filter_months(tl months_in_year)
	    else filter_months(tl months_in_year)
    in
	filter_months([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
    end


fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    number_in_months(dates, unique_months(months))


fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
    dates_in_months(dates, unique_months(months))
				

fun generic_remove_duplicates(lst: int list) =
    (* not used anywhere just implemented for fun *)
    let
	fun element_in_lst(x: int, lst: int list) =
	    not (null lst) andalso ((hd lst = x) orelse element_in_lst(x, tl lst))

	fun reduce(lst: int list, output_lst: int list) =
	    if null lst
	    then output_lst
	    else if element_in_lst((hd lst), output_lst)
	    then reduce(tl lst, output_lst)
	    (* using @ instead of consing can prevent reversing order *)
	    else reduce(tl lst, (hd lst) :: output_lst)
    in
	reduce(lst, [])
    end
			   
			   
				  
	

				       
				       
					 
	    
	    
	    
	    
						   
	    
