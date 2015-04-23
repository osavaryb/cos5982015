open Oak

let f_simple pkt = 
	if in_range pkt IpSrc [(10,20)]
	then if in_range pkt IpDst [(0,15)] then Deliver else Drop
	else Drop

let main () = 
	let policy = Oak.compile f_simple in 
	let fds = Oak.string_of_policy policy in 
	print_endline fds

let _ = main ()
