open Oak

let f_simple pkt = 
	if in_range pkt IpSrc [(10,20)] then 
		if in_range pkt IpDst [(0,15)] 
		then Deliver 
		else Drop
	else Drop


let main () = 
	let policy = Oak.compile f_simple in 
	Oak.print_policy policy
	    

let _ = main ()





















































	  

(*
	    
let trusted_ips = [(0,100)]

let firewall pkt = 
	if in_range pkt IpSrc trusted_ips then 
		(add pkt [IpDst; IpSrc] "conn";
		Deliver)
	else 
		(if in_relation pkt [IpSrc; IpDst] "conn"
		then Deliver 
		else Drop)

*)









