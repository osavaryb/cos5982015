open Oak


let trusted_ips = [(0,100)]
		
let firewall pkt = 
	if in_range pkt IpSrc trusted_ips then 
		(add pkt [IpDst; IpSrc] "conn";
		Deliver)
	else 
		(if in_relation pkt [IpSrc; IpDst] "conn"
		then Deliver 
		else Drop)


let main () = 
	Oak.compile firewall

	    

let _ = main ()







