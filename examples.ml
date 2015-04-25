open Oak

let f_simple pkt = 
	if in_range pkt IpSrc [(10,20)]
	then if in_range pkt IpDst [(0,15)] then Deliver else Drop
	else Drop

let trusted_ips = [(0,100)]

let firewall pkt = 
	if in_range pkt IpSrc trusted_ips then 
		(add pkt [IpDst; IpSrc] "conn";
		Deliver)
	else 
		(if in_relation pkt [IpSrc; IpDst] "conn"
		then Deliver 
		else Drop)

let firewall_test_packets = [ [(IpSrc,150); (IpDst, 10)] ; [(IpSrc,10); (IpDst, 150)]; [(IpSrc,150); (IpDst, 10)]; [(IpSrc, 150); (IpDst, 9)]]	    
	    

let main () = 
	let policy = Oak.compile firewall in 
	let fds = Oak.string_of_policy policy in
	print_endline fds;
	run policy firewall_test_packets 

let _ = main ()
