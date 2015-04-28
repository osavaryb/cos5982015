open Oak


let trusted_ips = [(0,100)]


let f_simple pkt = 
	if in_range pkt IpSrc [(10,20)] then 
		if in_range pkt IpDst [(0,15)] 
		then Deliver 
		else Drop
	else Drop

    
let firewall pkt = 
	if in_range pkt IpSrc trusted_ips then 
		(add pkt [IpDst; IpSrc] "conn";
		Deliver)
	else 
		(if in_relation pkt [IpSrc; IpDst] "conn"
		then Deliver 
		else Drop)

let f_cross (f_1) (f_2) pkt =
  let d_1 = f_1 pkt in
  let d_2 = f_2 pkt in
  Multicast (d_1,d_2)

	    
let main () =
    let pol = Oak.compile (f_cross firewall firewall) in
    print_policy pol
      

	    

let _ = main ()







