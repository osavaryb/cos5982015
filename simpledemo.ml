open Oak

let f_simple1 pkt = 
	if in_range pkt IpSrc [(10,20)] then 
		if in_range pkt IpDst [(0,15)] 
		then Deliver 
		else Drop
	else Drop

let f_simple2 pkt = 
	if in_range pkt IpSrc [(14,16)] then 
		if in_range pkt IpDst [(10,20)] 
		then Deliver 
		else Drop
	else Drop

let f_one (f_cont) pkt =
 f_cont pkt

let f_zero (f_cont) pkt =
 Drop



	    
let f_cross (f_1) (f_2) pkt =
  let d_1 = f_1 pkt in
  let d_2 = f_2 pkt in
  Multicast (d_1,d_2)

    

let main () = 
  let pol = Oak.compile (f_cross f_simple1 f_simple2) in
  print_policy pol
	    

let _ = main ()






	  



