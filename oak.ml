
open Map

type field = 
      IpSrc 
	| IpDst 

type range = (int*int) list

module OrderedField = struct
	type t = field
	let compare f1 f2 = compare f1 f2
end

module FM = Map.Make(OrderedField)

type packet = range FM.t


let max_value field = 
	match field with 
	| IpSrc -> 1000
	| IpDst -> 1000

let min_value _ = 0


let intersection'' (lo,hi) (lo',hi') : range = 
	assert (lo <= hi);
	assert (lo' <= hi');
	if lo' > hi || hi < lo' 
	then []
	else [(max lo lo', min hi hi')]


let rec intersection' r rs  : range =
	match rs with 
	| [] -> [] 
	| x::xs -> (intersection'' r x) @ (intersection' r xs)



let rec intersection rs1 rs2 : range = 
	match rs1 with 
	| [] -> []
	| r::rs -> (intersection' r rs2) @ (intersection rs rs2)


let union (r1:range) (r2:range) : range = 
	r1 @ r2


let in_range (p: packet) (f: field) (r: range) : bool = 
	try 
		let r' = FM.find f p in 
		(intersection r r') = r'
	with Not_found -> true




(******************************************************
 *
 *  Testing...
 *
 ******************************************************)

let print_interval (lo,hi) = 
	print_endline ("lo:" ^ (string_of_int lo) ^ " hi: " ^ (string_of_int hi))

let print_range r = 
	List.iter print_interval r


let test_intersection () = 
	let r1 = [(15,55)] in 
	let r2 = [(10,20); (50,60)] in 
	print_range (intersection r1 r2)

let () = 
	test_intersection ()






