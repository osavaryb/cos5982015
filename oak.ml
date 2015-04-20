
open Map


type relation = string

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

type forwarding_decision =
	| Deliver
	| Drop
	| ForwardTo of int
	| Multicast of forwarding_decision * forwarding_decision

type decision_tree =
	| Root of decision_tree option
    | Leaf of forwarding_decision
    | Add of field list * relation * decision_tree ref 
    | Remove of field list * relation * decision_tree ref 
    | Inrange of range * field * ((packet * decision_tree) option) * ((packet * decision_tree) option)



(* Global mutable decision tree *)
let dtree = ref (Root None)
let loc = dtree


let run (f: packet -> forwarding_decision) : unit = 
	let sym_pkt = FM.empty in 
	(* while ... do f sym_pkt; *)
	()




let max_value = 1000
let min_value = 0

let intersection'' ((lo,hi):int*int) (lo',hi') : range = 
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

let complement' (lo,hi) : range = 
	match lo=min_value, hi=max_value with 
	| true, true ->  []
	| true, false -> [(hi,max_value)]
	| false, true -> [(min_value, lo)]
	| false, false -> [(min_value,lo); (hi,max_value)]

let complement (r: range) : range = 
	let comps = List.map complement' r in 
	List.fold_left (fun acc c -> intersection acc c) [(min_value,max_value)] comps


let in_range (p: packet) (f: field) (r: range) : bool = 
	try 
	  let r' = FM.find f p in
	  match intersection r r' with
	  | [] -> false
	  | _ -> true
	with Not_found -> 
		(match !dtree with 
		 | Leaf fd -> 
		 | Branches tru fal ->  )  *)




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

let test_complement1 () = 
	let r = [(10,20)] in
	print_range (complement r)

let test_complement2 () = 
	let r = [] in
	print_range (complement r)

let test_complement3 () = 
	let r = [(10,20); (900,max_value)] in
	print_range (complement r)

let rec run_tests tests = 
	match tests with 
	| [] -> ()
	| t::ts -> 
		t (); 
		print_endline ""; 
		run_tests ts

let () = 
	let tests = 
		[test_intersection; 
		 test_complement1; 
		 test_complement2; 
		 test_complement3] in 
	run_tests tests






