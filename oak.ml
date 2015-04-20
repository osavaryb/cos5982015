
open Map


type relation = string

type field = 
      IpSrc 
	| IpDst 

type range = int*int list

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
    | Add of field list * relation * decision_tree
    | Remove of field list * relation * decision_tree
    | Inrange of range * field * ((packet * decision_tree) option) * ((packet * decision_tree) option)



(* Global mutable decision tree *)
let dtree = ref (Root None)



let run (f: packet -> forwarding_decision) : unit = 
	let sym_pkt = FM.empty in 
	(* f sym_pkt; *)
	()






let max_value field = 
	match field with 
	| IpSrc -> 1000
	| IpDst -> 1000

let min_value _ = 0

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





let in_range (p: packet) (f: field) (r: range) : bool = 
	try 
	  let r' = FM.find f p in
	  match intersection r r' with
	  | Some _ -> true
	  | None -> false
	with Not_found -> true (*
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


let test_intersection () = 
	let r1 = (15,55) in 
	let r2 = (10,20) in
	match (intersection r1 r2) with
	| Some ran -> print_interval ran
	| None -> ()

let () = 
	test_intersection ()






