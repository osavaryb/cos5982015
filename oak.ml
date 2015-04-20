
open Map


    



  
type field = 
      IpSrc 
	| IpDst 

type range = int*int

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
  | Dub of forwarding_decision * forwarding_decision

type decision_tree =
     | Leaf of forwarding_decision
     | Branches of ((packet * decision_tree) list)  * ((packet * decision_tree) list)

      
      

let max_value field = 
	match field with 
	| IpSrc -> 1000
	| IpDst -> 1000

let min_value _ = 0


let intersection (lo,hi) (lo',hi') : range option = 
	assert (lo <= hi);
	assert (lo' <= hi');
	if hi' < lo || hi < lo' 
	then None
	else Some (max lo lo', min hi hi')



(* probably don't need this 					     
let rec intersection' r rs  : range =
	match rs with 
	| [] -> [] 
	| x::xs ->
	    (match (intersection'' r x) with
	    | Some r' -> r'::(intersection' r xs)
	    | None -> (intersection' r xs))



let rec intersection rs1 rs2 : range = 
	match rs1 with 
	| [] -> []
	| r::rs -> (intersection' r rs2) @ (intersection rs rs2)



let union (r1:range) (r2:range) : range = 
	r1 @ r2
*)

(* TODO: instrument with code to generate the decision_tree *)	    
let in_range (p: packet) (f: field) (r: range) : bool = 
	try 
	  let r' = FM.find f p in
	  match intersection r r' with
	  | Some _ -> true
	  | None -> false
	with Not_found -> true




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






