
open Map
open Stack

let debug = false

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

type packet = (range FM.t) ref

type forwarding_decision =
	| Deliver
	| Drop
	| ForwardTo of int
	| Multicast of forwarding_decision * forwarding_decision

type decision_tree =
	| Dummy
    | Leaf of packet * forwarding_decision
    | Add of field list * relation * decision_tree ref 
    | Remove of field list * relation * decision_tree ref 
    | Inrange of range * field * (decision_tree ref) * (decision_tree ref)


(******************************************************
 *
 *  Printing
 *
 ******************************************************)
	  
let string_of_field f = 
	match f with 
	| IpSrc -> "ipsrc"
	| IpDst -> "ipdst"

let string_of_interval (lo,hi) = 
	"[" ^ (string_of_int lo) ^ ", " ^ (string_of_int hi)^ "]"

let string_of_range (r : range) = 
	List.fold_left (fun acc x -> (if acc = "" then "" else acc^"U")^(string_of_interval x)) "" r 

let string_of_packet (pkt : packet) = 
	let fields = 
		FM.fold 
			(fun f r acc ->
				let comb = (if acc = "" then "" else " || "^acc) in 
				(string_of_field f) ^ " :" ^ (string_of_range r) ^ comb
			) !pkt "" in 
    "<<" ^ fields ^ ">>"
    
let rec string_of_fd (fd : forwarding_decision) =
  match fd with
  | Deliver -> "Deliver"
  | Drop -> "Drop"
  | ForwardTo i -> "Forward to "^ (string_of_int i)
  | Multicast (fd1, fd2) -> "Multicasted: " ^  (string_of_fd fd1) ^ " and " ^ (string_of_fd fd2)

let rec repeat_str (n:int) (s:string) = 
	if n = 0 then ""
	else s ^ (repeat_str (n-1) s)

let print_dtree (dt: decision_tree) : unit = 
	let rec aux dt level = 
		let str = repeat_str level "\t" in 
		match dt with
		| Dummy -> print_endline (str ^ "Dummy")
		| Leaf (pkt,fd) -> print_endline (str ^ (string_of_fd fd))
		| Inrange (r,f,tru,fal) -> 
		    print_endline (str ^ "Inrange: field=" ^ (string_of_field f) ^ " range=" ^ (string_of_range r));
		    aux !tru (level + 1);
		    aux !fal (level + 1)
		| _ -> failwith "Error Unimplemented"
	in 
	aux dt 0

let rec print_fds (dt: decision_tree) : unit = 
	match dt with 
	| Dummy -> failwith "Unexplored Branch"
	| Leaf (pkt,fd) -> print_endline ((string_of_packet pkt) ^ " -> " ^ (string_of_fd fd))
	| Inrange (_,_,tru,fal) -> print_fds !tru; print_fds !fal
	| _ -> failwith "Error Unimplemented"


(******************************************************
 *
 *  Decision Tree
 *
 ******************************************************)

(* Global mutable decision tree *)
let root = ref Dummy
let loc = ref root
let next_pkts = ref (Stack.create ())

let run (f: packet -> forwarding_decision) : unit = 
	let sym_pkt = ref FM.empty in 
	Stack.push sym_pkt !next_pkts;
	while not (Stack.is_empty !next_pkts) do
	   let cur_pkt = Stack.pop !next_pkts in
	   if debug then print_endline ("current packet is: "^ (string_of_packet cur_pkt)) else ();
		let fd = f cur_pkt in 
		(!loc) := Leaf (cur_pkt, fd);
		loc := root;
		if debug then print_dtree !root else (); 
	done

let max_value = 1000
let min_value = 0
let total_range = [(min_value,max_value)]

let intersection'' ((lo,hi):int*int) (lo',hi') : range = 
	assert (lo <= hi);
	assert (lo' <= hi');
	if  hi < lo' || hi' < lo
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
	| true, false -> [(hi+1,max_value)]
	| false, true -> [(min_value, lo-1)]
	| false, false -> [(min_value,lo-1); (hi+1,max_value)]

let complement (r: range) : range = 
	let comps = List.map complement' r in 
	List.fold_left (fun acc c -> intersection acc c) [(min_value,max_value)] comps

let normalize_range (r: range) : range =
	let rec aux r = 
		match r with 
		| [] -> []
		| [x] -> [x]
		| x::y::tl ->
			let (lo,hi) = x in 
			let (lo',hi') = y in  
			if lo' <= hi then 
				let merged = (lo, max hi hi') in 
				aux (merged::tl)
			else 
				x::(aux (y::tl))
	in 
	let sorted = List.sort compare r in 
	aux sorted
	  
let in_range (p: packet) (f: field) (r: range) : bool = 
	let aux x y  = 
		match !(!loc) with 
		| Dummy -> 
			let ptru = FM.add f x !p in 
			let pfal = FM.add f y !p in 
		 	let new_tru, new_fal = ref Dummy, ref Dummy in 
		 	(!loc) := Inrange (r,f, new_tru, new_fal);
		 	(match x,y with 
		 	 | [], [] -> failwith "Both empty"
		 	 | [], _ -> loc := new_fal; p := pfal; false
		 	 | _, [] -> loc := new_tru; p := ptru; true
			 | _, _ ->  loc := new_tru; p := ptru; Stack.push (ref pfal) !next_pkts; true)
		| Inrange (r',f', tru, fal) -> 
		 	if r' = r && f' = f then 
			  (match x with
               | [] -> (loc := fal; false)
			   | _ -> (loc := tru; true))
		 	else failwith "Error [in_range: different values]"
		| _ -> failwith "Error [in_range: unhandled case]"
	in 
	let r' = try FM.find f !p with _ -> total_range in
	if debug then 
		let field_str = "in in_range for field "^(string_of_field f) in 
		let inter_str = " with inter:"^(string_of_range (intersection r r')) in 
		print_endline (field_str ^ inter_str)
	else ();
	aux (normalize_range (intersection r r')) (normalize_range (intersection (complement r) r')) 
		


(******************************************************
 *
 *  Testing...
 *
 ******************************************************)

let test_intersection () = 
	let r1 = [(15,55)] in 
	let r2 = [(10,20); (50,60)] in 
	print_endline (string_of_range (intersection r1 r2))

let test_complement1 () = 
	let r = [(10,20)] in
	print_endline (string_of_range (complement r))

let test_complement2 () = 
	let r = [] in
	print_endline (string_of_range (complement r))

let test_complement3 () = 
	let r = [(10,20); (900,max_value)] in
	print_endline (string_of_range (complement r))

let f_simple pkt = 
	if in_range pkt IpSrc [(10,20)]
	then if in_range pkt IpDst [(0,15)] then Deliver else Drop
	else Drop

let rec run_tests tests = 
	match tests with 
	| [] -> ()
	| t::ts -> 
		t (); 
		print_endline ""; 
		run_tests ts

let () = 
	(* let tests = 
		[test_intersection; 
		 test_complement1; 
		 test_complement2; 
		 test_complement3] in 
	run_tests tests; *)
	run f_simple;
 	if debug then () else print_fds !root







