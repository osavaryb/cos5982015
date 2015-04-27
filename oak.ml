(* TODO : verify when in_relation branching  that choice hasn't been made yet*)

  
open Map
open Stack

let debug = false
let o1 = true
    
type relation = string

type field = 
      IpSrc 
	| IpDst 


(* range is a disjunction of intervals. For example, [ (10,20), (25,100)] represents the range [10, 20]U[25,100] *)
type range = (int*int) list

module OrderedField = struct
	type t = field
	let compare f1 f2 = compare f1 f2
end

module FM = Map.Make(OrderedField)

(* symbolic packet consists of a mapping between fields and ranges *)
type packet' = range FM.t
type packet = (packet' * (relation*(field list)) list * (relation*(field list)) list) ref 


type forwarding_decision =
	| Deliver
	| Drop
	| ForwardTo of int
	| Multicast of forwarding_decision * forwarding_decision
	| Ctrl

type decision_tree =
  | Dummy
    | Leaf of packet * forwarding_decision   
    | Add of packet * field list * relation * decision_tree ref 
    | Remove of packet * field list * relation * decision_tree ref 
    | Inrange of range * field * (decision_tree ref) * (decision_tree ref)
    | Inrelation of relation * field list * decision_tree ref * decision_tree ref
    (* 	| ForwardAccordingTo of relation * int list * field list * int *)

type policy = decision_tree

(* 
 "Leaf pkt fd" forwards the current packet as denoted by forwarding decision fd. pkt is the most general (symbolic) packet that can reach this node
 "Add pkt fl rel dt" adds the tuple of field values corresponding to the fields in fl to the relation rel before procceding with dt. pkt is the most general (symbolic) packet that can reach this node
 "Remove pkt fl rel dt" removes the tuple of field values corresponding to the fields in fl to the relation rel before procceding with dt. pkt is the most general (symbolic) packet that can reach this node
 "Inrange r f dtrue dfalse" proceeds with dtrue if field f of the current packet is in range r and with dfalse otherwise.

*)


	  
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
  if lo = hi then
    string_of_int lo
  else
	"[" ^ (string_of_int lo) ^ ", " ^ (string_of_int hi)^ "]"

let string_of_range (r : range) = 
	List.fold_left (fun acc x -> (if acc = "" then "" else acc^"U")^(string_of_interval x)) "" r 


let string_of_rel (rel, fields) =
  rel^" : ["^(String.concat "," (List.map string_of_field fields))^"]"
  

let string_of_packet' (pkt': packet') = 
	let aux f r acc = 
		let sep = (if acc = "" then "" else " || "^acc) in 
		(string_of_field f) ^ " :" ^ (string_of_range r) ^ sep
	in 
	"<<" ^(FM.fold aux pkt' ""  )^ ">>"
  
let string_of_packet (pkt : packet) =
	let (fs,rel_tru,rel_fal) = !pkt in 
	let pstr = (string_of_packet' fs) in
	  pstr^"in ("^(String.concat "," (List.map string_of_rel rel_tru))^") and not in ("^(String.concat "," (List.map string_of_rel rel_fal))^")"
    
let rec string_of_fd (fd : forwarding_decision) =
  match fd with
  | Deliver -> "Deliver"
  | Drop -> "Drop"
  | ForwardTo i -> "Forward to "^ (string_of_int i)
  | Multicast (fd1, fd2) -> "Multicasted: " ^  (string_of_fd fd1) ^ " and " ^ (string_of_fd fd2)
  | Ctrl -> "Send to controller"

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
		| Add (pkt, fields, rel, dt) ->
		    print_endline (str^ "Adding ("^(String.concat "," (List.map string_of_field fields))^") to "^rel);
		    aux !dt (level+1)
		| Remove (pkt, fields, rel, dt) ->
		    print_endline (str^ "Removing ("^(String.concat "," (List.map string_of_field fields))^") to "^rel);
		    aux !dt (level+1)
		| Inrelation(rel, fields, tru, fal) ->
		    print_endline (str ^ "Inrel: fields (" ^(String.concat "," (List.map string_of_field fields))^ ") in " ^rel);
		    aux !tru (level + 1);
		    aux !fal (level + 1)
	in 
	aux dt 0

let rec decisions (p: policy) : (packet*forwarding_decision) list = 
	match p with 
	| Dummy -> []
	| Leaf (pkt,fd) -> [(pkt,fd)]
	| Inrange (_,_,tru,fal) -> (decisions !tru) @ (decisions !fal)
	| Inrelation (_, _, tru, fal) -> (decisions !tru) @ (decisions !fal)
	| Add (pkt, _, _, _) -> [(pkt, Ctrl)]
	| Remove (pkt, _, _, _) -> [(pkt, Ctrl)]

let string_of_policy (p: policy) : string = 
	let str = List.fold_left 
		(fun acc (pkt,fd) -> acc ^ "\n" ^ (string_of_packet pkt) ^ " --> " ^ (string_of_fd fd)) 
		"" (decisions p) in 
	str ^ "\n"
	

let rec string_of_rules (rs:(packet' * forwarding_decision) list) =
  match rs with
  | [] -> ""
  | (p, fd)::rs' ->
      let str = string_of_rules rs' in
      " | "^(string_of_packet' p)^" ---> "^(string_of_fd fd)^"  \n"^str

(******************************************************
 *
 *  Decision Tree
 *
 ******************************************************)

(* Global mutable decision tree *)
let root = ref Dummy
let loc = ref root
let next_pkts = ref (Stack.create ())


let compile (f: packet -> forwarding_decision) : policy = 
	root := Dummy;
	loc := root;
	next_pkts := (Stack.create ());
	let sym_pkt = ref (FM.empty, [], []) in 
	Stack.push sym_pkt !next_pkts;
	while not (Stack.is_empty !next_pkts) do
	   let cur_pkt = Stack.pop !next_pkts in
	   if debug then print_endline ("current packet is: "^ (string_of_packet cur_pkt)) else ();
		let fd = f cur_pkt in 
		(!loc) := Leaf (cur_pkt, fd);
		loc := root;
		if debug then print_dtree !root else (); 
	done; !root

let max_value = 1000
let min_value = 0
let total_range = [(min_value,max_value)]


(* returns the intersection between two intervals, if any *)   
let intersection'' ((lo,hi):int*int) (lo',hi') : range = 
	assert (lo <= hi);
	assert (lo' <= hi');
	if  hi < lo' || hi' < lo
	then []
	else [(max lo lo', min hi hi')]

(* lists the intersection between r and each of the intervals in rs *)
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

(* after normalization, a range is a list of  disjoint, increasing intervals *)
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

(* 
 Implementation: While building the decision tree using symbolic packets,
       If "in_range p f r" is called by the traced function while at an unexplored location, we add an Inrange node to the decision tree, refines the fielf f of symbolic packet p with range r and explore the true branch of the decision tree. We also add a packet with field f refined with the complement of range r to subsequently explore the false branch of the tree. 
       If "in_range p f r" is called while tracing back on an existing Inrange node for field f and range r, we explore the true branch if the tested range r intersects with the p's range for field f, and explore the false branch otherwise.
*)
let in_range (p: packet) (f: field) (r: range) : bool = 
	let (pkt, rel_tru, rel_fal) = !p in 

	let aux x y  = 
		match !(!loc) with 
		| Dummy -> 
			let ptru = (FM.add f x pkt, rel_tru, rel_fal) in 
			let pfal = (FM.add f y pkt, rel_tru, rel_fal) in 
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
	let r' = try FM.find f pkt with _ -> total_range in
	if debug then 
		let field_str = "in in_range for field "^(string_of_field f) in 
		let inter_str = " with inter:"^(string_of_range (intersection r r')) in 
		print_endline (field_str ^ inter_str)
	else ();
	aux (normalize_range (intersection r r')) (normalize_range (intersection (complement r) r')) 
		



(* Same as add but also creates alternative routes without add node *)
let rec add' (p: packet) (fields: field list) (rel: relation) : unit =
  let (pkt, rel_tru, rel_fal) = !p in
  let already_decided = (List.mem (rel, fields) rel_tru) || (List.mem(rel, fields) rel_fal) in
	match !(!loc) with 
	| Dummy ->
	    if already_decided then
	      add p fields rel else
	    let child = ref Dummy in
	    p := (pkt, rel_tru, (rel, fields)::rel_fal);
	    Stack.push (ref (pkt, (rel, fields)::rel_tru, rel_fal)) !next_pkts;
	    (!loc) := Inrelation(rel, fields, ref Dummy ,ref (Add (p, fields, rel, child)));
	    loc := child
	| Add (p',fields', rel', dt) -> 
		if fields' = fields && rel' = rel then 
			loc := dt
		else failwith "Error [add': different values]"
	| Inrelation(rel', fields', tru, fal) ->
	    if fields' = fields && rel' = rel then 
	      (match List.mem rel (List.map fst rel_tru), List.mem rel (List.map fst rel_fal) with 
	      | true, _ ->
			  (match (!fal) with
			  | Add ( p', fields', rel', dt') ->
			      if fields' = fields && rel' = rel then 
			        loc := tru
			      else
				failwith "Error [add': add node expected with same rel and fields after shadow-inrelation]"
			  | _ -> failwith "Error [add': add node expected after shadow-inrelation]")
	      | false, _ -> loc := fal
(* 	      | _ -> failwith "Error [add': false and true]" *) )
	| _ -> failwith "Error [add': unhandled case]"	  
and add (p: packet) (fields: field list) (rel: relation) : unit =
        if o1 then add' p fields rel else 
	match !(!loc) with 
	| Dummy -> 
		let child = ref Dummy in 
		(!loc) := Add (p, fields, rel, child);
		loc := child
	| Add (p',fields', rel', dt) -> 
		if fields' = fields && rel' = rel then 
			loc := dt
		else failwith "Error [add: different values]"
	| _ -> failwith "Error [add: unhandled case]"


	      

let rec remove' (p: packet) (fields: field list) (rel: relation) : unit =
  let (pkt, rel_tru, rel_fal) = !p in
  let already_decided = (List.mem (rel, fields) rel_tru) || (List.mem(rel, fields) rel_fal) in
  match !(!loc) with 
  | Dummy ->
      if already_decided then
	remove p fields rel
      else
	    let child = ref Dummy in
	    p := (pkt, (rel, fields)::rel_tru, rel_fal);
	    Stack.push (ref (pkt, rel_tru, (rel, fields)::rel_fal)) !next_pkts;
	    (!loc) := Inrelation(rel, fields, ref (Remove (p, fields, rel, child)), ref Dummy);
	    loc := child
	| Remove (p',fields', rel', dt) -> 
		if fields' = fields && rel' = rel then 
			loc := dt
		else failwith "Error [remove': different values]"
	| Inrelation(rel', fields', tru, fal) ->
	    if fields' = fields && rel' = rel then 
	      (match List.mem rel (List.map fst rel_tru), List.mem rel (List.map fst rel_fal) with 
	      | true, _ ->
		  (match (!tru) with
		  | Remove ( p', fields', rel', dt') ->
		      if fields' = fields && rel' = rel then 
			loc := fal
		      else
			failwith "Error [add': add node expected with same rel and fields after shadow-inrelation]"
		  | _ -> failwith "Error [remove': add node expected after shadow-inrelation]"
		   )
	      | false, _ -> loc := fal
(*	      | _ -> failwith "Error [remove': false and true]" *) )
	| _ -> failwith "Error [remove': unhandled case]"	      
and remove (p: packet) (fields: field list) (rel: relation) : unit =
        if o1 then remove' p fields rel else
	match !(!loc) with 
	| Dummy -> 
		let child = ref Dummy in 
		(!loc) := Remove (p, fields, rel, child);
		loc := child
	| Remove (p',fields', rel', dt) -> 
		if fields' = fields && rel' = rel then 
			loc := dt
		else failwith "Error [remove: different values]"
	| _ -> failwith "Error [remove: unhandled case]"

	      
	      
	      
let in_relation (p: packet) (fields: field list) (rel: relation) : bool = 
  let (pkt, rel_tru, rel_fal) = !p in
      
	match !(!loc) with 
	| Dummy ->
		Stack.push (ref (pkt, rel_tru, (rel, fields)::rel_fal)) !next_pkts;
		p := (pkt, (rel, fields)::rel_tru, rel_fal);
		let ctru,cfal = ref Dummy, ref Dummy in 
		(!loc) := Inrelation (rel, fields, ctru, cfal);
		loc := ctru;
		true
	| Inrelation (rel', fields', tru, fal) ->
		if fields' = fields && rel' = rel then 
			(match List.mem rel (List.map fst rel_tru), List.mem rel (List.map fst rel_fal) with 
			 | true, _ -> loc := tru; true 
			 | false, _ -> loc := fal; false 
			(* | _ -> failwith "Error [inrelation: false and true]" *) )
		else failwith "Error [inrelation: different values]"
	| _ -> failwith "Error [inrelation: unhandled case]"


(******************************************************
 *
 *  Runtime system
 *
 ******************************************************)


module SM = Map.Make(String)	     

type rule = packet' * forwarding_decision
type sym_rule = packet * forwarding_decision
type rules = rule list
type sym_rules = sym_rule list

type env = ((int list) list) SM.t 


let environment = ref (SM.empty)



let matches (p: packet') (pattern: packet') : bool = 
	let aux k v acc = 
		acc &&		
		(try 
			let v' = FM.find k p in (intersection v v') = v'
		with _ -> true)
	in
	FM.fold aux pattern true

let rec evaluate_rules (p: packet') (rules: rules) : forwarding_decision =
	match rules with 
	| [] -> Ctrl
	| (p',fd)::rs -> 
		if matches p p' then fd 
		else evaluate_rules p rs

let rec evaluate_pol (p: packet') (pol:policy) : forwarding_decision =
	let aux fields rel = 
		let vals = List.map (fun fld -> try FM.find fld p with _ -> failwith "Error [evaluate_pol aux: Field not in Relation]") fields in 
   		let vals' = List.map (fun x -> fst (List.hd x)) vals in 
   		let tups = try SM.find rel !environment with _ -> [] in 
   		(vals',tups)
	in 
    match pol with
    | Leaf(p', fd) -> fd
    | Inrange(r, f, tru, fal) ->
		let r' = try FM.find f p with _ -> failwith "Error [evaluate_pol: Inrange, uninitialized field]" in
		(match intersection r' r with 
		| [] -> evaluate_pol p (!fal)
		|  _ -> evaluate_pol p (!tru))  	
   | Add(_, fields, rel, dt') -> 
   		let vals',tups = aux fields rel in
   		environment := SM.add rel (vals'::tups) !environment;
   		evaluate_pol p (!dt')
   | Remove(_, fields, rel, dt') ->
   		let vals',tups = aux fields rel in
   		environment := SM.add rel (List.filter (fun t -> not (t=vals')) tups) !environment;
   		evaluate_pol p (!dt')
   | Inrelation(rel, fields, tru, fal) ->
      	let vals',tups = aux fields rel in
   		if List.mem vals' tups 
   		then evaluate_pol p !tru
   		else evaluate_pol p !fal
   | Dummy -> failwith "Error [evaluate_pol: Reached an unexplored part of the policy]"



	 
let rec build_rules (srs: sym_rules): rules =
  let rec aux p fd flds fi =
    (match flds, fi with
    | (f::flds'), (i::fi') ->
	let r = try FM.find f p with _ -> total_range in
	(match (normalize_range (intersection [(i,i)] r)) with
	| [] -> []
	| r' -> let p' = FM.add f r' p in
	  aux p' fd flds' fi')
    | [], [] -> [(p, fd)]
    | _,_ -> failwith "Error [build_rules aux: mismatched field and projection number]" ) in
  let refine_packet p (rel, flds) fd =
	try 
	  let crel = SM.find rel !environment in
	  List.fold_left (fun acc fi -> (aux p fd flds fi)@acc)  [] crel 
    with _ ->  [] (* no rule, relation is empty *)
  in
  match srs with
  | (p, fd)::srs' ->
      let rs' = build_rules srs' in
      let (p', trel, frel) = !p in
      (* for each of the relation in trel *)
               (* for each of the tuple in the relation *)
              (* make a rule intersecting the packet p' with that tuple  (may result in no rule if empty) *)
     ( match trel with
      | [] -> (p', fd)::rs'
      | _ -> List.append (List.fold_left (fun acc rh -> List.append (refine_packet p' rh fd) acc) [] trel) rs')
  | [] -> []

let rec simulate (p:packet') (r:rules) (pol:policy) : rules * forwarding_decision =
	print_endline ("Packet: " ^ (string_of_packet' p));
	match evaluate_rules p r with 
	| Ctrl -> let fd = evaluate_pol p pol in
	          let rules' = build_rules (decisions pol) in
	          print_endline "Sent to controller";
		  	  print_endline (string_of_rules rules');
	          (rules', fd)
	| fd -> print_endline "Matched at switch"; (r,fd)

let run (pol: policy) (inputs: ((field*int) list) list) : unit = 
	let srules = decisions pol in 
	let rules = ref (build_rules srules) in
	print_endline (string_of_policy pol);
	let rec aux pkts =
		match pkts with 
		| [] -> ()
		| p::ps ->
		    (print_endline " -------- processing packet ----------");
			let (rules',fd) = simulate p !rules pol in 
			rules := rules';
			print_endline ("forwarded according to " ^ (string_of_fd fd) ^ "\n");
			aux ps
	in 
	let rec to_packet' fvalues = 
		match fvalues with 
		| [] -> FM.empty
		| (fld,v)::vs -> FM.add fld [(v,v)] (to_packet' vs)
	in
	let pkts = List.map to_packet' inputs in
	let str = string_of_rules !rules in
	print_endline ("Starting to run controller with switch rules : \n"^str);
	aux pkts







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
	run_tests tests *) ()





