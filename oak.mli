

(* a standard packet field *)
type field = IpSrc | IpDst 
(* range is a union of intervals *)
type range = (int*int) list
(* name of a relation *)
type relation = string
(* forwarding decision can deliver, drop, forward, multicast *)
type forwarding_decision =
	| Deliver
	| Drop
	| ForwardTo of int
	| Multicast of forwarding_decision * forwarding_decision
(* abstract packet type *)
type packet 
(* abstract policy type *)
type policy

(* minimum value for a range interval *)
val min_value: int 
(* maximum value for a range interval *)
val max_value: int
(* compile a user-function into a policy *)
val compile: (packet -> forwarding_decision) -> policy
(* show the forwarding decisions for a policy *)
val string_of_policy: policy -> string
(* in_range p f r returns true if the value of field f of packet p is in range r, and false otherwise *)
val in_range: packet -> field -> range -> bool
(* add packet's fields to a relation *)
val add: packet -> field list -> relation -> unit
(* remove packet's fields from a relation *)
val remove: packet -> field list -> relation -> unit
(* test if a packet's fields are in a relation *)
val in_relation: packet -> field list -> relation -> bool


