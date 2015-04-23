
type field = IpSrc | IpDst 
type range = (int*int) list
type relation = string
type forwarding_decision =
	| Deliver
	| Drop
	| ForwardTo of int
	| Multicast of forwarding_decision * forwarding_decision
	| Ctrl

type packet 
type policy


val min_value: int 
val max_value: int
val string_of_policy: policy -> string
val compile: (packet -> forwarding_decision) -> policy
(* in_range p f r returns true if the value of field f of packet p is in range r, and false otherwise *)
val in_range: packet -> field -> range -> bool
val add: packet -> field list -> relation -> unit
val remove: packet -> field list -> relation -> unit
val in_relation: packet -> field list -> relation -> bool


