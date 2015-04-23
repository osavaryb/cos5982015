
type field = IpSrc | IpDst 
type range = (int*int) list
type relation = string
type forwarding_decision =
	| Deliver
	| Drop
	| ForwardTo of int
	| Multicast of forwarding_decision * forwarding_decision

type packet 
type policy


val min_value: int 
val max_value: int
val string_of_policy: policy -> string
val compile: (packet -> forwarding_decision) -> policy
val in_range: packet -> field -> range -> bool