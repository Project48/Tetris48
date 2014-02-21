signature MATRIX = 
sig
	type matrix
	val getElemant	: matrix * int * int -> int
	val setElement	: matrix * int * int * int -> matrix
end
(*
structure Matrix :> MATRIX = 
struct
	type matrix = something
	fun getElemant (m, i, j) = something
	fun setElement (m, i, j, v) = something
end
*)

