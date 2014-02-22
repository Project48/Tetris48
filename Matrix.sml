(*
Tacken med Matrix är att fungera ungifär som inmutable Array2 för int. 
*)
signature MATRIX = 
sig
	type matrix
	val getElemant	: matrix * int * int -> int
	val setElement	: matrix * int * int * int -> matrix
end

structure Matrix :> MATRIX = 
struct
	type matrix = int vector vector
	fun getElemant (m, i, j) = i
	fun setElement (m, i, j, v) = m
end


