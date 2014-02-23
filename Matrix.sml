(*
Tanken med Matrix är att det ska fungera ungefär som immutable Array2 för int. 
*)
(* 
Raderna i och kolonnerna j räknas upp från 0.
*)

signature MATRIX = 
sig
	type matrix
	val getElement	: matrix * int * int -> int
	val setElement	: matrix * int * int * int -> matrix
end

structure Matrix :> MATRIX = 
struct
	type matrix = int vector vector
	
	(* getElement (m, i, j)
	TYPE: matrix * int * int -> int
	PRE:
	POST: the element in matrix m on the i:th row and the j:th column
	EXAMPLE: 
	getElement(#[#[0, 0, 1], #[0, 0, 0]], 0, 2) = 1 !ELLER!
	getElement(Vector.fromList [Vector.fromList[0,0,1],Vector.fromList[0,0,0]], 0, 2) = 1
	*)
	fun getElement (m, i, j) = Vector.sub(Vector.sub(m, i), j)

	(* setElement (m, i, j, v)
	TYPE: matrix * int * int * int -> matrix
	PRE:
	POST: replaces the element in matrix m on the i:th row and the j:th column with v
	EXAMPLE: 
	setElement(#[#[0, 0, 1], #[0, 0, 0]], 0, 2, 3) = #[#[0, 0, 3], #[0, 0, 0]] !ELLER!
	setElement(Vector.fromList[[0,0,1],Vector.fromList[0,0,0]], 0, 2, 3) = fromList[fromList[0, 0, 3], fromList[0, 0, 0]]
	*)
	fun setElement (m, i, j, v) = Vector.update(m, i, Vector.update(Vector.sub(m, i), j, v))

end


