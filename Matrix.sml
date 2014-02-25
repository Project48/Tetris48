(*
Tanken med Matrix är att det ska fungera ungefär som immutable Array2. 
*)
(* 
Raderna i och kolonnerna j räknas upp från 0.
*)

signature MATRIX = 
sig
	type 'a matrix
	val getElement	: 'a matrix * int * int -> 'a
	val setElement	: 'a matrix * int * int * 'a -> 'a matrix
	val nCols : 'a matrix -> int
 	val nRows : 'a matrix -> int
 	val createMatrix : int * int * 'a -> 'a matrix
end

structure Matrix :> MATRIX = 
struct
	type 'a matrix = 'a vector vector
	
	(* getElement (m, i, j)
	TYPE: 'a matrix * int * int -> 'a
	PRE:
	POST: the element in matrix m on the i:th row and the j:th column
	EXAMPLE: 
	getElement(#[#[0, 0, 1], #[0, 0, 0]], 0, 2) = 1 !ELLER!
	getElement(Vector.fromList [Vector.fromList[0,0,1],Vector.fromList[0,0,0]], 0, 2) = 1
	*)
	fun getElement (m, i, j) = Vector.sub(Vector.sub(m, i), j)

	(* setElement (m, i, j, v)
	TYPE: 'a matrix * int * int * 'a -> 'a matrix
	PRE:
	POST: replaces the element in matrix m on the i:th row and the j:th column with v
	EXAMPLE: 
	setElement(#[#[0, 0, 1], #[0, 0, 0]], 0, 2, 3) = #[#[0, 0, 3], #[0, 0, 0]] !ELLER!
	setElement(Vector.fromList[Vector.fromList[0,0,1],Vector.fromList[0,0,0]], 0, 2, 3) = fromList[fromList[0, 0, 3], fromList[0, 0, 0]]
	*)
	fun setElement (m, i, j, v) = Vector.update(m, i, Vector.update(Vector.sub(m, i), j, v))

	(* nCols m
	TYPE: 'a matrix -> int
	PRE:
	POST: number of columns in matrix m
	EXAMPLE: nCols(Vector.fromList[Vector.fromList[0,0,1],Vector.fromList[0,0,0]]) = 3
	*)
	fun nCols m = Vector.length(Vector.sub (m, 0))

	(* nRows m
	TYPE: 'a matrix -> int
	PRE: number of rows in matrix m
	POST:
	EXAMPLE: nRows(Vector.fromList[Vector.fromList[0,0,1],Vector.fromList[0,0,0]]) = 2
	*)
	fun nRows m = Vector.length m
	
	(* createMatrix (i, j, init)
	TYPE: int * int * 'a -> 'a matrix
	PRE:
	POST: matrix that have i rows and j columns with init as all elements
	EXAMPLE: createMatrix (2, 3, 0) = fromList[fromList[0, 0, 0], fromList[0, 0, 0]]
	*)
	fun createMatrix (i, j, init) = Vector.tabulate(i, fn x => Vector.tabulate(j, fn x => init))
end


