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
 	val getRow : 'a matrix * int -> 'a vector
	val setRow : 'a matrix * int * 'a vector -> 'a matrix
end

structure Matrix :> MATRIX = 
struct
	type 'a matrix = 'a vector vector
	
	(* getElement (m, i, j)
	TYPE: 'a matrix * int * int -> 'a
	PRE: 0 <= i < columns of m and 0 <= j < rows of m
	POST: the element in matrix m on the i:th row and the j:th column
	EXAMPLE: getElement(setElement(createMatrix(3,3,0), 0, 2, 3), 0, 2) = 3
	*)
	fun getElement (m, i, j) = Vector.sub(Vector.sub(m, i), j)

	(* setElement (m, i, j, v)
	TYPE: 'a matrix * int * int * 'a -> 'a matrix
	PRE: 0 <= i < columns of m and 0 <= j < rows of m
	POST: replaces the element in matrix m on the i:th row and the j:th column with v
	EXAMPLE: setElement(createMatrix(3,3,0), 0, 2, 3) = 
[0  0  3]
[0  0  0]
[0  0  0]
	*)
	fun setElement (m, i, j, v) = Vector.update(m, i, Vector.update(Vector.sub(m, i), j, v))

	(* nCols m
	TYPE: 'a matrix -> int
	PRE: none
	POST: number of columns in matrix m
	EXAMPLE: nCols(Vector.fromList[Vector.fromList[0,0,1],Vector.fromList[0,0,0]]) = 3
	*)
	fun nCols m = Vector.length(Vector.sub (m, 0))

	(* nRows m
	TYPE: 'a matrix -> int
	PRE: none
	POST: number of rows in matrix m
	EXAMPLE: nRows(createMatrix (1,2,3)) = 1
	*)
	fun nRows m = Vector.length m
	
	(* createMatrix (i, j, init)
	TYPE: int * int * 'a -> 'a matrix
	PRE: 0 < i, j
	POST: matrix that have i rows and j columns with init as all elements
	EXAMPLE: createMatrix (2, 3, 0) = 
[0   0   0]
[0   0   0]
	*)
	fun createMatrix (i, j, init) = Vector.tabulate(i, fn x => Vector.tabulate(j, fn x => init))
	
	(* getRow (m, i)
	TYPE: 'a matrix * int -> 'a vector
	PRE: 0 <= i < columns of m
	POST: row i in matrix m
	EXAMPLE: getRow(createMatrix (2, 3, 0) , 1) = Vector.fromList[0, 0, 0]
	*)
	fun getRow (m : 'a matrix, i) = Vector.sub(m, i)

	(* setRow (m, i, v)
	TYPE: 'a matrix * int * 'a vector -> 'a matrix
	PRE:  0 <= i < columns of m
	POST: row i in matrix m replaced by element v
	EXAMPLE: setRow(createMatrix (2, 3, 0), 1, Vector.fromList[1,1,1]) =
[0  0  0]
[1  1  1]
	*)
	fun setRow (m : 'a matrix, i, v) = Vector.update(m,i,v)
end


