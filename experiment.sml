(* ANVAND INTE I RIKTIG KOD*)
use "Matrix.sml";
use "GameEngine";

structure Experiment =
struct

(*Prints an option as [] and NONE as " "*)
fun tetrisPrint x =
    case x of 
	SOME(_) => print "[]"
      | NONE => print "  "	

(*uses tetrisPrint on all elements in m*)
fun printMatrix m = 
    let
	val rows = Matrix.nRows m
        val cols = Matrix.nCols m
		   
	fun printMatrix' (mat, n1, n2) = 
	    if n2 >= cols then () 
	    else if n1 >= rows then (print "|\n"; printMatrix'(mat, 0, n2+1))
	    else (tetrisPrint (Matrix.getElement (mat, n1, n2)); printMatrix'(mat, n1+1, n2))
    in
	printMatrix'(m, 0, 0)
    end

open GameEngine;
fun printgs (gs(m,(at,(x,y),af),nt)) = 
	let
		val cols = nCols(m)
		val rows = nRows(m)
		fun printrad i = Vector.appi (fn (j, elem) => tetrisPrint(elem)) (getRow (m, i) )
	in
		List.tabulate (n,  printrad)
	end

end
