(* ANVAND INTE I RIKTIG KOD*)
use "Matrix.sml";
use "GameEngine";
use "Miscellaneous";
open Miscellaneous;

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

fun printGS' (gs(m,(at,(x,y),af),nt,cr), i, j) = 
	let
		val blocks = List.map (fn (dx, dy) => (dy+y, dx+x)) (createBlocks at af)

	in
		case (List.find (fn (bi,bj) => bi=i andalso bj=j) blocks)  of
		 	SOME(_) => if isSome 	(getElement (m, i, j)) 	then "~~" 	else "{}"
		 	| NONE  => if isSome	(getElement (m, i, j)) 	then "[]" 	else "  "  
	end

fun printGS (state as gs(m,(at,(x,y),af),nt,cr)) = 
	let
		val cols = nCols(m)
		val rows = nRows(m)
		fun printrad i = (
			print ("rad:");
			printInt i;
			print "\t";
			print "|"; 
			Vector.appi 
			    (fn (j, elem) => (print
						  ( printGS' 
							(state, i,j)
						  )
					     )
			    )
			    (getRow (m, i));
			println("|")
			)
	in
		(
		  List.tabulate (rows,  printrad);
		  println "--------+";
		  print "NEXT: " ;
		  print ((fn Tetromino_T => "T" | Tetromino_I => "I" | Tetromino_O => "O" | Tetromino_S => "S" | Tetromino_Z => "Z"| Tetromino_L => "L" | Tetromino_J => "J"  ) nt );
		  print " |score:\t";
		  printInt cr;
		  println "";
		  println "--------+"
		)
	end


end
