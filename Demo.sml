(*
	use "Matrix";
	use "experiment";
	use "Bot";
	use "GameEngine";	
*)
(*use "Bot";*)


(*Demo.sml*)
structure Demo = 
struct
	val comandDelay = 0.5
	val unableDelay = 1.0
	structure DemoBot = FakeBot

	open GameEngine
	open Matrix
	open Miscellaneous

	fun printGS' (gs(m,(at,(x,y),af),nt,cr), i, j) = 
	let
		val blocks = List.map (fn (dx, dy) => (dy+y, dx+x)) (createBlocks at af)

	in
		case (List.find (fn (bi,bj) => bi=i andalso bj=j) blocks)  of
		 	SOME(_) => if isSome 	(getElement (m, i, j)) 	then "><" 	else "{}"
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
	

	fun loop g [] = 
		let
			val _ = printGS g 
			val coms = DemoBot.getGameCommands(g)
			val _ = if coms <> nil then () else (println "not command!"; delay (unableDelay*0.25); println "fake gravity applied..."; delay (unableDelay*0.75);  loop g (SoftDrop::nil)) 
		in
			loop g coms
		end
		
	|	loop (g) (com::coms) = 
	 case doCommand (g, com) of
	 	SOME(x) => (printGS x; delay comandDelay; loop (x) coms)
	 	| NONE => 
	 		if (com = HardDrop orelse com = SoftDrop) 
	 		then (printGS g; println "GameOver") 
	 		else (
	 			println "unable to do command!";
	 			 delay (unableDelay*0.25);
	 			  println "fake gravity applied...";
	 			   delay (unableDelay*0.75); 
	 			    loop g (SoftDrop::nil) 
	 			    )


	fun newGame (r,c) = loop (gs((createMatrix (r,c, NONE : block option)),
	 ( Tetromino_T, ((c-1) div 2,0) : position ,  North),  
	Tetromino_I, 0 )) []

end
