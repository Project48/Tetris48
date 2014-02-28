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
	val unableDelay = 1

	open GameEngine
	open Matrix
	open Experiment

	fun newGame (r,c) = gs((createMatrix (r,c, NONE : block option)),
	 ( Tetromino_T, ((c-1) div 2,0) : position ,  North),  
	Tetromino_I )

	(*random*)
	fun nextRand (a,b) = (b,((a+1*b+1)mod (101)))
	val tetrominos = Vector.fromList [Tetromino_T,Tetromino_I,Tetromino_O,Tetromino_S,Tetromino_Z,Tetromino_L,Tetromino_J]
	fun setNextType (gs(m,(at,(x,y),af),nt), nynexttype) = gs(m,(at,(x,y),af),nynexttype)

	structure DemoBot = SmartBot

	fun loop g (a,b) [] = (printGS g ;loop g (a,b) (DemoBot.getGameCommands(g)))
	|	loop (g) (a,b) (com::coms) = 
	if  (com = HardDrop orelse com = SoftDrop) then 
		 if ( isSome(doCommand (g, com)))  then (printGS g;
		  delay comandDelay;
		  loop (setNextType ( valOf(doCommand(g, com)), Vector.sub(tetrominos, b mod 7))) (nextRand(a,b)) coms) 
		else (printGS g; println "GameOver")
	else if isSome(doCommand (g, com)) then (printGS g; delay comandDelay; loop (valOf (doCommand (g, com))) (a,b) coms)
		 else (println "unable to do command!"; delay (unableDelay*0.25); println "fake gravity applied..."; delay (unableDelay*0.75);  loop g (a,b) (SoftDrop::nil) )

	fun startDemo () = loop (newGame(20,10)) (153,156) [];

end
