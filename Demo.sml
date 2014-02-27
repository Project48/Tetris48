(*
	use "Matrix";
	use "experiment";
	use "Bot";
	use "GameEngine";	
*)

(*Demo.sml*)
structure Demo = 
struct

	open GameEngine
	open Matrix
	open Experiment

	fun nextRand (a,b) = (b,((a+1*b+1)mod (101)))
	val tetrominos = Vector.fromList [Tetromino_T,Tetromino_I,Tetromino_O,Tetromino_S,Tetromino_Z,Tetromino_L,Tetromino_J]

	fun getGameCommands (g) =  (SoftDrop::SoftDrop::SoftDrop::SoftDrop::LeftShift::SoftDrop::LeftShift::SoftDrop::RotateCW::SoftDrop::LeftShift::SoftDrop::RotateCW::SoftDrop::RightShift::SoftDrop::RotateCCW::SoftDrop::SoftDrop::SoftDrop::RotateCW::SoftDrop::RightShift::SoftDrop::RotateCCW::SoftDrop::RotateCW::SoftDrop::RotateCW::SoftDrop::RotateCCW::SoftDrop::RotateCCW::SoftDrop::RightShift::SoftDrop::RotateCW::SoftDrop::RightShift::SoftDrop::SoftDrop::SoftDrop::RightShift::SoftDrop::SoftDrop::SoftDrop::nil)
	
	
	fun loop g (a,b) [] = 
		(loop g (a,b) (getGameCommands(g)))
	|	loop (g as gs(m,(at,(x,y),af),nt)) (a,b) (com::coms) = if ( (com = HardDrop orelse com = SoftDrop) andalso not ( isSome(doCommand (g, com))) ) then (printGS g; println "GameOver" )
	else if isSome(doCommand (g, com)) 
		then 	
		(printGS g; delay 0.5; loop (valOf (doCommand (g, com) ) ) (a,b) coms)
	else (println "unable to do command!"; delay 3.0; println "fake gravity applied..."; delay 1.0;  loop (gs(m,(at,(x,y),af),Vector.sub(tetrominos, b mod 7) )) (nextRand(a,b)) (SoftDrop::(com::coms)) )


end
