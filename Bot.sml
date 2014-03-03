
signature BOT = 
sig
	val getGameCommands : GameEngine.gamestate -> GameEngine.gameCommand list
end

structure SmartBot :> BOT = 
struct
	open GameEngine
	fun getGameCommands (g) =  (SoftDrop::SoftDrop::SoftDrop::SoftDrop::LeftShift::SoftDrop::LeftShift::SoftDrop::RotateCW::SoftDrop::LeftShift::SoftDrop::RotateCW::SoftDrop::RightShift::SoftDrop::RotateCCW::SoftDrop::SoftDrop::SoftDrop::RotateCW::SoftDrop::RightShift::SoftDrop::RotateCCW::SoftDrop::RotateCW::SoftDrop::RotateCW::SoftDrop::RotateCCW::SoftDrop::RotateCCW::SoftDrop::RightShift::SoftDrop::RotateCW::SoftDrop::RightShift::SoftDrop::SoftDrop::SoftDrop::RightShift::SoftDrop::SoftDrop::SoftDrop::nil)
end

structure FakeBot :> BOT = 
struct
	open GameEngine

	fun 	userInputMap #"a" = SOME LeftShift
		|	userInputMap #"d" = SOME RightShift
		|	userInputMap #"w" = SOME HardDrop 
		|	userInputMap #"s" = SOME SoftDrop
		|	userInputMap #"q" = SOME RotateCCW
		|	userInputMap #"e" = SOME RotateCW
		| 	userInputMap _ 	  = NONE
  	
  	fun userInput () = 
  	let
  		val i = valOf (TextIO.input1 TextIO.stdIn)
  	in
  		if  isSome (userInputMap i) then  valOf (userInputMap i) else userInput ()
  	end

  	fun getGameCommands (g) = [userInput () ];
  			
end