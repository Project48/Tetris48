use "GameCommand";
signature BOT = 
sig
	val getGameCommands : GameState -> GameCommand.gameCommand list
end

