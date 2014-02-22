use "GameCommand";
use "GameEngine";
signature BOT = 
sig
	val getGameCommands : GameEngine.gamestate -> GameCommand.gameCommand list
end

