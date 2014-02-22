structure GameEngine = 
struct
	datatype gameCommand = MoveL | MoveR | MoveD | RotateL | RotateR
	datatype tetromino_type = tetromino_O | tetromino_I | tetromino_S | tetromino_Z | tetromino_L | tetromino_J | tetromino_T
	datatype orientation = north | east | south | west
	type position = int * int
	datatype gamestate = gs of Matrix.matrix * tetromino_type * pos * orientation * tetromino_type

	fun doCommand (g : gamestate, MoveL) = g
		| doCommand (g : gamestate, MoveR) = g
		| doCommand (g : gamestate, MoveD) = g
		| doCommand (g : gamestate, RotateL) = g
		| doCommand (g : gamestate, RotateR) = g
end
