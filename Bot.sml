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

structure FooBot :> BOT =
struct

open Matrix
open GameEngine

(* n elements of c added to list l*)
fun moveRepetition (c, 0, l) = l
  | moveRepetition (c, n, l) = moveRepetition (c, n - 1, c::l)

(*converts a gamestate into an int based on block positions*)
fun gameValue (g as gs(m,(at,(x,y),af),nt, score)) =
    let
	val nRows = nRows m
	val nCols = nCols m

	fun gameValue' (r, c, n) =
	    if r >= nRows then n 
	    else if c >= nCols then gameValue'(r+1, 0, n)
	    else if isSome (Matrix.getElement(m, r, c)) then gameValue'(r, c+1, n + (nRows - (r + 1))) 
	    else gameValue'(r, c+1, n)
 
       fun holesAmount' (r, c, n) =
	   if r >= nRows then n 
	   else if isSome (Matrix.getElement(m, r, c)) then holesAmount' (r + 1, c, n)
	   else holesAmount' (r + 1, c, n + 1)								 

      (* amount of holes in the gamestate *)
       fun holesAmount (r, c, n) = 
	   if c >= nCols then n
	   else if r >= nRows then holesAmount(0, c + 1, n)
	   else if isSome (Matrix.getElement(m, r, c)) then holesAmount(0, c + 1, holesAmount'(r, c, n))
	   else holesAmount (r + 1, c, n)

      (* amount of cleared rows *)
       fun clearedRows (r, n) =
	   if r >= nRows then n 
	   else if not (Vector.exists (not o isSome) (Matrix.getRow (m, r))) then clearedRows(r + 1, n + 1)
	   else clearedRows(r + 1, n)

    in
	gameValue'(0, 0, 0) + (4 * holesAmount(0, 0, 0)) + (~5 * score)
    end 

(*uses hardDrop on g and if NONE is returned then returns massive number else the gameValue*)
fun dropTest g = if isSome (hardDrop g) then gameValue (valOf(hardDrop g)) else 10000

(*converts a gamestate and a list of moves into a value, dropTests when out of commands*)
fun moveValue (g, []) = dropTest g
  | moveValue (g, l::ls) = moveValue(valOf (doCommand (g, l)), ls) 			      


(*returns amount of space right of block*)
fun spaceRight g =
    let
	fun spaceRight'(gg, n) =
	   case doCommand(gg, RightShift) of 
	       NONE => n
              |SOME (_)  => spaceRight'(valOf (doCommand(gg, RightShift)), n + 1)
    in
	spaceRight'(g, 0)
end

(*returns amount of space left of block*)
fun spaceLeft g = 
    let
	fun spaceLeft'(gg, n) =
	    case doCommand(gg, LeftShift) of 
		NONE => n 
	       |SOME (_)  => spaceLeft'(valOf (doCommand(gg, LeftShift)), n + 1)
    in
	spaceLeft'(g, 0)
end

(*returns list of moves for the best move choice on the right side of the matrix/gamestate and its value*)
fun bestRight g = 
    let
	val spotsRight = spaceRight g
	fun bestRight'(0, (l, v)) = (l, v)
	  | bestRight'(n, (l, v)) = 
	    let
		val moveList = moveRepetition (RightShift, n, [])
		val mVal = moveValue (g, moveList)			      					
	    in 
		if mVal < v then bestRight'(n - 1, (moveList, mVal)) else bestRight'(n - 1, (l, v))
	    end
	in
	    bestRight'(spotsRight, ([], moveValue (g, [])))	    
end

(* returns list of moves for the best move choice on the left side of the matrix/gamestate and its value*)
fun bestLeft g = 
    let
	val spotsLeft = spaceLeft g
	fun bestLeft'(0, (l, v)) = (l, v)
	  | bestLeft'(n, (l, v)) = 
	    let
		val moveList = moveRepetition (LeftShift, n, [])
		val mVal = moveValue (g, moveList)
	    in
		if mVal < v then bestLeft'(n - 1, (moveList, mVal)) else bestLeft'(n - 1, (l, v))
	    end
	in
	    bestLeft'(spotsLeft, ([], moveValue (g, [])))
end


(*return the best choice of both right and left side of the matrix and the value*)
fun bestChoice' g  = 
    let
	val (rightMoves, rightVal) = bestRight g
	val (leftMoves, leftVal) = bestLeft g
    in
	if rightVal < leftVal then (rightMoves, rightVal) else (leftMoves, leftVal)
end

(*returns the best choice of both right and left side of the matrix and rotation*)

fun bestChoice g = 
    let
	val (nMoves, nVal) = bestChoice' g
	val (eMoves, eVal) = bestChoice'(valOf (doCommand(g, RotateCW)))
	val (sMoves, sVal) = bestChoice'(valOf (doCommand((valOf (doCommand(g, RotateCW))), RotateCW)))
	val (wMoves, wVal) = bestChoice'(valOf (doCommand(g, RotateCCW)))

	val (neMoves, neVal) = if nVal < eVal then (nMoves, nVal) else (RotateCW::eMoves, eVal)
	val (swMoves, swVal) = if wVal < sVal then (RotateCCW::wMoves, wVal) else (RotateCW::RotateCW::sMoves, sVal)

        val bestMove = if neVal < swVal then neMoves else swMoves
     in
	 SoftDrop :: SoftDrop :: SoftDrop :: bestMove @ [HardDrop]
end
    



(*help functions for testing *)
fun newGameB (r,c) = gs((createMatrix (r,c, NONE : block option)),
	 ( Tetromino_T, ((c-1) div 2,0) : position ,  North),  
	Tetromino_I , 0)



fun h g = valOf (hardDrop g)

fun d (g, []) = h g
  | d (g, l::ls) = d (valOf (doCommand(g, l)), ls) 


val getGameCommands = bestChoice

end
