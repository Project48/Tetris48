signature BOT = 
sig
	val getGameCommands : GameEngine.gamestate -> GameEngine.gameCommand list
end

structure Bot :> BOT =
struct

open Matrix
open GameEngine

(* moveRepetition (c, n, l)
TYPE: int * 'a * 'a list -> 'a list
PRE:none
POST: n amounts of element c appended to l
EXAMPLE: moveRepetition (2, "hej", []) = ["hej", "hej"]
*)
(*VARIANT: n *)
fun moveRepetition (c, 0, l) = l
  | moveRepetition (c, n, l) = moveRepetition (c, n - 1, c::l)


(* gameValue g
TYPE: gameState -> int
PRE: true
POST: the game value of g*)
(*VARIANT:*)
fun gameValue (g as gs(m,(at,(x,y),af),nt, score)) =
    let
	val nRows = nRows m
	val nCols = nCols m
			  
	(* gameValue' (r, c, n)
        TYPE: int * int * int -> int
        PRE: all rows and columns are same length
        POST: height component of the game value
	 *)
	(*VARIANT: r *)	 
	fun gameValue' (r, c, n) =
	    if r >= nRows then n 
	    else if c >= nCols then gameValue'(r+1, 0, n)
	    else if isSome (Matrix.getElement(m, r, c)) then gameValue'(r, c+1, n + (nRows - (r + 1))) 
	    else gameValue'(r, c+1, n)

	(* holesAmount' (r, c, n)
        TYPE: int * int * int -> int
        PRE: all rows and columns are same length
        POST: amount of empty spaces in column c starting at row r
	 *)
	(*VARIANT: r *)		   
	fun holesAmount' (r, c, n) =
	    if r >= nRows then n 
	    else if isSome (Matrix.getElement(m, r, c)) then holesAmount' (r + 1, c, n)
	    else holesAmount' (r + 1, c, n + 1)								 
			      
       (* holesAmount (r, c, n)
        TYPE: int * int * int -> int
        PRE:all rows and columns are same length
        POST: amount of "holes" in g
	 *)
	(*VARIANT: c *)
       fun holesAmount (r, c, n) = 
	   if c >= nCols then n
	   else if r >= nRows then holesAmount(0, c + 1, n)
	   else if isSome (Matrix.getElement(m, r, c)) then holesAmount(0, c + 1, holesAmount'(r, c, n))
	   else holesAmount (r + 1, c, n)

    in
	gameValue'(0, 0, 0) + (4 * holesAmount(0, 0, 0)) + (~5 * score)
    end 

(* dropTest g
TYPE: gameState -> int
PRE: true
POST: ifSome g then gameValue (hardDrop g) else 10000
*)
fun dropTest g = if isSome (hardDrop g) then gameValue (valOf(hardDrop g)) else 10000

(* moveValue (g, l)
TYPE: gameState * command List -> int
PRE: true
POST: dropTest value of g after all commands in l have been executed, if invalid move occurs then 10000
*)
(*VARIANT: length l *)
fun moveValue (g, []) = dropTest g
  | moveValue (g, l::ls) = 
    case doCommand (g, l) of SOME (x) => moveValue(x, ls)
			  |  NONE  => 10000	

(* spaceRight g
TYPE: gameState -> int
PRE: true
POST: amount of steps possible to move to the right
*)
fun spaceRight g =
    let
        (* spaceRight g
        TYPE: gameState * command -> int
        PRE: true
        POST: amount of steps possible to move to the right
        EXAMPLE:
	 *)
	(*VARIANT: isSome gg *)
	fun spaceRight'(gg, n) =
	   case doCommand(gg, RightShift) of 
	       NONE => n
              |SOME (_)  => spaceRight'(valOf (doCommand(gg, RightShift)), n + 1)
    in
	spaceRight'(g, 0)
end

(* spaceLeft g
TYPE:gameState -> int
PRE: true
POST: amount of steps possible to move to the left
*)
fun spaceLeft g = 
    let
	(* spaceRight g
        TYPE: gameState * command -> int
        PRE: true
        POST: amount of steps possible to move to the left
        EXAMPLE:
	 *)
	(*VARIANT: isSome gg *)
	fun spaceLeft'(gg, n) =
	    case doCommand(gg, LeftShift) of 
		NONE => n 
	       |SOME (_)  => spaceLeft'(valOf (doCommand(gg, LeftShift)), n + 1)
    in
	spaceLeft'(g, 0)
end

(* bestRight g
TYPE: gameState -> command list * int
PRE: true
POST: tuple of list of series best commands to perform for lowest game value and the game value right side of the moving block
*)
fun bestRight g = 
    let
	val spotsRight = spaceRight g

	(* bestRight' n (l, v)
        TYPE: int * (command list * int) -> command list * int
        PRE: true
        POST: tuple of list of series best commands to perform for lowest game value and the game value right side of the moving block
	 *)
	(*VARIANT: n *)			    
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

(* bestLeft g
TYPE: gameState -> command list * int
PRE: true
POST: tuple of list of series best commands to perform for lowest game value and the game value left side of the moving block
*)
fun bestLeft g = 
    let
	val spotsLeft = spaceLeft g
	(* bestLeft' n (l, v)
        TYPE: int * (command list * int) -> command list * int
        PRE: true
        POST: tuple of list of series of commands to perform for lowest game value and the game value left side of the moving block
	 *)
	(*VARIANT: n *)				  
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
								  
(* bestChoice' g
TYPE: gameState -> command list * int
PRE: true
POST: tuple of list of series of commands to perform for the lowest game value and that game value without rotating
*)
fun bestChoice' g  = 
    let
	val (rightMoves, rightVal) = bestRight g
	val (leftMoves, leftVal) = bestLeft g
    in
	if rightVal < leftVal then (rightMoves, rightVal) else (leftMoves, leftVal)
end

(* rotationHelper g l
TYPE: gameState * command list -> command list * int
PRE: true
POST: if performing all commands in l is valid then bestChoice' g else ([SoftDrop], 10000)
*)
(*VARIANT: lenght l*)
fun rotationHelper (g, []) = bestChoice' g
  | rotationHelper (g, l::ls) =
    case doCommand (g, l) of NONE => ([SoftDrop], 10000)
			   | SOME (x) => rotationHelper (x, ls)

(* bestChoice g
TYPE: gameState -> command list
PRE: true
POST: tuple of list of series of commands to perform for the lowest game value with rotations
*)
fun bestChoice g = 
    let
	val (nMoves, nVal) = bestChoice' g
	val (eMoves, eVal) = rotationHelper (g, [RotateCW])
	val (sMoves, sVal) = rotationHelper (g, [RotateCW, RotateCW])
	val (wMoves, wVal) = rotationHelper (g, [RotateCCW])
	
	val (neMoves, neVal) = if eVal < nVal then (RotateCW::eMoves, eVal) else (nMoves, nVal)
	val (swMoves, swVal) = if sVal < wVal then (RotateCW::RotateCW::sMoves, sVal) else(RotateCCW::wMoves, wVal)  

        val bestMove = if swVal < neVal then swMoves else neMoves
     in
	 SoftDrop :: SoftDrop :: SoftDrop :: bestMove @ [HardDrop]
end
    
val getGameCommands = bestChoice

end
