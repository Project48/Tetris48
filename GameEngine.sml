use "Matrix";

structure GameEngine = 
struct
    open Matrix

	(* REPRESENTATION CONVENTION: commands used to move tetrominos: 
   	   LeftShift: moves the tetromino one step left. 
   	   RightShift: moves the tetromino one step right. 
	   HardDrop: drops the tetromino the surface it lands on´and locks, making it impossible to move. 
	   SoftDrop: moves the tetromino one step down. 
	   RotateCW: rotates the tetromino 90 degrees clockwise. 
	   RotateCCW: rotates the tetromino 90 degrees counterclockwise.
	   REPRESENTATION INVARIANT: none
 	*)
	datatype gameCommand = LeftShift | RightShift | HardDrop | SoftDrop | RotateCW | RotateCCW
	
	(* REPRESENTATION CONVENTION: different types of the following onesided tetrominos:
	   Tetromino_O: 
	   [][]
	   [][]
	   Tetromino_I:
	   [][][][]
	   Tetromino_S:
	     [][]
	   [][]
	   Tetromino_Z:
	   [][]
	     [][]
	   Tetromino_L:
	       []
	   [][][]
   	   Tetromino_J:
	   []
	   [][][]
	   Tetromino_T:
	     []
	   [][][]
   	   REPRESENTATION INVARIANT: none
   	*)
	datatype tetromino_type = Tetromino_O | Tetromino_I | Tetromino_S | Tetromino_Z | Tetromino_L | Tetromino_J | Tetromino_T
	
	(* REPRESENTATION CONVENTION: directions that a onesided tetromino can have:
	   North: the initial direction
	   East: 90 degrees clockwise or 270 degrees counterclockwise from North. 
	   South: 180 degrees clockwise or 180 degrees counterclockwise from North. 
      	   West: 270 degrees clockwise or 90 degrees counterclockwise from North.
	   REPRESENTATION INVARIANT: none
	*)
	datatype facing = North | East | South | West
	
	(* REPRESENTATION CONVENTION: one of four blocks of a tetromino
   	   REPRESENTATION INVARIANT: none
 	*)
	datatype block = simpleblock;

	(* REPRESENTATION CONVENTION: position (x,y) where x is the column, and y is the row in the game matrix
	   REPRESENTATION INVARIANT: none
	 *)
	type position = int * int
		
	(* REPRESENTATION CONVENTION: a gamestate consisting of game matrix m, active tetromino type at, active tetromino position (x,y), active tetromino facing af, next tetromino type nt and clearRows clrRows, represented by (gs(m,(at,(x,y),af),nt,clrRows)). All blocks in m are lockdown, if an element in m is NONE then the place is empty
   	   REPRESENTATION INVARIANT: tetromino_type, position and facing must not be so that any part of the tetromino overlaps any non-empty block in the matrix, not either so it lies below, on the left or the right of the matrix
 	*)
	datatype gamestate = gs of block option matrix * (tetromino_type * position * facing) * tetromino_type * int
	
	(* createBlocks tt fac
	TYPE: tetromino_type -> facing -> position list
	PRE: none
	POST: a list of coordinates for tetromino type tt in the direction fac
	EXAMPLE: createBlocks Tetromino_T North = [(~1, 0), (0, ~1), (1, 0), (0, 0)]
	*)
	fun 	createBlocks Tetromino_T North 	= ((~1,0)::(0,~1)::(1,0)       ::(0,0)::nil) : position list
	|	createBlocks Tetromino_T East 	=         (0,~1)::(1,0)::(0,1)::(0,0)::nil
	|	createBlocks Tetromino_T South 	= (~1,0)        ::(1,0)::(0,1)::(0,0)::nil
	|	createBlocks Tetromino_T West 	= (~1,0)::(0,~1)       ::(0,1)::(0,0)::nil

	|	createBlocks Tetromino_I North 	= (~1,0)::(0,0)::(1,0)::(2,0)::nil
	|	createBlocks Tetromino_I East 	= (0,~1)::(0,0)::(0,1)::(0,2)::nil
	|	createBlocks Tetromino_I South 	= (~1,0)::(0,0)::(1,0)::(2,0)::nil
	|	createBlocks Tetromino_I West 	= (0,~1)::(0,0)::(0,1)::(0,2)::nil

	|	createBlocks Tetromino_J North   = (~1,~1)::(~1,0)::(0,0)::(1,0)::nil
	| 	createBlocks Tetromino_J East    = (1,~1)::(0,~1)::(0,0)::(0,1)::nil
	|       createBlocks Tetromino_J South   = (~1,0)::(0,0)::(1,0)::(1,1)::nil
	| 	createBlocks Tetromino_J West    = (0,~1)::(0,0)::(0,1)::(~1,1)::nil

	|	createBlocks Tetromino_L North   = (~1,0)::(0,0)::(1,0)::(1,~1)::nil
	|	createBlocks Tetromino_L East    = (0,~1)::(0,0)::(0,1)::(1,1)::nil
	|	createBlocks Tetromino_L South   = (~1,1)::(~1,0)::(0,0)::(1,0)::nil
	|	createBlocks Tetromino_L West	 = (~1,1)::(0,~1)::(0,0)::(0,1)::nil

	|	createBlocks Tetromino_S North 	 = (1,~1)::(0,~1)::(0,0)::(~1,0)::nil
	| 	createBlocks Tetromino_S East 	 = (0,~1)::(0,0)::(1,0)::(1,~1)::nil
	|	createBlocks Tetromino_S South	 = (1,0)::(0,0)::(0,1)::(~1,1)::nil
	|	createBlocks Tetromino_S West	 = (~1,~1)::(~1,0)::(0,0)::(0,1)::nil

	|	createBlocks Tetromino_Z North	 = (~1,~1)::(0,~1)::(0,0)::(1,0)::nil
	|	createBlocks Tetromino_Z East	 = (1,~1)::(1,0)::(0,0)::(0,1)::nil
	|	createBlocks Tetromino_Z South	 = (~1,0)::(0,0)::(0,1)::(1,1)::nil
	|	createBlocks Tetromino_Z West	 = (0,~1)::(0,0)::(~1,0)::(~1,1)::nil

	|	createBlocks Tetromino_O North	 = (0,~1)::(0,0)::(1,~1)::(1,0)::nil
	|	createBlocks Tetromino_O East	 = (0,~1)::(0,0)::(1,~1)::(1,0)::nil
	|	createBlocks Tetromino_O South	 = (0,~1)::(0,0)::(1,~1)::(1,0)::nil
	|	createBlocks Tetromino_O West	 = (0,~1)::(0,0)::(1,~1)::(1,0)::nil


	(* checkRow (m, i)
	TYPE: 'a option matrix * int -> bool
	PRE: 0 <= i < number of rows in m
	POST: true if row i in matrix m is full else false
	EXAMPLE: checkRow (createMatrix (2, 2, NONE : block option), 0) = false
	*)
	fun checkRow (m, i) = Vector.all  (fn NONE => false | SOME(_) => true) (getRow(m, i))

	(* moveRows (m, i)
        TYPE: 'a matrix * int -> 'a matrix
        PRE: 0 <= i <= number of rows in m
        POST: TODO
        EXAMPLE: moveRows (setRow((createMatrix (2, 2, NONE), 0, Vector.fromList[SOME(1), NONE])), 1) = 
        [SOME(1) NONE]
        [SOME(1) NONE]
        *)
        (*
        VARIANT: i
        *)
	fun moveRows (m , 0) = m
	| 	moveRows (m , i) = moveRows( setRow(m, i, getRow(m,i-1)) ,i-1)

        (* deleteRow' (state, i)
        TYPE: gamestate * int -> gamestate
        PRE: 0 <= i <= number of rows in m
        POST: if row i in matrix m in gamestate state is "full" then state with m without that row, but with an "empty" row at the top instead else state
        EXAMPLE: deleteRow' (gs(createMatrix(2,2,NONE),(at, ap, af),nt,clrRows)) = state
        *)
        (*
        VARIANT: i
        *)
	fun deleteRow' (g as gs(m,(at,ap,af),nt,clrRows) , i) =
	let 
	    val newRow = Vector.tabulate (nCols m, fn x => NONE)
	    val rows = nRows m
	in
	    if i < rows andalso checkRow(m, i) then 
		(deleteRow'(g, i+1) ; deleteRow'(gs(( Matrix.setRow(moveRows (m, i), 0, newRow)),(at,ap,af),nt,clrRows+1),0)) 
	    else if i < rows then 
		deleteRow'(g, i+1)
	    else
		g
	end

        (* deleteRow state
        TYPE: gamestate -> gamestate
        PRE: state must be validated
        POST: if row i in matrix m in gamestate state is "full" then state with m without that row, but with an "empty" row at the top instead else state
        EXAMPLE: deleteRow (gs(createMatrix(2,2,NONE),(at, ap, af),nt,clrRows)) = state
        *)
	fun deleteRow g = deleteRow' (g, 0)

	(* gamestate_Validation state
        TYPE: gamestate -> bool
        PRE: state must be validated
        POST: true if the active tetromino is inside the matrix or just above the top else false
        EXAMPLE: gamestate_Validation (gs((createMatrix(20,10, NONE),(Tetromino_I,(5,5),North),Tetromino_T,0))) = true
        *)
	fun gamestate_Validation (g as gs(m,(at,(x,y),af),nt,clrRows) ) = 
		let
			val blocks = List.map (fn (dx, dy) => (dy+y, dx+x)) (createBlocks at af)
		in
			not (isSome (
				List.find (fn (i,j) => j<0 orelse j >= nCols(m) orelse i< ~2 orelse i >= nRows(m) orelse if i<0 then false else isSome(getElement(m,i,j)) ) blocks
				))
		end
	
	(* lockDown_Validation state
	TYPE: gamestate -> bool
	PRE: state must be validated
        POST: true if active tetromino at in gamestate state is inside matrix m else false
        EXAMPLE: lockDown_Validation(gs((createMatrix(20,10, NONE),(Tetromino_I,(5,5),North),Tetromino_T,0))) = true
        *)
	fun lockDown_Validation (g as gs(m,(at,(x,y),af),nt,clrRows) ) = 
		let
			val blocks = List.map (fn (dx, dy) => (dy+y, dx+x)) (createBlocks at af)
		in
			not (isSome (
				List.find (fn (i,j) => j<0 orelse j >= nCols(m) orelse i< 0 orelse i >= nRows(m) orelse if i<0 then false else isSome(getElement(m,i,j)) ) blocks
				))
		end

	(*lockDown state
	TYPE: gamestate -> gamestate option
	PRE: state must be validated
        POST: if tetromino is outside of matrix in gamestate state then NONE else matrix m, with, if any, full rows removed, in state with active tetromino locked
        *)
	fun lockDown (g as gs(m,(at,(x,y),af),nt,clrRows)) = 
		if not (lockDown_Validation g) then 
			NONE
		else
		let 
			val nymatris 	= ( List.foldr (fn ((dx,dy) , ma ) => setElement (ma, y+dy, x+dx, SOME(simpleblock))) m (createBlocks at af) ) 
			val nypos 		= ((nCols m) div 2, ~1)
			val nyaf 		= North
			val nyat		= nt
			val nynt		= List.nth ([Tetromino_Z,Tetromino_O,Tetromino_S,Tetromino_L,Tetromino_J,Tetromino_I,Tetromino_T], (RunCall.unsafeCast(Time.now()) - (RunCall.unsafeCast(g) div 2)) mod 7)
		in 
			SOME (deleteRow( gs(nymatris,(nyat,nypos,nyaf),nynt,clrRows)))
		end

	(* hardDrop state
	TYPE: gamestate -> gamestate option
	PRE: state must be validated
	POST: gamestate state after a hardDrop operation
	*) 
	(*
	VARIANT: y
	*)
	fun hardDrop (g as gs(m,(at,(x,y),af),nt,clrRows)) =  
	if gamestate_Validation (gs(m,(at,(x,y+1),af),nt,clrRows)) then hardDrop (gs(m,(at,(x,y+1),af),nt,clrRows)) else lockDown(g)

	(* rcw f
	TYPE: facing -> facing
	PRE: none
	POST: direction when direction f is rotated 90 degrees clockwise
	EXAMPLE: rcw North = East
	*)
	fun rcw North = East
	  | rcw East = South
	  | rcw South = West
	  | rcw West  = North   
	
	(* rccw f
	TYPE: facing -> facing
	PRE: none
	POST: direction when direction f is rotated 90 degrees counterclockwise
	EXAMPLE: rccw East = North
	*)
	fun rccw East = North
	  | rccw South = East
	  | rccw West = South
	  | rccw North = West      


	(* doCommand (state, command)
	TYPE: gamestate * gameCommand -> gamestate option
	PRE: state must be validated
	POST: give the next state after command is performed on gamestate state if the command is allowed on state else NONE
	*)
	fun doCommand (g as gs(m,(at,(x,y),af),nt,clrRows), LeftShift) 	= Option.filter gamestate_Validation ( gs(m,(at,(x-1,y),af),nt,clrRows) )
	|	doCommand (g as gs(m,(at,(x,y),af),nt,clrRows), RightShift) = Option.filter gamestate_Validation ( gs(m,(at,(x+1,y),af),nt,clrRows) )
	|	doCommand (g as gs(m,(at,(x,y),af),nt,clrRows), SoftDrop) 	= if gamestate_Validation g 
																then	if gamestate_Validation (gs(m,(at,(x,y+1),af),nt,clrRows) )
																	 		then SOME ( gs(m,(at,(x,y+1),af),nt,clrRows) )
																	 		else lockDown (g) 	
																else	NONE
	|	doCommand (g as gs(m,(at,ap,af),nt,clrRows), RotateCW) = Option.filter gamestate_Validation ( gs(m,(at,ap, (rcw af)),nt,clrRows) )
	|	doCommand (g as gs(m,(at,ap,af),nt,clrRows), RotateCCW) = Option.filter gamestate_Validation ( gs(m,(at,ap, (rccw af)),nt,clrRows) )
	|	doCommand (g, HardDrop) = hardDrop g
	|	doCommand (g :gamestate, c :gameCommand) = NONE (*unknown command*)
	
end
