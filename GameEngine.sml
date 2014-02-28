use "Matrix";

(*En Spelmotorn för tetris. *)
structure GameEngine = 
struct
    open Matrix
	(*gameCommand är en datatype för dom input kommandon som en spelare använder i Tetris spelandet.
	Ett komando är som en knapklikning. 
	Om man vill använda sig av Delayed Auto Shift eller autorepeat bör det göras i koden som mappar spelerens input till gameCommand som en serie av knapptrykningar eller i hårdvaran. 
	LeftShift: 	flytar tetromino:n en steg till vänster 
	RightShift:	flytar tetromino:n en steg till höger
	HardDrop:	gör att tetromino:n faller ner omedelbart på den första ytan den landar på och en omedelbart Lock Down(Spelaren kan inte längre flyta eller rotera tetromino:n)
	SoftDrop:	flytar tetromino:n en steg till ner
	RotateCW: 	Roterar tetromino:n 90 grader medsols
	RotateCCW:	Roterar tetromino:n 90 grader motsols
	*Med tetromino:n syftas det på den tetromino:n som inte Lock Down, dvs den spelaren frortfarande kan flytta/rotera på.
	*)
	datatype gameCommand = LeftShift | RightShift | HardDrop | SoftDrop | RotateCW | RotateCCW
	(*Datatype används som idenifierar för dom 7 olika typerna en ensidiga tetromino:na kan ha.
	Dom ensidiga tetromino:na får roteras men inte reflekteras. Nedan vissas endast en riknig av vare ensidiga tetromino.
	tetromino_O: 
	[][]
	[][]
	tetromino_I:
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
		*)
	datatype tetromino_type = Tetromino_O | Tetromino_I | Tetromino_S | Tetromino_Z | Tetromino_L | Tetromino_J | Tetromino_T
	(*Denna datatype är tänkt att basriva dom fyra riktningar ensidig Tetromino kan ha.
		North: grund/start riktningen av en Tetromino 
		East: är 90 grader medsols eller 270 grader motsols från grund riktningen
		South: är 180 grader medsols eller 180 grader motsols från grund riktningen
		West: är 270 grader medsols eller 90 grader motsols från grund riktningen
		  *)
	datatype facing = North | East | South | West
	
	(*Denna datatyp inehåller iformation som används under rendering*)
	datatype block = simpleblock;

	(*position är en tuppel av typen int * int för att bestriva fria spelbara Tetromino:n position.
	Den första elementet av tuppeln är vilken komumn i spel matrisen.
	Den andra elementet av tuppeln är vilken rad i spel matrisen.
	*)
	type position = int * int
	(*gamestate är immutable datatype som besriver det aktuella speltilståndet.
		gamestate har instansen gs(spel_matrisen, (aktiv_tetromino_typ, aktiv_tetromino_position, aktiv_tetromino_facing),nästkommande_tetromino_typ)

		spel_matrisen: dom fasta block i spelplanen. När en den aktiva tetromino slutligen hamnar i Lock Down plaseras den som block i spel_matrisen.
			Om det är en NONE på en given ruta i matrisen betyder det att inget block ligger där, rutan är tom annars ligger ett fast block där
		
		aktiv_tetromino_typ: vilken type den spelbar tetromino har

		aktiv_tetromino_position: vilken position den spelbar tetromino har

		aktiv_tetromino_facing: vilken riktning den spelbar tetromino har

		nästkommande_tetromino_typ: vilken type den nästkommande tetromino kommer ha
		*)
	datatype gamestate = gs of block option matrix * (tetromino_type * position * facing) * tetromino_type
	

	(*Skapar en matris med blocken från en tetromino_type och facing*)
	fun createBlocks Tetromino_T North 	= (~1,0)::(0,~1)::(1,0)       ::(0,0)::nil
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




	(* Förslag *
	Validering av en gamestate för att undersöka om den befinersig i ett förbjudet tillstånd
	POST: true om g är tillåtet annars false
	*)
	fun gamestate_Validation (g as gs(m,(at,(x,y),af),nt) ) = 
		let
			val blocks = List.map (fn (dx, dy) => (dy+y, dx+x)) (createBlocks at af)
		in
			not (isSome (
				List.find (fn (i,j) => j<0 orelse j >= nCols(m) orelse i< ~2 orelse i >= nRows(m) orelse if i<0 then false else isSome(getElement(m,i,j)) ) blocks
				))
		end

	(*Låser det aktuela blocket*)
	(*lockDown state
	TYPE: gamestate => gamestate option
		*)
	fun lockDown (g as gs(m,(at,(x,y),af),nt)) = 
		if not (gamestate_Validation g) then 
			NONE
		else
		let 
			val nymatris 	= ( List.foldr (fn ((dx,dy) , ma ) => setElement (ma, y+dy, x+dx, SOME(simpleblock))) m (createBlocks at af) ) 
			val nypos 		= ((nCols m) div 2, ~1)
			val nyaf 		= North
			val nyat		= nt
			val nynt		= at (*Byter bara plats på aktuela och nästa just nu*)
		in 
			SOME ( gs(nymatris,(nyat,nypos,nyaf),nynt))
		end

	(*harddrop' state
	TYPE: gamestate -> gamestate option
	PRE: state måste vara validerad
	POST: state efter en harddrop opration
	*) 
	fun hardDrop (g as gs(m,(at,(x,y),af),nt)) =  
	if gamestate_Validation (gs(m,(at,(x,y+1),af),nt)) then hardDrop (gs(m,(at,(x,y+1),af),nt)) else lockDown(g)


	(*Map riktning + 90grader*)
	fun rcw North = East
	  | rcw East = South
	  | rcw South = West
	  | rcw West  = North   
	(*Map riktning - 90grader*)
	fun rccw East = North
	  | rccw South = East
	  | rccw West = South
	  | rccw North = West      


	(* doCommand (state , command)
	TYPE: gamestate * gameCommand -> gamestate option
	PRE: TODO
	POST: give the next state after command is performed if the command is allowed on state else NONE
	EXEMPLE: TODO
		*)
	fun doCommand (g as gs(m,(at,(x,y),af),nt), LeftShift) 	= Option.filter gamestate_Validation ( gs(m,(at,(x-1,y),af),nt) )
	|	doCommand (g as gs(m,(at,(x,y),af),nt), RightShift) = Option.filter gamestate_Validation ( gs(m,(at,(x+1,y),af),nt) )
	|	doCommand (g as gs(m,(at,(x,y),af),nt), SoftDrop) 	= if gamestate_Validation g 
																then	if gamestate_Validation (gs(m,(at,(x,y+1),af),nt) )
																	 		then SOME ( gs(m,(at,(x,y+1),af),nt) )
																	 		else lockDown (g) 	
																else	NONE
	|	doCommand (g as gs(m,(at,ap,af),nt), RotateCW) = Option.filter gamestate_Validation ( gs(m,(at,ap, (rcw af)),nt) )
	|	doCommand (g as gs(m,(at,ap,af),nt), RotateCCW) = Option.filter gamestate_Validation ( gs(m,(at,ap, (rccw af)),nt) )
	|	doCommand (g, HardDrop) = hardDrop g
	|	doCommand (g :gamestate, c :gameCommand) = NONE (*unknown command*)
	
	(* checkRow (m, i)
	TYPE: 'a option matrix * int -> bool
	PRE:
	POST: true if row i in matrix m is full else false
	EXAMPLE: checkRow(Vector.fromList[Vector.fromList[SOME(1),SOME(1),SOME(1)],Vector.fromList[NONE,NONE,NONE]], 0) = true
	*)
	fun checkRow (m, i) = Vector.all  (fn NONE => false | SOME(_) => true) (getRow(m, i))
	
	(* deleteRow (m, i)
	TYPE: 'a option matrix * int -> 'a option matrix
	PRE:
	POST: if row i contains is full then delete that row add an empty row at the end
	EXAMPLE: deleteRow(Vector.fromList[Vector.fromList[SOME(1),SOME(1),SOME(1)],Vector.fromList[NONE,NONE,NONE]], 0)
	*)
	fun deleteRow (m, i) = (* code here *)

end

