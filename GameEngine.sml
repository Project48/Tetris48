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
	(*position är en tuppel av typen int * int för att bestriva fria spelbara Tetromino:n position.
	Den första elementet av tuppeln är vilken komumn i spel matrisen.
	Den andra elementet av tuppeln är vilken rad i spel matrisen.
	*)
	type position = int * int
	(*gamestate är immutable datatype som besriver det aktuella speltilståndet.
		gamestate har instansen gs(spel_matrisen, (aktiv_tetromino_typ, aktiv_tetromino_position, aktiv_tetromino_facing),nästkommande_tetromino_typ)

		spel_matrisen: dom fastta block spelplanen. När en den aktiva tetromino slutligen hamnar Lock Down plaseras den som block i spel_matrisen.
			Om det är en 0 på en given ruta i matrisen betyder det att inget block ligger där, rutan är tom annars ligger ett fast block där
		
		aktiv_tetromino_typ: vilken type den spelbar tetromino har

		aktiv_tetromino_position: vilken position den spelbar tetromino har

		aktiv_tetromino_facing: vilken riktning den spelbar tetromino har

		nästkommande_tetromino_typ: vilken type den nästkommande tetromino kommer ha
		*)
	datatype gamestate = gs of matrix * (tetromino_type * position * orientation) * tetromino_type

	(* doCommand (state , command)
	TYPE: gamestate * gameCommand -> gamestate option
	PRE: TODO
	POST: give the next state after command is performed if the command is allowed on state else NONE
	EXEMPLE: TODO
		*)
	fun doCommand (g :gamestate, c :gameCommand) = NONE (*TODO*)

	(* Förslag *
	Validering av en gamestate för att undersöka om den befinersig i ett förbjudet tillstånd
	fun gamestate_Validation (g :gamestate) = true/false
	*)
end

