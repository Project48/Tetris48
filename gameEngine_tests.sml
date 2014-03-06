use "Matrix";
use "GameEngine";
open GameEngine;

(1, (rcw o rccw) North = North);
(2, (rcw o rccw) East = East);
(3, (rcw o rccw) South = South);
(4, (rcw o rccw) West  = West);
(5, (rccw o rcw) North = North);
(6, (rccw o rcw) East = East);
(7, (rccw o rcw) South = South);
(8, (rccw o rcw) West  = West);
(9, (fn gs(_,_,_ , claerRow) => claerRow) (gs(Matrix.createMatrix (5,5,SOME(simpleblock)),(Tetromino_O,(0,0),North), Tetromino_O , 0)) = 0);
(10, 
((fn gs(_,_,_ , claerRow) => claerRow) o deleteRow) 
(gs(Matrix.createMatrix (5,10,SOME(simpleblock)),(Tetromino_O,(0,0),North), Tetromino_O , 0)) = 5);
(11, 
((fn gs(_,_,_ , claerRow) => claerRow) o deleteRow) 
(gs(Matrix.setElement(Matrix.createMatrix (5,10,SOME(simpleblock)),1,1, NONE : block option),(Tetromino_O,(0,0),North), Tetromino_O , 0)) = 4);

(12, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_T North)))

 then true else (not o gamestate_Validation) (gs(Matrix.createMatrix (100,100,SOME(simpleblock)),(Tetromino_T,(50,50),North), Tetromino_T , 0))
);

(13, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_T North)))

 then true else (gamestate_Validation) (gs(Matrix.createMatrix (100,100,NONE : block option),(Tetromino_T,(50,50),North), Tetromino_T , 0))
);

(14, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_T North)))

 then true else (not o lockDown_Validation) (gs(Matrix.createMatrix (100,100,SOME(simpleblock)),(Tetromino_T,(50,50),North), Tetromino_T , 0))
);

(15, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_I North)))

 then true else (lockDown_Validation) (gs(Matrix.createMatrix (100,100,NONE : block option),(Tetromino_I,(50,50),North), Tetromino_I , 0))
);

(16, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_I North)))

 then true else isSome (doCommand (gs(Matrix.createMatrix (100,100,NONE : block option),(Tetromino_I,(50,50),North), Tetromino_I , 0),HardDrop))
);

(17, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_I North)))

 then true else isSome (doCommand (gs(Matrix.createMatrix (100,100,NONE : block option),(Tetromino_I,(50,50),North), Tetromino_I , 0),LeftShift))
);

(18, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_I North)))

 then true else isSome (doCommand (gs(Matrix.createMatrix (100,100,NONE : block option),(Tetromino_I,(50,50),North), Tetromino_I , 0),RightShift))
);

(19, 
if (isSome(List.find (fn (x,y) => x < ~10 orelse 10 < x orelse y < ~10 orelse y > 10)  
(createBlocks Tetromino_I North)))

 then true else isSome (doCommand (gs(Matrix.createMatrix (100,100,NONE : block option),(Tetromino_I,(50,50),North), Tetromino_I , 0),SoftDrop))
);
