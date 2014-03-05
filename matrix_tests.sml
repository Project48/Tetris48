(*matrix_tests.sml*)
use "Matrix";
open Matrix;
(1, nRows(createMatrix (1,2,3)) = 1);
(2, nCols(createMatrix (1,2,3)) = 2);
(3, getRow(createMatrix (1,2,0), 0) = Vector.fromList[0,0]); 
(4, getRow(setElement(createMatrix (4,3,0),3, 2, 8 ), 3) = Vector.fromList[0,0,8]);
(5,  getElement(setElement(createMatrix (4,3,0),3, 2, 8), 3, 2) = 8 );
(6,  getElement(setElement(createMatrix (4,3,0),3, 2, 8), 3, 1) = 0 );
(7,  getRow (setRow (createMatrix (5,5,0), 1, Vector.fromList[2,4,8,16,32]),1) = Vector.fromList[2,4,8,16,32]);
(8,  getRow (setRow (createMatrix (5,5,0), 1, Vector.fromList[2,4,8,16,32]),4) = Vector.fromList[0,0,0,0,0]);
(9, getElement(setRow (createMatrix (5,5,0), 1, Vector.fromList[2,4,8,16,32]), 1, 1) = 4);
(10, getElement(setRow (createMatrix (5,5,0), 1, Vector.fromList[2,4,8,16,32]), 1, 2) = 8);
(11, getElement(setRow (createMatrix (5,5,0), 1, Vector.fromList[2,4,8,16,32]), 2, 0) = 0);
