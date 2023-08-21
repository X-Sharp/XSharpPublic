// 653. error XS1520: Method must have a return type
/*
Problem happens when normally the two classes are inf different files, but happens also in single file as below
Also note that the order of which the classes appear makes a difference, reordering them causes the error to go away

Also th eerror message does not include file/line information
*/
PARTIAL CLASS WrongCASE
END CLASS

PARTIAL CLASS WrongCaSe
CONSTRUCTOR(a,b)
END CLASS

FUNCTION Start() AS VOID

RETURN
