// 771. Various problems with compiling VFP code
/*
Taken from https://www.xsharp.eu/forum/welcome/2421-arrays-in-foxpro#17765
Confirmed that the following code compiles and runw without errors in VFP
*/
FUNCTION Start as VOID
PRIVATE mvec1, mvec2, mvecnew
DIMENSION mvec (1,2)
DO AssignValueToVector WITH "mvec", 1,2, "hello world!!!"
? mvec(1,2)

STORE .f. TO mvecnew // all PRIVATE variables must be inicialized TO exist after the NEXT line. The first value DEFINE de PROCEDURE or FUNCTION owner of the variable. IF you DO not put this line the variable IS created and disposed inside AssignValueToVector PROCEDURE
? AssignValueToVector ("mvecnew",3,2, 1234)
? mvecnew(3,2)

AssignValueToVector ( @mvec, 2 , 2, "hello" ) // FUNCTION default IS by val. It need @ before REF param
? mvec(2,2)

*!* DO AssignValueToVector WITH @mvecnew, 1 , 2, "VFP compiler syntax error in VFP!!!"

maux= CREATEOBJECT("myclass")
maux.methodToAssignValue(1,2,"hello")
? maux.arrayproperty(1,2)
WAIT
RETURN
****************************
PROCEDURE AssignValueToVector
PARAMETERS pvec, prow, pcol, pvalue // pvec ARRAY passed by REF or passed name of ARRAY defined before
IF Type("&pvec.(3,1)")="U" // TO detects IF the ARRAY IS not declared correctli
IF Type("pvec")="C"
dimension &pvec.(3,2) // An ARRAY could change dimensions without lost the values. Pay atention: "you can use macros too"
ENDIF
ENDIF

IF Type("&pvec.(1,1)")<> "U"
&pvec.(prow,pcol)= pvalue // Pay atention: "you can use macros too"
ELSE
pvec(prow,pcol)= pvalue
ENDIF
RETURN
**************************************
DEFINE CLASS MYCLASS AS Custom
DIMENSION ARRAYPROPERTY (2,2)
PROCEDURE methodToAssignValue (prow,pcol,pvalue)
//this. arrayproperty(prow,pcol)= pvalue
ENDDEFINE
