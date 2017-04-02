// 460. error XS9002: Parser: unexpected input '@' (used to be compiler crash)
//      error XS1003: Syntax error, 'Entity' expected


#translate ARRAY(<x>) => ArrayCreate(<x>)

#include "C460.CH"
FUNCTION Start( ) AS VOID
LOCAL aFiller := FillerClass{} AS FillerClass

// this one is ok:
@1,2 zSay "Filler             " zGet aFiller:Filler     PICTURE "@!" MESSAGE "Enter the filler" WHEN aFiller:Mode == "ADD" VALID Fillers->(zdbCheckKey(aFiller:Filler,"FillerFL"))

// error XS9002: Parser: unexpected input '@'
@1,2 zGet aFiller:Active CHECKBOX "Active " RIGHT ON .t. OFF .f. MESSAGE "Is this an active filler?" VALID ValidActive(aFiller)
RETURN




GLOBAL GetList := {} AS ARRAY

CLASS _GET_Object
	EXPORT reader AS USUAL
	EXPORT cargo AS USUAL
	METHOD Display() AS _GET_Object
	RETURN SELF
END CLASS

FUNCTION SetPos(y AS INT,x AS INT) AS VOID
	? y,x
RETURN 
FUNCTION Row() AS INT
RETURN 0
FUNCTION Col() AS INT
RETURN 0

FUNCTION DevPos(y AS INT,x AS INT) AS VOID
	? y,x
RETURN 
FUNCTION DevOut(cText AS STRING) AS VOID
	? cText
RETURN 
FUNCTION _GET_(oVar AS USUAL, cVar AS STRING, cPic := "" AS STRING, oValid := NIL AS USUAL, oWhen := NIL AS USUAL) AS _GET_Object
	? oVar , cVar , cPic , oValid , oWhen
	LOCAL oGet AS _GET_Object
	oGet := _GET_Object{}
RETURN _GET_Object{}

FUNCTION zdbCheckKey() CLIPPER
RETURN TRUE
FUNCTION zGEReader() CLIPPER
RETURN TRUE

CLASS FillerClass
	EXPORT Filler AS INT
	EXPORT Active AS INT
	EXPORT Mode := "" AS STRING
END CLASS
