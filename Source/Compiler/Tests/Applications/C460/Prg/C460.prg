// 460. error XS9002: Parser: unexpected input '@' (used to be compiler crash)
//      error XS1003: Syntax error, 'Entity' expected

#pragma warnings(9043, off) // zGEDBCOmboNew double defined
#xtranslate :Field1 => \[1 \]
#xtranslate :Field2 => \[2 \]


#translate ARRAY(<x>) => ArrayCreate(<x>)

#include "C460.CH"
FUNCTION Start( ) AS VOID
LOCAL oFiller := FillerClass{} AS FillerClass
LOCAL aFiller := ArrayCreate(2) AS ARRAY
LOCAL GetList := {} AS ARRAY

aFiller:Field1 := "Field1"
aFiller:Field2 := "Field2"

@1,2 zSay "Filler             " zGet aFiller:Field1     PICTURE "@!" MESSAGE "Enter the filler" WHEN aFiller:Mode == "ADD" VALID Fillers->(zdbCheckKey(aFiller:Filler,"FillerFL"))
@1976,2 zGet aFiller:Field2 CHECKBOX "Active " RIGHT ON .t. OFF .f. MESSAGE "Is this an active filler?" VALID ValidActive(aFiller)

xAssert(Row() == 1976)
xAssert(ALen(GetList) == 2)


ASize(GetList , 0)

// this one is ok:
@1,2 zSay "Filler             " zGet oFiller:Filler     PICTURE "@!" MESSAGE "Enter the filler" WHEN aFiller:Mode == "ADD" VALID Fillers->(zdbCheckKey(aFiller:Filler,"FillerFL"))

xAssert(Row() == 1)

// error XS9002: Parser: unexpected input '@'
@1976,2 zGet oFiller:Active CHECKBOX "Active " RIGHT ON .t. OFF .f. MESSAGE "Is this an active filler?" VALID ValidActive(aFiller)

xAssert(Row() == 1976)

xAssert(ALen(GetList) == 2)
LOCAL oGet AS _GET_Object
oGet := GetList[1]
? oGet:cargo[2]
xAssert(oGet:cargo[2] == "Enter the filler")

xAssert(GetList[2]:cargo[2] == "Is this an active filler?")
RETURN

FUNCTION zGECheckNew(a,b,c,d,e,f,g,h,i) CLIPPER
	? "zGECheckNew arguments:",a,b,c,d,e,f,g,h,i
	xAssert(f == TRUE)
	LOCAL oGet AS _GET_Object
	oGet := _GET_Object{}
	oGet:cargo := ArrayCreate(10)
RETURN oGet


CLASS FillerClass
	EXPORT Filler AS INT
	EXPORT Active AS INT
	EXPORT Mode := "" AS STRING
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

