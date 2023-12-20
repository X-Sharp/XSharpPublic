using XSHarp.VFP
FUNCTION Start( ) AS VOID
    try
	local aStruct as Array
	local i as dword
	aStruct := {{"FLD1","C",10,0}, {"FLD2","N",10,0}}
	DbCreate("R915", aStruct)
	DbUseArea(TRUE, ,"R915")
	For  i := 1 upto 10
	    local oTest as Test
	    local oObj as Empty
	    oObj := Empty{}

        otest := Test{i}
        with oTest
    	    DbAppend()
    	    REPLACE FLD1 with GetText()
    	    REPLACE FLD2 with :Number
        end with
        SCATTER MEMVAR
        ? M->FLD1
        ? M->FLD2
	next
	DbCommit()
	var fld := "FLD1"
    For  i := 1 upto 10
        locate for  &fld ==  Repl(Chr(64+i),10)
        xAssert(FieldGet(2) == i )
    next
    catch e as Exception
        ? e:ToString()
        end try
RETURN

#pragma options("LB", on)
FUNCTION GetText()
    RETURN :Text
#pragma options("LB", default)

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


CLASS Test
    PROPERTY Text as STRING AUTO
    PROPERTY Number as DWORD AUTO
    CONSTRUCTOR(nNum as DWORD)
        SELF:Text   :=  Repl(Chr(64+nNum),10)
        SELF:Number := nNum
        REPLACE FLD1 with ::Text
END CLASS
