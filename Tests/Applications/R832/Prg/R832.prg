// Tests for https://github.com/X-Sharp/XSharpPublic/issues/915
// compile with /nostddefs
#command  COPY STRUCTURE TO <(dbFile)> [FIELDS <fields,...>] ; 
   =>  DbCopyStruct( <(dbFile)>, {<(fields)>} ) 

#command LIST [<list,...>]                                              ;
         [<off:OFF>]                                                    ;
         [<toPrint: TO PRINTER>]                                        ;
         [TO FILE <(toFile)>]                                           ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => __dbList(                                                      ;
                   <.off.>, { <{list}> }, .t.,                          ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>,         ;
                   <.toPrint.>, <(toFile)>                              ;
                 )
   

#translate RENAME(<(oldFile)>) => RENAMEDFILE(<(oldFile)>)

FUNCTION Start( ) AS VOID
    local cFileName as STRING
    LOCAL cField1 as String
    local cField2 as STRING      
    FIELD FirstName, LastName
    cFileName := "Filename"
    cField1   := "Field1"
    cField2   := "Field2"
	COPY STRUCTURE TO (cFileName) FIELDS (cField1), (cField2)       
	
	LIST FIRSTNAME, LASTNAME TO PRINTER WHILE LastName < "K"
	RENAME(Temp.dbf) 
RETURN

FUNCTION DbCopyStruct( cFileName AS STRING, aFields as array) 
xAssert(cFileName == "Filename" )
xAssert(aLen(aFields) == 2)
xAssert(aFields[1] == "Field1")
xAssert(aFields[2] == "Field2")
RETURN NIL    


                    
FUNCTION __DbList(lOff, aBlocks, lPrint, uFor, uWhile, nNext, nRec, lRest, lToPrint, cFile)
xAssert(lOff == false)
xAssert(Alen(aBlocks) == 2)
XAssert(IsCodeBlock(aBlocks[1]))    
XAssert(IsCodeBlock(aBlocks[2]))
xAssert(IsNil(uFor))
xAssert(IsCodeBlock(uWhile))
xAssert(IsNil(nNext))
xAssert(IsNil(nRec))
xAssert(lRest == false)
xAssert(lToPrint == true)
xAssert(IsNil(cFile))          
RETURN NIL


FUNCTION RENAMEDFILE(cFile AS STRING)  
xAssert(cFile == "Temp.dbf")              
return NIL


PROC xAssert(l AS LOGIC) AS VOID
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
