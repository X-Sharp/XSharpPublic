FUNCTION Start as VOID
? PROGRAM()
? 0, PROGRAM(0)
? 1, PROGRAM(1)
? 2, PROGRAM(2)
DO myproc
wait

PROCEDURE myproc
? PROGRAM()
? 0, PROGRAM(0)
? 1, PROGRAM(1)
? 2, PROGRAM(2)

return



FUNCTION xStart(args as string[]) AS VOID
LOCAL cBinFolder,cBinDbf AS STRING
LOCAL cTestFolder,cTestdbf AS STRING
LOCAL cFilename AS STRING

cBinFolder := WorkDir()
cTestFolder := "C:\Test\"
cFilename := "testpath.dbf"
? ProcName(0, FALSE)
? ProcName(0, TRUE)
? Program(-1)
? Program(0)
? Program(0,TRUE)
cBinDbf := cBinFolder + cFilename
cTestdbf := cTestFolder + cFilename

? "Bin path:", cBinDbf
? "Test path:", cTestdbf
?

DbCreate(cBinDbf , {{"FLD","C",10,0}})
DbUseArea(TRUE,,cBinDbf)
DbAppend()
FieldPut(1, "BIN")
DbCloseArea()

DbCreate(cTestdbf , {{"FLD","C",10,0}})
DbUseArea(TRUE,,cTestdbf)
DbAppend()
FieldPut(1, "Test")
DbCloseArea()


? DbUseArea(TRUE,,cFilename)
? FieldGet(1) // "BIN", file is opened from exe fodler, correct
DbCloseArea()

SetDefault(cTestFolder)

? DbUseArea(TRUE,,cFilename)
? FieldGet(1) // "BIN" again, wrong, this should be opened from the test folder, like VO does
DbCloseArea()

FErase(cBinDbf)

? DbUseArea(TRUE,,cFilename)
? FieldGet(1) // "Test", now it is indeed being opened from the test folder
DbCloseArea()
wait
RETURN
