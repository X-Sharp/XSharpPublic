using System.IO
FUNCTION Throwme(e AS Exception) AS VOID
    THROW e
FUNCTION Start() AS VOID STRICT
    local aStruct as array
    aStruct := {{"ID", "N",10,0},{"MEMO","M",10,0}}
    DbCreate("test",aStruct, "DBFCDX")
    DbUseArea(TRUE, "DBFCDX", "test","test1", TRUE)
    DbUseArea(TRUE, "DBFCDX", "test","test2", TRUE)
    DbSelectArea("test1")
    DbAppend()
    FieldPut(1, 1)
    FieldPut(2, "Memo 1")
    BlobRootLock()
    DbSelectArea("test2")
    BlobRootLock()
    DbSelectArea("test1")
    BlobRootPut("test")
    BlobRootUnLock()
    DbCommit()
    DbSelectArea("test2")
    DbAppend()
    FieldPut(1, 2)
    FieldPut(2, "Memo 2")
    DbCommit()
    BlobRootLock()
    ? BlobRootGet()
    BlobRootUnLock()
    DbGoTop()
    DO WHILE ! Eof()
        ? FieldGet(2)
        DbSkip(1)
    ENDDO
    DbCloseAll()
    WAIT
