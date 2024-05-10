
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID STRICT
    ? "DBFMEMO"
    CreateMemo()
    CreateBlob()
FUNCTION CreateMemo as VOID

    ? RddSetDefault("DBFMEMO")
    ? RddSetDefault()
    ? DBCREATE("Test", {{"ID","N",10,0}, {"MEMO","M",10,0}}) // creates test.dbf and test.dbv
    ? DBUSEAREA(TRUE,,"Test")
    FOR VAR i := 1 to 10
        DBAPPEND()
        FIELDPUT(1,i)
        FIELDPUT(2,"Some text for the memo "+Strzero(i,10))
        DBCOMMIT()
    NEXT
    DbCloseArea()
 ? DBUSEAREA(TRUE,,"Test")
    DO WHILE ! EOF()
    	? FieldGet(1), FieldGet(2)
    	DbSkip(1)
    ENDDO
    DbCloseArea()



FUNCTION CreateBlob as VOID
    ? "DBFBLOB"
    RddSetDefault("DBFBLOB")
    ? "Create File"
    ? DbCreate("testBlob.dbv") // does not need a fields list. Just creates the Memo Header.
    WriteBlob()
    ReadBlob()
    wait
function  WriteBlob as void
    DbUseArea(.T.,,"testBlob.dbv")
    var cKeys := ""
    FOR VAR i := 1 to 2
        var block := BlobDirectPut(0, "test "+strzero(i,10))
        cKeys += Ntrim(block)+";"
        ? "Write Block", i, block
    NEXT
    ? "Put Root:", cKeys
    ? BLobRootLock()
    ? BlobRootPut(cKeys)
    ? BlobRootUnlock()
    DbCloseArea()
function ReadBlob as void
    DbUseArea(.T.,,"testBlob.dbv")
    BlobRootLock()
    var cKeys := (string) BlobRootGet()
    ? "Read Root", cKeys
    var keys := cKeys:Split(c';')

    BlobRootUnlock()
    FOREACH var cKey in keys
        if ! String.IsNullOrEmpty(cKey)
            ? "Read Block", cKey, BlobDirectGet(Val(cKey))
        endif
    NEXT
    DbCloseArea()

