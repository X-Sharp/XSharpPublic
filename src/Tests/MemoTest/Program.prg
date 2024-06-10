
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Start() AS VOID
	LOCAL cFileName AS STRING
	LOCAL cBaseRdd AS STRING
	LOCAL cIndex AS STRING
	LOCAL lNtx AS LOGIC

	cFileName := "c:\test\dbmemo"
	lNtx := false

	IF lNtx
		cBaseRdd := "DBFNTX"
		cIndex := cFileName + ".ntx"
	ELSE
		cBaseRdd := "DBFCDX"
		cIndex := cFileName + ".cdx"
	ENDIF

	FErase(cIndex)

	? DbCreate(cFileName, {{"CFIELD","C",10,0},{"NFIELD","N",8,2},{"MFIELD","M",10,0}},cBaseRdd,,,,,{"DBFMEMO"})

	? DbUseArea(TRUE,cBaseRdd,cFileName,,,,,,{"DBFMEMO"})
	DbAppend()
	FieldPut(1,"test1")
	FieldPut(2,1)
	FieldPut(3,"some text")
	DbAppend()
	FieldPut(1,"test3")
	FieldPut(2,3)
	FieldPut(3,"some text3")
	DbAppend()
	FieldPut(1,"test2")
	FieldPut(2,2)
	FieldPut(3,"some text2")
	DbGoTop()
	? DbCreateIndex(cFileName,"NFIELD")
	DbGoBottom()
    ? RecNo(), FieldGet(1), FieldGet(2), FieldGet(3)
    ? DbOrderInfo(DBOI_FULLPATH)
    DbCloseArea()


	? File(cIndex)

    wait

FUNCTION Startx() AS VOID STRICT
    ? "DBFMEMO"
    CreateMemo()
    CreateBlob()
FUNCTION CreateMemo as VOID

    ? RddSetDefault("DBFCDX")
    ? DBCREATE("Test", {{"ID","N",10,0}, {"MEMO","M",10,0}}, ,,,,,{"DBFMEMO"})
    ? DBUSEAREA(TRUE,,"Test",,,,,,{"DBFMEMO"})
    ? DbCreateIndex("test", "ID")
    FOR VAR i := 1 to 10
        DBAPPEND()
        FIELDPUT(1,i)
        FIELDPUT(2,"Some text for the memo "+Strzero(i,10))
        DBCOMMIT()
    NEXT
    DbCloseArea()
 ? DBUSEAREA(TRUE,,"Test",,,,,,{"DBFMEMO"})
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

