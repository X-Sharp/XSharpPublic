//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit

#pragma warnings(165, off)  // unassigned variables

// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.RT.Tests

    CLASS DBFBlobtests

        [Trait("Category", "Blob")];
            [Fact];
            METHOD BlobDirectTest() AS VOID

            LOCAL cPath AS STRING
            LOCAL aStruct AS ARRAY
            LOCAL nRecSize, nBlockNo AS DWORD
            LOCAL pRecord, pField AS BYTE PTR
            //RDDINFO(_SET_MEMOEXT, ".DBF.FPT.U")
            cPath := ""
            aStruct := { { "DATA", "M", 10, 0, "DATA" } }
            DBCREATE(cPath + "MEMOTEST.DBF", aStruct, "DBFCDX")

            DBUSEAREA(TRUE, "DBFCDX", cPath + "MEMOTEST.DBF", "MEMOTEST", TRUE, FALSE)

            nRecSize := DbInfo(DBI_GETRECSIZE)
            pRecord := MemAlloc(nRecSize)
            pRecord[1] := 32

            // record 1
            pField := pRecord + 1
            MemoWrit("test.txt", "record 1")
            nBlockNo := BLOBDirectImport(0, "test.txt")
            nBlockNo := BLOBDirectPut(nBlockNo, "record 1")
            MemCopyString(pField, Str(nBlockNo, 10, 0), 10)
            DBAPPEND()
            VODBRecordPut(pRecord)
            Assert.True(BLOBDirectGet(nBlockNo,1,100) =="record 1")
            BlobDirectExport(nBlockNo, "test.txt", BLOB_EXPORT_OVERWRITE)
            Assert.True(MemoRead("test.txt") == "record 1")

            // record 2 (several blocks)
            pField := pRecord + 1
            nBlockNo := BLOBDirectPut(0, Replicate("a", 100))
            MemCopyString(pField, Str(nBlockNo, 10, 0), 10)
            DBAPPEND()
            VODBRecordPut(pRecord)

            Assert.True(BLOBDirectGet(nBlockNo,1,50) ==Replicate("a", 50))
            Assert.True(BLOBDirectGet(nBlockNo,-50,50) ==Replicate("a", 50))
            BlobDirectExport(nBlockNo, "test.txt", BLOB_EXPORT_OVERWRITE)
            Assert.True(MemoRead("test.txt") == Replicate("a", 100))
            // record 3
            pField := pRecord + 1
            nBlockNo := BLOBDirectPut(0, "record 3")
            MemCopyString(pField, Str(nBlockNo, 10, 0), 10)
            DBAPPEND()
            VODBRecordPut(pRecord)
            BlobDirectExport(nBlockNo, "test.txt", BLOB_EXPORT_OVERWRITE)
            Assert.True(MemoRead("test.txt") == "record 3")

            VODBCommit()
            VODBCloseArea()
            MemFree(pRecord)

            // TEST
            Assert.True( DBUSEAREA(TRUE, "DBFCDX", cPath + "MEMOTEST.DBF", "MEMOTEST", FALSE, FALSE) )
            VODBGoTop()
            Assert.True( !VODBEof() )
            Assert.True( FieldGetSym(#DATA) == "record 1" )
            VODBSkip(1)
            Assert.True( !VODBEof() )
            Assert.True( FieldGetSym(#DATA) == Replicate("a", 100) )
            VODBSkip(1)
            Assert.True( !VODBEof() )
            Assert.True( FieldGetSym(#DATA) == "record 3" )
            VODBSkip(1)
            Assert.True( VODBEof() )
        VODBCloseArea()

        [Trait("Category", "Blob")];
            [Fact];
            METHOD BlobRootTest() AS VOID
            LOCAL cPath AS STRING
            LOCAL aStruct AS ARRAY
            //LOCAL cMemo := "test.txt" as STRING
            LOCAL cContents := "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ" AS STRING
            //LOCAL cMemoContents := "The quick brown fox jumps over the lazy dog" AS STRING
            //RDDINFO(_SET_MEMOEXT, ".DBF.FPT.U")
            cPath := ""
            aStruct := { { "DATA", "M", 10, 0, "DATA" } }
            DBCREATE(cPath + "MEMOTEST.DBF", aStruct, "DBFCDX")

            DBUSEAREA(TRUE, "DBFCDX", cPath + "MEMOTEST.DBF", "MEMOTEST", TRUE, FALSE)
            Assert.True( BlobRootLock())
            Assert.True( BlobRootPut(cContents))
            Assert.True( BlobRootUnLock())
            Assert.True( BlobRootGet() == cContents)


    END CLASS
END NAMESPACE // XSharp.Runtime.Tests
