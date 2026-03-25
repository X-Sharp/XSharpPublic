//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that this code code expects zero based arrays

USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.RDD
// Inspired by Harbour
/// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking/*" />
STRUCTURE DbfLocking
 /// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking.Offset/*" />
	PUBLIC Offset AS INT64
 /// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking.FileSize/*" />
	PUBLIC FileSize AS INT64
 /// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking.RecordSize/*" />
	PUBLIC RecordSize AS LONG
 /// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking.Direction/*" />
	PUBLIC Direction AS LONG

/// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking.Initialize/*" />
METHOD Initialize( model AS DbfLockingModel ) AS VOID
	SWITCH model
	CASE DbfLockingModel.Clipper52
		SELF:Offset     := 1000000000
		SELF:FileSize   := 1000000000
		SELF:RecordSize := 1
		SELF:Direction  := 1
	CASE DbfLockingModel.Clipper53
		SELF:Offset     := 1000000000
		SELF:FileSize   := 1000000000
		SELF:RecordSize := 1
		SELF:Direction  := 1
	CASE DbfLockingModel.Clipper53Ext
		SELF:Offset     := 4000000000
		SELF:FileSize   := 294967295
		SELF:RecordSize := 1
		SELF:Direction  := 1
	CASE DbfLockingModel.FoxPro
		SELF:Offset     := 0x40000000
		SELF:FileSize   := 0x07ffffff
		SELF:RecordSize := 1
		SELF:Direction  := 2
	CASE DbfLockingModel.FoxProExt
		SELF:Offset     := 0x7ffffffe
		SELF:FileSize   := 0x3ffffffd
		SELF:RecordSize := 1
		SELF:Direction  := -1
	CASE DbfLockingModel.Harbour64
		SELF:Offset     := 0x7FFFFFFF00000001
		SELF:FileSize   := 0x7ffffffe
		SELF:RecordSize := 1
		SELF:Direction  := 1
	CASE DbfLockingModel.VoAnsi
		SELF:Offset     := 0x80000000
		SELF:FileSize   := 0x7fffffff
		SELF:RecordSize := 1
		SELF:Direction  := 1
	END SWITCH
    /// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking.FileLockOffSet/*" />
    PROPERTY FileLockOffSet AS INT64
        GET
            VAR iOffset := SELF:Offset
	        IF SELF:Direction < 0
		        iOffset -= SELF:FileSize
	        ELSE
		        iOffset++
            ENDIF
            RETURN iOffset
        END GET
    END PROPERTY
    /// <include file="XSharp.RDD.Docs.xml" path="doc/DbfLocking.RecnoOffSet/*" />
    METHOD RecnoOffSet(recordNbr AS DWORD, recSize AS LONG, headerLength AS LONG) AS INT64
	    VAR iOffset := SELF:Offset
	    IF SELF:Direction < 0
		    iOffset -= (INT64)recordNbr
	    ELSEIF( SELF:Direction == 2 )
		    iOffset += (INT64)( ( recordNbr - 1 ) * recSize + headerLength )
	    ELSE
		    iOffset += (INT64) recordNbr
	    ENDIF
        RETURN iOffset
END STRUCTURE


END NAMESPACE
