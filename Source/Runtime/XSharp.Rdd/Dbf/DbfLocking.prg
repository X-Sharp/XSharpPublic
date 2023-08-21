//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that this code code expects zero based arrays

USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.RDD
// Inspired by Harbour
/// <summary>This structure holds the various settings for locking models</summary>
STRUCTURE DbfLocking
    /// <summary>Offset of the Locking </summary>
	PUBLIC Offset AS INT64
    /// <summary>Length for File locks </summary>
	PUBLIC FileSize AS INT64
    /// <summary>Length for Record locks </summary>
	PUBLIC RecordSize AS LONG
    /// <summary>Direction of locking, used to calculate file lock offsets and record lock offsets</summary>
	PUBLIC Direction AS LONG

/// <summary>Set various numbers based on a locking model.</summary>	
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
    /// <summary>File Lock offsets </summary>
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
    /// <summary>Calculate the record offset based </summary>
    METHOD RecnoOffSet(recordNbr AS LONG, recSize AS LONG, headerLength AS LONG) AS INT64
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
