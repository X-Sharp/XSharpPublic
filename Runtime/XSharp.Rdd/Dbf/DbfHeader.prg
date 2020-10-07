//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that this code code expects zero based arrays
#pragma options ("az", ON)

USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.RDD

/// <summary>DBF Header.</summary>
CLASS DbfHeader
    // Fixed Buffer of 32 bytes
    // Matches the DBF layout
    // Read/Write to/from the Stream with the Buffer
    // and access individual values using the other fields
	
	PRIVATE CONST OFFSET_SIG			 := 0  AS BYTE
	PRIVATE CONST OFFSET_YEAR			 := 1  AS BYTE           // add 1900 so possible values are 1900 - 2155
	PRIVATE CONST OFFSET_MONTH	         := 2  AS BYTE
	PRIVATE CONST OFFSET_DAY             := 3  AS BYTE
	PRIVATE CONST OFFSET_RECCOUNT        := 4  AS BYTE
	PRIVATE CONST OFFSET_DATAOFFSET      := 8  AS BYTE
	PRIVATE CONST OFFSET_RECSIZE         := 10 AS BYTE
	PRIVATE CONST OFFSET_RESERVED1       := 12 AS BYTE
	PRIVATE CONST OFFSET_TRANSACTION     := 14 AS BYTE
	PRIVATE CONST OFFSET_ENCRYPTED       := 15 AS BYTE
	PRIVATE CONST OFFSET_DBASELAN        := 16 AS BYTE
	PRIVATE CONST OFFSET_MULTIUSER       := 20 AS BYTE
	PRIVATE CONST OFFSET_RESERVED2       := 24 AS BYTE
	PRIVATE CONST OFFSET_HASTAGS	     := 28 AS BYTE
	PRIVATE CONST OFFSET_CODEPAGE        := 29 AS BYTE
	PRIVATE CONST OFFSET_RESERVED3       := 30 AS BYTE
	INTERNAL CONST SIZE                  := 32 AS BYTE
	
	PRIVATE Buffer   AS BYTE[]
    PRIVATE _oRDD AS DBF

    /// <summary>Hot ?  => Header has changed ?</summary>
	INTERNAL isHot	AS LOGIC
	/// <summary>DBF Version</summary>

    CONSTRUCTOR (oRDD AS DBF)
        SELF:_oRDD := oRDD
	    Buffer := BYTE[]{DbfHeader.SIZE}
	    isHot  := FALSE
        RETURN

PROPERTY Version    AS DBFVersion	;
    GET (DBFVersion) Buffer[OFFSET_SIG] ;
    SET Buffer[OFFSET_SIG] := (BYTE) VALUE,isHot := TRUE
	
/// <summary>Year of last update
/// YY is added to a base of 1900 decimal to determine the actual year.
/// Therefore, YY has possible values from 0x00-0xFF, which allows for a range from 1900-2155.</summary>	
PROPERTY Year		AS LONG			
	GET
		LOCAL nYear AS LONG
		nYear := DateTime.Now:Year
		nYear := nYear - (nYear % 100)  // Get century
		RETURN Buffer[OFFSET_YEAR] + nYear
	END GET
	SET
		Buffer[OFFSET_YEAR] := (BYTE) (VALUE% 100)
		isHot := TRUE
	END SET
END PROPERTY

/// <summary>Month of last update</summary>
PROPERTY Month		AS BYTE			;
    GET Buffer[OFFSET_MONTH]	;
    SET Buffer[OFFSET_MONTH] := VALUE, isHot := TRUE
        
/// <summary>Day of last update</summary>	
PROPERTY Day		AS BYTE			;
    GET Buffer[OFFSET_DAY]	;
    SET Buffer[OFFSET_DAY] := VALUE, isHot := TRUE

/// <summary>Number of records in the table. (Least significant byte first.)</summary>
PROPERTY RecCount	AS LONG			;
    GET BitConverter.ToInt32(Buffer, OFFSET_RECCOUNT) ;
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RECCOUNT, SIZEOF(LONG)), isHot := TRUE

/// <summary>Number of bytes in the header. (Least significant byte first.)</summary>	
PROPERTY HeaderLen	AS SHORT		;
    GET BitConverter.ToInt16(Buffer, OFFSET_DATAOFFSET);
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_DATAOFFSET, SIZEOF(SHORT)), isHot := TRUE
	
/// <summary>Length of one data record, including deleted flag</summary>	
PROPERTY RecordLen	AS WORD		;
    GET BitConverter.ToUInt16(Buffer, OFFSET_RECSIZE);
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RECSIZE, SIZEOF(WORD)), isHot := TRUE
	
/// <summary>Reserved flag at position 12</summary>	
PROPERTY Reserved1	AS SHORT		;
    GET BitConverter.ToInt16(Buffer, OFFSET_RESERVED1);
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RESERVED1, SIZEOF(SHORT)), isHot := TRUE
	
/// <summary>Flag indicating incomplete dBASE IV transaction.</summary>
PROPERTY Transaction AS BYTE		;
    GET Buffer[OFFSET_TRANSACTION];
    SET Buffer[OFFSET_TRANSACTION] := VALUE, isHot := TRUE
	
/// <summary>dBASE IV encryption flag.</summary>
PROPERTY Encrypted	AS BYTE			;
    GET Buffer[OFFSET_ENCRYPTED];
    SET Buffer[OFFSET_ENCRYPTED] := VALUE, isHot := TRUE

/// <summary>dBASE IV LAN value.</summary>	
PROPERTY DbaseLan	AS LONG			;
    GET BitConverter.ToInt32(Buffer, OFFSET_DBASELAN) ;
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_DBASELAN, SIZEOF(LONG)), isHot := TRUE

/// <summary>dBASE IV Multi User value.</summary>	
PROPERTY MultiUser	AS LONG			;
    GET BitConverter.ToInt32(Buffer, OFFSET_MULTIUSER)	;
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_MULTIUSER, SIZEOF(LONG)), isHot := TRUE

/// <summary>Reserved flag at position 24</summary>	
PROPERTY Reserved2	AS LONG			;
    GET BitConverter.ToInt32(Buffer, OFFSET_RESERVED2);
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RESERVED2, SIZEOF(LONG))


/// <summary>Table Flags</summary>	
PROPERTY TableFlags	AS DBFTableFlags ;
    GET (DBFTableFlags)Buffer[OFFSET_HASTAGS] ;
    SET Buffer[OFFSET_HASTAGS] := (BYTE) VALUE, isHot := TRUE
        
/// <summary>Code Page</summary>	
PROPERTY CodePage	AS DbfHeaderCodepage			 ;
    GET (DbfHeaderCodepage) Buffer[OFFSET_CODEPAGE]  ;
    SET Buffer[OFFSET_CODEPAGE] := (BYTE) VALUE, isHot := TRUE
        
/// <summary>Reserved flag at position 30</summary>	
PROPERTY Reserved3	AS SHORT         ;
    GET BitConverter.ToInt16(Buffer, OFFSET_RESERVED3);
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_RESERVED3, SIZEOF(SHORT)), isHot := TRUE
	
/// <summary>Date of last update.</summary>
PROPERTY LastUpdate AS DateTime      ;  
    GET DateTime{Year, Month, Day} ;

/// <summary>Is the codepage an Ansi codepage</summary>	
PROPERTY IsAnsi AS LOGIC GET CodePage:IsAnsi()

PROPERTY FieldCount AS LONG GET  (SELF:HeaderLen - DbfHeader.SIZE) / DbfField.SIZE  

    // Dbase (7?) Extends this with
    // [FieldOffSet(31)] PUBLIC LanguageDriverName[32]	 as BYTE
    // [FieldOffSet(63)] PUBLIC Reserved6 AS LONG
    /*
    0x02   FoxBASE
    0x03   FoxBASE+/Dbase III plus, no memo
    0x04   dBase 4
    0x05   dBase 5
    0x07   VO/Vulcan Ansi encoding
    0x13   FLagship dbv
    0x23   Flagship 2/4/8
    0x30   Visual FoxPro
    0x31   Visual FoxPro, autoincrement enabled
    0x33   Flagship 2/4/8 + dbv
    0x43   dBASE IV SQL table files, no memo
    0x63   dBASE IV SQL system files, no memo
    0x7B   dBASE IV, with memo
    0x83   FoxBASE+/dBASE III PLUS, with memo
    0x87   VO/Vulcan Ansi encoding with memo
    0x8B   dBASE IV with memo
    0xCB   dBASE IV SQL table files, with memo
    0xE5   Clipper SIX driver, with SMT memo
    0xF5   FoxPro 2.x (or earlier) with memo
    0xFB   FoxBASE
    
    FoxPro additional Table structure:
    28 	Table flags:
    0x01   file has a structural .cdx
    0x02   file has a Memo field
    0x04   file is a database (.dbc)
    This byte can contain the sum of any of the above values.
    For example, the value 0x03 indicates the table has a structural .cdx and a
    Memo field.
    29 	Code page mark
    30 ? 31 	Reserved, contains 0x00
    32 ? n 	Field subrecords
    The number of fields determines the number of field subrecords.
    One field subrecord exists for each field in the table.
    n+1 			Header record terminator (0x0D)
    n+2 to n+264 	A 263-byte range that contains the backlink, which is the
    relative path of an associated database (.dbc) file, information.
    If the first byte is 0x00, the file is not associated with a database.
    Therefore, database files always contain 0x00.
    see also ftp://fship.com/pub/multisoft/flagship/docu/dbfspecs.txt
    
    */

METHOD Read() AS LOGIC
    VAR oStream := _oRDD:Stream
    VAR nPos := oStream:Position
    VAR Ok := oStream:SafeSetPos(0) .AND. oStream:SafeRead(SELF:Buffer) .AND. oStream:SafeSetPos(nPos)    
    RETURN Ok

METHOD Write() AS LOGIC
	LOCAL dtInfo AS DateTime
	dtInfo := DateTime.Now
    // Update the Date/Time information
	SELF:Year   := (BYTE)(dtInfo:Year % 100)
	SELF:Month  := (BYTE)dtInfo:Month
	SELF:Day    := (BYTE)dtInfo:Day

    VAR oStream := _oRDD:Stream
    VAR nPos := oStream:Position
    VAR Ok  := oStream:SafeSetPos(0) .AND. oStream:SafeWrite(SELF:Buffer) .AND. oStream:SafeSetPos(nPos)    
    IF Ok
        SELF:isHot := FALSE
    ENDIF
    RETURN Ok


END CLASS

END NAMESPACE
