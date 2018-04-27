//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.IO
USING System.Text
USING System.Linq


BEGIN NAMESPACE XSharp.RDD
    CLASS DBF INHERIT Workarea  
        PROTECT _Header			AS DbfHeader    
        PROTECT _HeaderLength	AS WORD  	// Size of header 
        PROTECT _BufferValid	AS LOGIC	// Current Record is Valid
        PROTECT _HasMemo		AS LOGIC
        PROTECT _HasTags		AS LOGIC
        PROTECT _HasAutoInc		AS LOGIC 
        PROTECT _HasTimeStamp	AS LOGIC
        PROTECT _LastUpdate	    AS DateTime
        PROTECT _RecCount		AS LONG		
        PROTECT _RecNo			AS LONG
        //PROTECT _Temporary		AS LOGIC    
        //PROTECT _NullCount		AS LONG
        //PROTECT _NullOffSet		AS LONG
        PROTECT _RecordChanged	AS LOGIC 	// Current record has changed ?
        PROTECT _Positioned		AS LOGIC 	// 
        PROTECT _Appended		AS LOGIC	// Record has been added ?
        PROTECT _Deleted		AS LOGIC	// Record has been deleted ?
        PROTECT _HeaderDirty	AS LOGIC	// Header is dirty ?
        PROTECT _fLocked		AS LOGIC
        PROTECT _HeaderLocked	AS LOGIC
        PROTECT _PackMemo		AS LOGIC
        PROTECT _OpenInfo		AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method
        PROTECT _ParentRelInfo	AS DbRelInfo  // parent rel struct   
        PROTECT _Locks			AS LONG[]
        //PROTECT _DirtyRead		AS LONG
        //PROTECT _HasTrigger		AS LOGIC
        //PROTECT _Encrypted		AS LOGIC	// Current record Encrypted
        //PROTECT _TableEncrypted 	AS LOGIC	// Whole table encrypted
        //PROTECT _CryptKey		AS STRING       
        //PROTRECT _Trigger		as DbTriggerDelegate  
        PROTECT _oIndex			AS BaseIndex
        PROTECT _oMemo			AS BaseMemo
        
        CONSTRUCTOR()
            SELF:_Header := DbfHeader{}
            SELF:_Header:initialize()
            
            
            
            //	METHOD DbEval(info AS DbEvalInfo) AS LOGIC
         METHOD GoTop() AS LOGIC
            IF ( SELF:_hFile != F_ERROR )
                SELF:GoTo( 1 )
                RETURN TRUE
            ENDIF
            RETURN FALSE

         METHOD GoBottom() AS LOGIC
            IF ( SELF:_hFile != F_ERROR )
                SELF:Goto( SELF:RecCount )
            ENDIF
            RETURN FALSE

         METHOD GoTo(nRec AS LONG) AS LOGIC
            IF ( SELF:_hFile != F_ERROR )
                IF ( nRec <= SELF:RecCount ) .AND. ( nRec > 0 )
                    // virtual pos
                    Self:_RecNo := nRec
                ENDIF
            ENDIF
            RETURN FALSE

            //	METHOD GoToId(oRec AS OBJECT) AS LOGIC
            //	METHOD Skip(nToSkip AS INT) AS LOGIC
            //	METHOD SkipFilter(nToSkip AS INT) AS LOGIC
            //	METHOD SkipRaw(nToSkip AS INT) AS LOGIC 
            //	METHOD SkipScope(nToSkip AS INT) AS LOGIC
            
            // Append and Delete
            //	METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
            //	METHOD Delete() AS LOGIC   
            //	METHOD GetRec() AS BYTE[]  
            //	METHOD Pack() AS LOGIC
            //	METHOD PutRec(aRec AS BYTE[]) AS LOGIC 
            //	METHOD Recall() AS LOGIC
            //	METHOD Zap() AS LOGIC   
            
            // Open and Close   
        VIRTUAL METHOD Close() 			AS LOGIC 
            IF ( SELF:_hFile != NULL )
                VAR isOk := TRUE
                //
                isOk := FClose( SELF:_hFile )
                IF ( isOk )
                    TRY
                        SUPER:Close()
                        CATCH
                        isOk := FALSE
                    END TRY
                    SELF:_hFile := NULL
                ENDIF
                RETURN isOk
            ENDIF
            RETURN FALSE

            //	METHOD Create(info AS DbOpenInfo) AS LOGIC  

        VIRTUAL METHOD Open(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
            LOCAL isOK AS LOGIC
            //
            isOk := FALSE
            SELF:_OpenInfo := info
            // Should we set to .DBF per default ?
            IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
                SELF:_OpenInfo:Extension := ".DBF"
                //
                SELF:_OpenInfo:FileName := System.IO.Path.ChangeExtension( SELF:_OpenInfo:FileName, SELF:_OpenInfo:Extension )
            ENDIF
            //
            SELF:_FileName := SELF:_OpenInfo:FileName
            SELF:_Shared := SELF:_OpenInfo:Shared
            SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
            SELF:_hFile    := Fopen(SELF:_FileName, SELF:_OpenInfo:FileMode) 
            IF ( SELF:_hFile != F_ERROR )
                isOk := SELF:_fillHeader()
                IF ( isOk )
                    SELF:GoTop()
                ENDIF
            ENDIF
            RETURN isOk
            
        PRIVATE METHOD _fillHeader() AS LOGIC
            LOCAL isOk AS LOGIC
            //
            isOk := ( FRead3(SELF:_hFile, SELF:_Header:Buffer, HDROFFSETS.SIZE) == HDROFFSETS.SIZE )
            //
            IF ( isOk )
                LOCAL fieldCount := (( SELF:_Header:HeaderLen - HDROFFSETS.SIZE) / FLDOFFSETS.SIZE ) AS INT
                LOCAL fieldDefSize := fieldCount * FLDOFFSETS.SIZE AS INT
                // Something wrong in Size...
                IF ( fieldCount <= 0 )
                    RETURN FALSE
                ENDIF
                // Move to top, after header
                isOk := ( FSeek3( SELF:_hFile, HDROFFSETS.SIZE, SeekOrigin.Begin ) == HDROFFSETS.SIZE )
                IF ( isOk )
                    // Read full Fields Header
                    VAR fieldsBuffer := BYTE[]{ fieldDefSize } 
                    isOk := ( FRead3( SELF:_hFile, fieldsBuffer, (DWORD)fieldDefSize ) == (DWORD)fieldDefSize )
                    IF ( isOk )
                        VAR currentField := DbfField{}
                        currentField:Initialize()
                        // Now, process
                        SELF:_Fields := RddFieldInfo[]{ fieldCount }
                        FOR VAR i := 0 TO (fieldCount-1)
                            //
                            Array.Copy( fieldsBuffer, i*FLDOFFSETS.SIZE, currentField:Buffer, 0, FLDOFFSETS.SIZE )
                            SELF:_Fields[ i ] := RddFieldInfo{ currentField:Name, currentField:Type, currentField:Len, currentField:Dec }
                        NEXT
                        //
                    ENDIF
                ENDIF
            ENDIF
            RETURN isOk
            
            // Filtering and Scoping 
            //	METHOD ClearFilter() 	AS LOGIC
            //	METHOD ClearScope() 	AS LOGIC 
            //	METHOD Continue()		AS LOGIC     
            //	METHOD GetScope()		AS DbScopeInfo 
            //	METHOD ScopeInfo(nOrdinal AS LONG) AS OBJECT
            //	METHOD SetFilter(info AS DbFilterInfo) AS LOGIC 
            //	METHOD SetScope(info AS DbScopeInfo) AS LOGIC
            
            // Fields
            //METHOD CreateFields(aFields AS DbField[]) AS LOGIC
            //	METHOD FieldIndex(fieldName AS STRING) AS LONG 


        VIRTUAL METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult AS OBJECT
            SWITCH nOrdinal
                CASE DbFieldInfo.DBS_NAME
                    oResult := SELF:_Fields[nFldPos]:Name
                CASE DbFieldInfo.DBS_LEN
                    oResult := SELF:_Fields[nFldPos]:Length
                CASE DbFieldInfo.DBS_DEC
                    oResult := SELF:_Fields[nFldPos]:Decimals
                CASE DbFieldInfo.DBS_TYPE
                    oResult := SELF:_Fields[nFldPos]:FieldType
                CASE DbFieldInfo.DBS_ALIAS
                    oResult := SELF:_Fields[nFldPos]:Alias

                CASE DbFieldInfo.DBS_ISNULL
                CASE DbFieldInfo.DBS_COUNTER
                CASE DbFieldInfo.DBS_STEP    

                CASE DbFieldInfo.DBS_BLOB_GET     
                CASE DbFieldInfo.DBS_BLOB_TYPE	// Returns the data type of a BLOB (memo) field. This
                                                // is more efficient than using Type() or ValType()
                                                // since the data itself does not have to be retrieved
                                                // from the BLOB file in order to determine the type.
                CASE DbFieldInfo.DBS_BLOB_LEN	    // Returns the storage length of the data in a BLOB (memo) file	
                CASE DbFieldInfo.DBS_BLOB_OFFSET	// Returns the file offset of the data in a BLOB (memo) file.
                CASE DbFieldInfo.DBS_BLOB_POINTER	// Returns a numeric pointer to the data in a blob
                                                    // file. This pointer can be used with BLOBDirectGet(),
                                                    // BLOBDirectImport(), etc.

                CASE DbFieldInfo.DBS_BLOB_DIRECT_TYPE		
                CASE DbFieldInfo.DBS_BLOB_DIRECT_LEN		

                CASE DbFieldInfo.DBS_STRUCT				
                CASE DbFieldInfo.DBS_PROPERTIES			
                CASE DbFieldInfo.DBS_USER		

                OTHERWISE
                    oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult

        METHOD FieldName(nFldPos AS LONG) AS STRING
            RETURN SUPER:FieldName( nFldPos )
            
            // Read & Write		
        METHOD GetValue(nFldPos AS LONG) AS OBJECT
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:GetValue(nFldPos)
            ELSE                            
                RETURN SUPER:GetValue(nFldPos)
            ENDIF
            
        METHOD GetValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:GetValueFile(nFldPos, fileName)
            ELSE                            
                RETURN SUPER:GetValueFile(nFldPos, fileName)
            ENDIF
            
        METHOD GetValueLength(nFldPos AS LONG) AS LONG
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:GetValueLength(nFldPos)
            ELSE                            
                RETURN SUPER:GetValueLength(nFldPos)
            ENDIF
        METHOD Flush() 			AS LOGIC
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:Flush()
            ELSE                            
                RETURN SUPER:Flush()
            ENDIF
            //	METHOD GoCold()			AS LOGIC
            //	METHOD GoHot()			AS LOGIC   
        METHOD PutValue(nFldPos AS LONG, oValue AS OBJECT) AS LOGIC
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:PutValue(nFldPos, oValue)
            ELSE                            
                RETURN SUPER:PutValue(nFldPos, oValue)
            ENDIF
            
        METHOD PutValueFile(nFldPos AS LONG, fileName AS STRING) AS LOGIC
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:PutValueFile(nFldPos, fileName)
            ELSE                            
                RETURN SUPER:PutValue(nFldPos, fileName)
            ENDIF
            
            
            // Locking
            //	METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC  
            //	METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
            //	METHOD Lock(uiMode AS DbLockMode) AS LOGIC 
            //	METHOD UnLock(oRecId AS OBJECT) AS LOGIC
            
            // Memo File Access 
        METHOD CloseMemFile() 	AS LOGIC    
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:CloseMemFile()
            ELSE                            
                RETURN SUPER:CloseMemFile()
            ENDIF
        METHOD CreateMemFile(info AS DbOpenInfo) 	AS LOGIC
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:CreateMemFile(info)
            ELSE                            
                RETURN SUPER:CreateMemFile(info)
            ENDIF
            
        METHOD OpenMemFile() 	AS LOGIC   
            IF _oMemo != NULL_OBJECT                    
                RETURN _oMemo:OpenMemFile()
            ELSE                            
                RETURN SUPER:OpenMemFile()
            ENDIF
            
            // Indexes
        METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderCondition(info)
            ELSE
                RETURN SUPER:OrderCondition(info)
            ENDIF
        METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC	
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderCreate(info)
            ELSE
                RETURN SUPER:OrderCreate(info)
            ENDIF
            
        METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC    	
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderDestroy(info)
            ELSE
                RETURN SUPER:OrderDestroy(info)
            ENDIF
        METHOD OrderInfo(nOrdinal AS LONG) AS OBJECT
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderInfo(nOrdinal)
            ELSE
                RETURN SUPER:OrderInfo(nOrdinal)
            ENDIF
        METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderListAdd(info)
            ELSE
                RETURN SUPER:OrderListAdd(info)
            ENDIF
        METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderListDelete(info)
            ELSE
                RETURN SUPER:OrderListDelete(info)
            ENDIF
        METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderListFocus(info)
            ELSE
                RETURN SUPER:OrderListFocus(info)
            ENDIF
        METHOD OrderListRebuild() AS LOGIC 
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:OrderListRebuild()
            ELSE
                RETURN SUPER:OrderListRebuild()
            ENDIF
        METHOD Seek(info AS DbSeekInfo) AS LOGIC
            IF _oIndex != NULL_OBJECT
                RETURN _oIndex:Seek(info)
            ELSE
                RETURN SUPER:Seek(info)
            ENDIF
            
            // Relations
            //	METHOD ChildEnd(info AS DbRelInfo) AS LOGIC
            //	METHOD ChildStart(info AS DbRelInfo) AS LOGIC
            //	METHOD ChildSync(info AS DbRelInfo) AS LOGIC
            //	METHOD ClearRel() AS LOGIC
            //	METHOD ForceRel() AS LOGIC  
            //	METHOD RelArea(nRelNum AS LONG) AS LONG 
            //	METHOD RelEval(info AS DbRelInfo) AS LOGIC
            //	METHOD RelText(nRelNum AS LONG) AS STRING
            //	METHOD SetRel(info AS DbRelInfo) AS LOGIC  
            //	METHOD SyncChildren() AS LOGIC
            
            // Trans	
            //    METHOD Trans(info AS DbTransInfo) 		AS LOGIC
            //    METHOD TransRec(info AS DbTransInfo) 	AS LOGIC
            
            // Blob
            //	METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
            
            // CodeBlock Support
            //	METHOD Compile(sBlock AS STRING) AS LOGIC
            //	METHOD EvalBlock(oBlock AS OBJECT) AS OBJECT	
            
            // Other
        VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            LOCAL oResult AS OBJECT
            SWITCH nOrdinal
            CASE DbInfo.DBI_ISDBF
                CASE DbInfo.DBI_CANPUTREC
                    oResult := TRUE		
                CASE DbInfo.DBI_LASTUPDATE
                    oResult := SELF:_Header:LastUpdate
                CASE DbInfo.DBI_GETHEADERSIZE
                    oResult := SELF:_Header:HeaderLen 
                    // DbInfo.GETLOCKARRAY
                    // DbInfo.TABLEEXT
                    // DbInfo.FULLPATH
                    // DbInfo.MEMOTYPE 
                    // DbInfo.TABLETYPE
                    // DbInfo.FILEHANDLE
                    // DbInfo.MEMOHANDLE
                    // DbInfo.TRANSREC
                    // DbInfo.SHARED
                    // DbInfo.ISFLOCK
                    // DbInfo.VALIDBUFFER 
                    // DbInfo.POSITIONED 
                    // DbInfo.ISENCRYPTED
                    // DbInfo.DECRYPT
                    // DbInfo.ENCRYPT
                    // DbInfo.LOCKCOUNT 
                    // DbInfo.LOCKOFFSET
                    // DbInfo.LOCKTEST
                    // DbInfo.LOCKSCHEME
                    // DbInfo.ROLLBACK
                    // DbInfo.PASSWORD
                    // DbInfo.TRIGGER
                    // DbInfo.OPENINFO
                    // DbInfo.DIRTYREAD
                    // DbInfo.DB_VERSION
                    // DbInfo.RDD_VERSION
                    // DbInfo.CODEPAGE
                    // DbInfo.DOS_CODEPAGE
                    
                OTHERWISE
                    oResult := SUPER:Info(nOrdinal, oNewValue)
                END SWITCH
            RETURN oResult  
            
            
            
            
        VIRTUAL METHOD RecInfo(oRecID AS OBJECT, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT  
            LOCAL oResult AS OBJECT
            LOCAL nCurrent := 0 AS LONG
            // if oRecID is empty
            // then set it to the current record
            // if oRecID != Current Record 
            // then save current record and move to new record
            // but only for the DBRI values that work on the current record:         
            //case DBRI_DELETED:
            
            //case DBRI_ENCRYPTED:
            
            //case DBRI_RAWRECORD:
            
            //case DBRI_RAWMEMOS:
            
            //case DBRI_RAWDATA: 
            // 
            // then return FALSE for the logical methods
            SWITCH nOrdinal
                // CASE DBRI_DELETED
                // CASE DBRI_ENCRYPTED
                // CASE DBRI_RAWRECORD
                // CASE DBRI_RAWMEMOS
                // CASE DBRI_RAWDATA 
                // DBRI_LOCKED
                // DBRI_RECNO
                // DBRI_RECSIZE 
                // DBRI_ENCRYPTED
                // 
                OTHERWISE
                    oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH            
            IF nCurrent != 0
                SELF:Goto(nCurrent)
            ENDIF
            RETURN oResult
            
            //	METHOD Sort(info AS DbSortInfo) AS LOGIC
            
            // Properties
            //	PROPERTY Alias 		AS STRING GET
            //	PROPERTY BoF 		AS LOGIC GET
            //	PROPERTY Deleted 	AS LOGIC GET
            //	PROPERTY EoF 		AS LOGIC GET
            //	PROPERTY Exclusive	AS LOGIC GET
            //	PROPERTY FieldCount AS LONG GET 
            //	PROPERTY FilterText	AS STRING GET 
            //	PROPERTY Found		AS LOGIC GET 
        PROPERTY RecCount	AS LONG 
            GET
                IF ( SELF:_hFile != F_ERROR )
                    VAR current := FTell( SELF:_hFile )
                    VAR fSize := FSeek3( SELF:_hFile, 0, FS_END )
                    FSeek3( SELF:_hFile, (LONG)current, FS_SET )
                    RETURN ( fSize - SELF:_Header:HeaderLen ) / SELF:_Header:RecordLen
                ENDIF
                // -1 ??
                RETURN 0
            END GET
        END PROPERTY

            //	PROPERTY RecNo		AS OBJECT GET 
            //	PROPERTY Shared		AS LOGIC GET
        VIRTUAL PROPERTY SysName AS STRING GET TYPEOF(Dbf):ToString()
        
        //	
        // Error Handling
        //	PROPERTY LastGenCode	AS LONG GET
        //	PROPERTY LastSubCode	AS LONG GET
        //	PROPERTY LastError		AS Exception GET
        PUBLIC ENUM HDROFFSETS
            MEMBER SIG			:= 0
            MEMBER YEAR			:= 1
            MEMBER MONTH	    := 2
            MEMBER DAY          := 3
            MEMBER RECCOUNT     := 4
            MEMBER DATAOFFSET   := 8
            MEMBER RECSIZE      := 10
            MEMBER RESERVED1    := 12
            MEMBER TRANSACTION  := 14
            MEMBER ENCRYPTED    := 15
            MEMBER DBASELAN     := 16
            MEMBER MULTIUSER    := 20
            MEMBER RESERVED2   := 24
            MEMBER HASTAGS	    := 28
            MEMBER CODEPAGE     := 29
            MEMBER RESERVED3    := 30
            MEMBER SIZE         := 32
            
        END ENUM
        
        PUBLIC ENUM FLDOFFSETS
            MEMBER NAME			:= 0
            MEMBER NAME_SIZE    := 11
            MEMBER TYPE			:= 11
            MEMBER OFFSET	    := 12
            MEMBER LEN          := 16
            MEMBER DEC          := 17
            MEMBER FLAGS        := 18
            MEMBER COUNTER      := 19
            MEMBER INCSTEP      := 23
            MEMBER RESERVED1    := 24
            MEMBER RESERVED2    := 25
            MEMBER RESERVED3    := 26
            MEMBER RESERVED4    := 27
            MEMBER RESERVED5    := 28
            MEMBER RESERVED6	:= 29
            MEMBER RESERVED7    := 30
            MEMBER HASTAG       := 31
            MEMBER SIZE         := 32
        END ENUM
        
        
        STRUCTURE DbfHeader                     
            // Fixed Buffer of 32 bytes
            // Matches the DBF layout  
            // Read/Write to/from the Stream with the Buffer 
            // and access individual values using the other fields
            PUBLIC Buffer   AS BYTE[]
            PUBLIC isHot	AS LOGIC
            
            PROPERTY Version    AS DBFVersion	;
                GET (DBFVersion) Buffer[HDROFFSETS.SIG] ;
                SET Buffer[HDROFFSETS.SIG] := (BYTE) VALUE
                
                PROPERTY Year		AS BYTE			;
                GET Buffer[HDROFFSETS.YEAR]	;
                SET Buffer[HDROFFSETS.YEAR] := VALUE, isHot := TRUE
                
                PROPERTY Month		AS BYTE			;
                GET Buffer[HDROFFSETS.MONTH]	;
                SET Buffer[HDROFFSETS.MONTH] := VALUE, isHot := TRUE
                
                PROPERTY Day		AS BYTE			;
                GET Buffer[HDROFFSETS.DAY]	;
                SET Buffer[HDROFFSETS.DAY] := VALUE, isHot := TRUE
                
                PROPERTY RecCount	AS LONG			;
                GET BitConverter.ToInt32(Buffer, HDROFFSETS.RECCOUNT) ;
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RECCOUNT, SIZEOF(LONG)), isHot := TRUE
                
                PROPERTY HeaderLen	AS SHORT		;
                GET BitConverter.ToInt16(Buffer, HDROFFSETS.DATAOFFSET);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.DATAOFFSET, SIZEOF(SHORT)), isHot := TRUE
                
                // Length of one data record, including deleted flag
                PROPERTY RecordLen	AS SHORT		;
                GET BitConverter.ToInt16(Buffer, HDROFFSETS.RECSIZE);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RECSIZE, SIZEOF(SHORT)), isHot := TRUE
                
                PROPERTY Reserved1	AS SHORT		;
                GET BitConverter.ToInt16(Buffer, HDROFFSETS.RESERVED1);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RESERVED1, SIZEOF(SHORT)), isHot := TRUE
                
                PROPERTY Transaction AS BYTE		;
                GET Buffer[HDROFFSETS.TRANSACTION];
                SET Buffer[HDROFFSETS.TRANSACTION] := VALUE, isHot := TRUE
                
                PROPERTY Encrypted	AS BYTE			;
                GET Buffer[HDROFFSETS.ENCRYPTED];
                SET Buffer[HDROFFSETS.ENCRYPTED] := VALUE, isHot := TRUE
                
                PROPERTY DbaseLan	AS LONG			;
                GET BitConverter.ToInt32(Buffer, HDROFFSETS.DBASELAN) ;
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.DBASELAN, SIZEOF(LONG)), isHot := TRUE
                
                PROPERTY MultiUser	AS LONG			;
                GET BitConverter.ToInt32(Buffer, HDROFFSETS.MULTIUSER)	;
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.MULTIUSER, SIZEOF(LONG)), isHot := TRUE
                
                PROPERTY Reserved2	AS LONG			;
                GET BitConverter.ToInt32(Buffer, HDROFFSETS.RESERVED2);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RESERVED2, SIZEOF(LONG))
                
                PROPERTY HasTags	AS DBFTableFlags ;
                GET (DBFTableFlags)Buffer[HDROFFSETS.HASTAGS] ;
                SET Buffer[HDROFFSETS.HASTAGS] := (BYTE) VALUE, isHot := TRUE
                
                PROPERTY CodePage	AS BYTE			 ;
                GET Buffer[HDROFFSETS.CODEPAGE]  ;
                SET Buffer[HDROFFSETS.CODEPAGE] := (BYTE) VALUE, isHot := TRUE
                
                PROPERTY Reserved3	AS SHORT         ;
                GET BitConverter.ToInt16(Buffer, HDROFFSETS.RESERVED3);
                SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, HDROFFSETS.RESERVED3, SIZEOF(SHORT)), isHot := TRUE
                
                PROPERTY LastUpdate AS DateTime      ;
                GET DateTime{1900+Year, Month, Day} ;
                SET Year := (BYTE) VALUE:Year % 100, Month := (BYTE) VALUE:Month, Day := (BYTE) VALUE:Day, isHot := TRUE
                
                METHOD initialize() AS VOID STRICT
                    Buffer := BYTE[]{HDROFFSETS.SIZE}
                    isHot  := FALSE
                    RETURN
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
                    30 – 31 	Reserved, contains 0x00
                    32 – n 	Field subrecords
                    The number of fields determines the number of field subrecords. 
                    One field subrecord exists for each field in the table.
                    n+1 			Header record terminator (0x0D)
                    n+2 to n+264 	A 263-byte range that contains the backlink, which is the 
                    relative path of an associated database (.dbc) file, information. 
                    If the first byte is 0x00, the file is not associated with a database. 
                    Therefore, database files always contain 0x00.	
                    see also ftp://fship.com/pub/multisoft/flagship/docu/dbfspecs.txt
                    
                    */
                    END STRUCTURE
            
            STRUCTURE DbfField   
                // Fixed Buffer of 32 bytes
                // Matches the DBF layout
                // Read/Write to/from the Stream with the Buffer 
                // and access individual values using the other fields
                METHOD initialize() AS VOID
                    SELF:Buffer := BYTE[]{FLDOFFSETS.SIZE}
                    
                PUBLIC Buffer		 AS BYTE[]	  
                
                PROPERTY Name		 AS STRING
                GET 
                    LOCAL fieldName := BYTE[]{FLDOFFSETS.NAME_SIZE} AS BYTE[]
                    Array.Copy( Buffer, FLDOFFSETS.NAME, fieldName, 0, FLDOFFSETS.NAME_SIZE )
                    LOCAL count := Array.FindIndex<BYTE>( fieldName, 0, { sz => sz == 0 } ) AS INT
                    IF count == -1
                        count := FLDOFFSETS.NAME_SIZE
                    ENDIF
                    LOCAL str := System.Text.Encoding.ASCII:GetString( fieldName,0, count ) AS STRING
                    IF ( str == NULL )
                        str := String.Empty
                    ENDIF
                    str := str:Trim()
                    RETURN str
                END GET
                SET
                    // Be sure to fill the Buffer with 0
                    Array.Clear( Buffer, FLDOFFSETS.NAME, 32 )
                    System.Text.Encoding.ASCII:GetBytes( VALUE, 0, Math.Max(32,VALUE:Length), Buffer, FLDOFFSETS.NAME )
                END SET
            END PROPERTY
            
            PROPERTY Type		 AS DBFieldType ;
            GET (DBFieldType) Buffer[ FLDOFFSETS.TYPE ] ;
            SET Buffer[ FLDOFFSETS.TYPE ] := (BYTE) VALUE
            
            // Offset from record begin in FP
            PROPERTY Offset 	 AS LONG ;
            GET BitConverter.ToInt32(Buffer, FLDOFFSETS.OFFSET);
            SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, FLDOFFSETS.OFFSET, SIZEOF(LONG))
            
            PROPERTY Len		 AS BYTE;
            GET Buffer[FLDOFFSETS.Len]  ;
            SET Buffer[FLDOFFSETS.Len] := (BYTE) VALUE
            
            PROPERTY Dec		 AS BYTE;
            GET Buffer[FLDOFFSETS.Dec]  ;
            SET Buffer[FLDOFFSETS.Dec] := (BYTE) VALUE
            
            PROPERTY Flags		 AS DBFFieldFlags;
            GET (DBFFieldFlags)Buffer[FLDOFFSETS.Flags] ;
            SET Buffer[FLDOFFSETS.Flags] := (BYTE) VALUE
            
            PROPERTY Counter	 AS LONG;
            GET BitConverter.ToInt32(Buffer, FLDOFFSETS.Counter);
            SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, FLDOFFSETS.Counter, SIZEOF(LONG))
            
            PROPERTY IncStep	 AS BYTE;
            GET Buffer[FLDOFFSETS.IncStep]  ;
            SET Buffer[FLDOFFSETS.IncStep] := (BYTE) VALUE
            
            PROPERTY Reserved1   AS BYTE;
            GET Buffer[FLDOFFSETS.Reserved1]  ;
            SET Buffer[FLDOFFSETS.Reserved1] := (BYTE) VALUE
            
            PROPERTY Reserved2   AS BYTE;
            GET Buffer[FLDOFFSETS.Reserved2]  ;
            SET Buffer[FLDOFFSETS.Reserved2] := (BYTE) VALUE
            
            PROPERTY Reserved3   AS BYTE;
            GET Buffer[FLDOFFSETS.Reserved3]  ;
            SET Buffer[FLDOFFSETS.Reserved3] := (BYTE) VALUE
            
            PROPERTY Reserved4  AS BYTE;
            GET Buffer[FLDOFFSETS.Reserved4]  ;
            SET Buffer[FLDOFFSETS.Reserved4] := (BYTE) VALUE
            
            PROPERTY Reserved5   AS BYTE;
            GET Buffer[FLDOFFSETS.Reserved5]  ;
            SET Buffer[FLDOFFSETS.Reserved5] := (BYTE) VALUE
            
            PROPERTY Reserved6   AS BYTE;
            GET Buffer[FLDOFFSETS.Reserved6]  ;
            SET Buffer[FLDOFFSETS.Reserved6] := (BYTE) VALUE
            
            PROPERTY Reserved7   AS BYTE;
            GET Buffer[FLDOFFSETS.Reserved7]  ;
            SET Buffer[FLDOFFSETS.Reserved7] := (BYTE) VALUE
            
            PROPERTY HasTag		 AS BYTE;
            GET Buffer[FLDOFFSETS.HasTag]  ;
            SET Buffer[FLDOFFSETS.HasTag] := (BYTE) VALUE
        END STRUCTURE
        
        
        [StructLayout(LayoutKind.Explicit)];
            STRUCTURE Dbf7Field   
            // Dbase 7 has 32 Bytes for Field Names
            // Fixed Buffer of 32 bytes
            // Matches the DBF layout
            // Read/Write to/from the Stream with the Buffer 
            // and access individual values using the other fields
            [FieldOffSet(00)] PUBLIC Buffer		 AS BYTE[]	
            [FieldOffSet(00)] PUBLIC Name		 AS BYTE[]    // Field name in ASCII (zero-filled).	  
            [FieldOffSet(32)] PUBLIC Type		 AS BYTE 	// Field type in ASCII (B, C, D, N, L, M, @, I, +, F, 0 or G).
            [FieldOffSet(33)] PUBLIC Len		 AS BYTE 	// Field length in binary.
            [FieldOffSet(34)] PUBLIC Dec		 AS BYTE
            [FieldOffSet(35)] PUBLIC Reserved1	 AS SHORT
            [FieldOffSet(37)] PUBLIC HasTag		 AS BYTE    // Production .MDX field flag; 0x01 if field has an index tag in the production .MDX file; 0x00 if the field is not indexed.
            [FieldOffSet(38)] PUBLIC Reserved2	 AS SHORT
            [FieldOffSet(40)] PUBLIC Counter	 AS LONG	// Next Autoincrement value, if the Field type is Autoincrement, 0x00 otherwise.
            [FieldOffSet(44)] PUBLIC Reserved3	 AS LONG	
            
        END STRUCTURE
        
        ENUM DBFVersion AS BYTE
            MEMBER FoxBase:=2
            MEMBER FoxBaseDBase3NoMemo:=3
            MEMBER dBase4 :=4
            MEMBER dBase5 :=5
            MEMBER VO :=7
            MEMBER Flagship := 0x13
            MEMBER Flagship248 := 0x23
            MEMBER VisualFoxPro:=0x30
            MEMBER VisualFoxProWithAutoIncrement:=0x31
            MEMBER Flagship248WithDBV := 0x33
            MEMBER dBase4SQLTableNoMemo:=0x43
            MEMBER dBase4SQLSystemNoMemo:=0x63
            MEMBER dBase4WithMemo_:=0x7b
            MEMBER FoxBaseDBase3WithMemo:=0x83
            MEMBER VOWithMemo := 0x87
            MEMBER dBase4WithMemo:=0x8b
            MEMBER dBase4SQLTableWithMemo:=0xcb
            MEMBER ClipperSixWithSMT:=0xe5
            MEMBER FoxPro2WithMemo:=0xf5
            MEMBER FoxBASE_:=0xfb
            
            MEMBER Unknown:=0
        END ENUM
        
        [Flags];
            ENUM DBFTableFlags AS BYTE
            MEMBER HasMemoField:=2
            MEMBER HasStructuralCDX:=1
            MEMBER IsDBC:=4
            MEMBER None:=0
        END ENUM
        
        [Flags];
            ENUM DBFFieldFlags AS BYTE
            MEMBER None:=0
            MEMBER System:=1
            MEMBER AllowNullValues:=2
            MEMBER Binary:=4
            MEMBER AutoIncrementing:=12
        END ENUM
        
    END CLASS
END NAMESPACE
