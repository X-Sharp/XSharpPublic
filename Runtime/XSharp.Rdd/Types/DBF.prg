//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.IO

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
            
            
            
            //	METHOD DbEval(info AS DbEvalInfo) AS LOGIC
            //	METHOD GoTop() AS LOGIC
            //	METHOD GoBottom() AS LOGIC   
            //	METHOD GoTo(nRec AS LONG) AS LOGIC
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
            //	METHOD Close() 			AS LOGIC  
            //	METHOD Create(info AS DbOpenInfo) AS LOGIC  
            //	METHOD Open(info AS DbOpenInfo) AS LOGIC
        VIRTUAL METHOD Open(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
            SELF:_OpenInfo := info
            // Should we set to .DBF per default ?
            IF String.IsNullOrEmpty(SELF:_OpenInfo:Extension)
                SELF:_OpenInfo:Extension := ".DBF"
                //
                Self:_OpenInfo:FileName := System.IO.Path.ChangeExtension( SELF:_OpenInfo:FileName, SELF:_OpenInfo:Extension )
            ENDIF
            //
            SELF:_FileName := SELF:_OpenInfo:FileName
            SELF:_Shared := SELF:_OpenInfo:Shared
            SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
            //
            SELF:_Stream := FileStream{ SELF:_FileName, FileMode.Open, ;
                    IIF( SELF:_ReadOnly, FileAccess.Read, FileAccess.ReadWrite ), ;
                    IIF( SELF:_Shared, FileShare.ReadWrite, FileShare.None ) }
            //

            RETURN TRUE

        PRIVATE METHOD _fillHeader() AS VOID
            //
            SELF:_Stream:Read( SELF:_Header:Buffer, 0, (INT)SIZEOF( DBFHeader ) )
            //
            RETURN
            
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
                // CASE DBS_ISNULL
                // CASE DBS_STEP
                OTHERWISE
                    oResult := SUPER:Info(nOrdinal, oNewValue)
            END SWITCH
            RETURN oResult
            //	METHOD FieldName(nFldPos AS LONG) AS STRING 
            
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
                    oResult := SELF:_LastUpdate
                CASE DbInfo.DBI_GETHEADERSIZE
                    oResult := SELF:_HeaderLength     
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
            //	PROPERTY RecCount	AS LONG GET
            //	PROPERTY RecNo		AS OBJECT GET 
            //	PROPERTY Shared		AS LOGIC GET
        VIRTUAL PROPERTY SysName AS STRING GET TYPEOF(Dbf):ToString()
        
        //	
        // Error Handling
        //	PROPERTY LastGenCode	AS LONG GET
        //	PROPERTY LastSubCode	AS LONG GET
        //	PROPERTY LastError		AS Exception GET
        
        
        [StructLayout(LayoutKind.Explicit)];
            STRUCTURE DbfHeader                     
            // Fixed Buffer of 32 bytes
            // Matches the DBF layout  
            // Read/Write to/from the Stream with the Buffer 
            // and access individual values using the other fields
            [FieldOffSet(00)] PUBLIC Buffer     AS BYTE[]

            [FieldOffSet(00)] PUBLIC Version  	AS DBFVersion	  
            [FieldOffSet(01)] PUBLIC Year		AS BYTE	  
            [FieldOffSet(02)] PUBLIC Month		AS BYTE	  
            [FieldOffSet(03)] PUBLIC Day		AS BYTE
            [FieldOffSet(04)] PUBLIC RecCount	AS LONG
            [FieldOffSet(08)] PUBLIC HeaderLen	AS SHORT 		// Position of first data record
            [FieldOffSet(10)] PUBLIC RecordLen	AS SHORT  		// Length of one data record, including deleted flag
            [FieldOffSet(12)] PUBLIC Reserved1	AS SHORT
            [FieldOffSet(14)] PUBLIC Transaction AS BYTE
            [FieldOffSet(15)] PUBLIC Encrypted	AS BYTE
            [FieldOffSet(16)] PUBLIC Reserved2	AS LONG 		// DbaseLan
            [FieldOffSet(20)] PUBLIC Reserved3	AS LONG			// MultiUser
            [FieldOffSet(24)] PUBLIC Reserved4	AS LONG
            [FieldOffSet(28)] PUBLIC HasTags	AS DBFTableFlags    		
            [FieldOffSet(29)] PUBLIC CodePage	AS BYTE        	// depends on dialect
            [FieldOffSet(30)] PUBLIC Reserved5	AS SHORT                             
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
        
        [StructLayout(LayoutKind.Explicit)];
            STRUCTURE DbfField   
            // Fixed Buffer of 32 bytes
            // Matches the DBF layout
            // Read/Write to/from the Stream with the Buffer 
            // and access individual values using the other fields
            [FieldOffSet(00)] PUBLIC Buffer		 AS BYTE[]	  
            [FieldOffSet(00)] PUBLIC Name		 AS BYTE[]
            [FieldOffSet(11)] PUBLIC Type		 AS DBFieldType
            [FieldOffSet(12)] PUBLIC Offset 	 AS LONG // Offset from record begin in FP
            [FieldOffSet(16)] PUBLIC Len		 AS BYTE
            [FieldOffSet(17)] PUBLIC Dec		 AS BYTE
            [FieldOffSet(18)] PUBLIC Flags		 AS DBFFieldFlags
            [FieldOffSet(19)] PUBLIC Counter	 AS LONG
            [FieldOffSet(23)] PUBLIC IncStep	 AS BYTE
            [FieldOffSet(24)] PUBLIC Reserved1   AS BYTE
            [FieldOffSet(25)] PUBLIC Reserved2   AS BYTE
            [FieldOffSet(26)] PUBLIC Reserved3   AS BYTE
            [FieldOffSet(27)] PUBLIC Reserved4  AS BYTE
            [FieldOffSet(28)] PUBLIC Reserved5   AS BYTE
            [FieldOffSet(29)] PUBLIC Reserved6   AS BYTE
            [FieldOffSet(20)] PUBLIC Reserved7   AS BYTE
            [FieldOffSet(31)] PUBLIC HasTag		 AS BYTE
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
