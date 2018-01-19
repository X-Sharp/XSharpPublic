//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#using System.IO

BEGIN NAMESPACE XSharp.RDD

CLASS Workarea IMPLEMENTS IRdd  
	// This class does NOT implement file based (DBF stuff). 
	// That is handled in the DBF class which inherits from RddBase
	#region Fields
	INTERNAL _Area			AS LONG		// Workarea Number (1 based)
	INTERNAL _Alias			AS STRING	// Unique Alias
	INTERNAL _FileName		AS STRING
	INTERNAL _Fields		AS RddFieldInfo[]	// List of Fields
	INTERNAL _Bof			AS LOGIC	// Is BOF ?
	INTERNAL _Bottom		AS LOGIC	// Is at Bottom ?
	INTERNAL _Eof			AS LOGIC	// Is EOF
	INTERNAL _Found			AS LOGIC	// Is Found ?
	INTERNAL _Top			AS LOGIC	// Is at Top
	INTERNAL _Result		AS OBJECT                
	INTERNAL _ScopeInfo		AS DbScopeInfo
	INTERNAL _FilterInfo	AS DbFilterInfo  
	INTERNAL _OrderCondInfo	AS DbOrderCondInfo
	INTERNAL _RelInfo		AS DbRelInfo
	INTERNAL _Parents		AS LONG		// # of parents   
	INTERNAL _MaxFieldNameLength AS LONG	// 

	// Some flags that are stored here but managed in subclasses
	INTERNAL _TransRec		AS LOGIC
	INTERNAL _RecordLength	AS WORD   	// Size of record
	INTERNAL _RecordBuffer	AS BYTE[]	// Current Record
	INTERNAL _BufferSize		AS LONG
	INTERNAL _Delimiter		AS STRING	// Field Delimiter
	INTERNAL _Separator	    AS STRING	// Field Separator
	INTERNAL _ReadOnly		AS LOGIC	// ReadOnly ?  
	INTERNAL _Shared		AS LOGIC	// Shared ?  
	INTERNAL _Stream		AS FileStream // File. 
	INTERNAL _Flush			AS LOGIC		// Must flush ? 

	// Memo and Order Implementation
	INTERNAL _Memo			AS IMemo
	INTERNAL _Order			AS IOrder

	#endregion

	CONSTRUCTOR() 
		SELF:_FilterInfo := DbFilterInfo{}
		SELF:_ScopeInfo  := DbScopeInfo{}            
        SELF:_OrderCondInfo := DbOrderCondInfo{}
		SELF:_Parents	 := 0   
		SELF:_Memo		 := BaseMemo{SELF}
		SELF:_Order		 := BaseIndex{SELF}     
		SELF:_Result	 := NULL
		SELF:_FileName	 := String.Empty
		SELF:_Fields	 := NULL
		SELF:_Area		 := -1
		SELF:_Shared	 := FALSE
		SELF:_ReadOnly   := FALSE
		SELF:_MaxFieldNameLength := 10
		SELF:_RelInfo    := NULL
		SELF:_Alias		 := String.Empty
		SELF:_RecordBuffer := NULL
			
VIRTUAL METHOD DbEval(info AS XSharp.RDD.DbEvalInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}


VIRTUAL METHOD GoTop( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GoBottom( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GoTo(nRec AS INT) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GoToId(oRec AS OBJECT) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Skip(nToSkip AS INT) AS LOGIC
	LOCAL lToSkip AS LONG
	IF nToSkip == 0
		RETURN SELF:SkipRaw(0)
	ENDIF
	SELF:_Top := SELF:_Bottom := FALSE
	IF nToSkip > 0
		lToSkip := 1 	// Forward
	ELSE
		lToSkip := -1	// Backward
		nToSkip := - nToSkip
	ENDIF
	DO WHILE --nToSkip >= 0
		IF !SELF:SkipRaw(lToSkip)
			RETURN FALSE
		ENDIF
		IF !SELF:SkipFilter(lToSkip)
			RETURN FALSE
		ENDIF              
		IF SELF:_BOF .or. SELF:_EOF
			EXIT
		ENDIF
	ENDDO
	IF lToSkip < 0
		SELF:_EOF := FALSE		
	ELSE
		SELF:_Bof := FALSE
	ENDIF
	RETURN TRUE	

VIRTUAL METHOD SkipFilter(nToSkip AS INT) AS LOGIC
	LOCAL Bottom /*, Deleted */ AS LOGIC
	// When no active filter, record is Ok
	IF SELF:_FilterInfo == NULL_OBJECT .or. ;
		!SELF:_FilterInfo:Active .or. ;
		SELF:_FilterInfo:FilterBlock == NULL_OBJECT ;
		// .or. !SetDeleted()
		RETURN TRUE
	ENDIF
	nToSkip := iif(nToSkip < 0 , -1, 1)
	Bottom := SELF:_Bottom    
	RETURN FALSE
/*
while( ! pArea->fBof && ! pArea->fEof )
   {
      /* SET DELETED * /
      IF( hb_setGetDeleted() )
      {
         IF( SELF_DELETED( pArea, &fDeleted ) != HB_SUCCESS )
            RETURN HB_FAILURE;
         IF( fDeleted )
         {
            IF( SELF_SKIPRAW( pArea, lUpDown ) != HB_SUCCESS )
               RETURN HB_FAILURE;
            continue;
         }
      }

      /* SET FILTER TO * /
      IF( pArea->dbfi.itmCobExpr )
      {
         if( SELF_EVALBLOCK( pArea, pArea->dbfi.itmCobExpr ) != HB_SUCCESS )
            return HB_FAILURE;

         if( HB_IS_LOGICAL( pArea->valResult ) &&
             ! hb_itemGetL( pArea->valResult ) )
         {
            if( SELF_SKIPRAW( pArea, lUpDown ) != HB_SUCCESS )
               return HB_FAILURE;
            continue;
         }
      }

      break;
   }

   /*
    * The only one situation when we should repos is backward skipping
    * if we are at BOTTOM position (it's SKIPFILTER called from GOBOTTOM)
    * then GOEOF() if not then GOTOP()
    * /
   IF( pArea->fBof && lUpDown < 0 )
   {
      IF( fBottom )
      {
         /* GOTO EOF (phantom) record -
            this is the only one place where GOTO is used by Harbour
            directly and RDD which does not operate on numbers should
            serve this method only as SELF_GOEOF() synonym. If it's a
            problem then we can remove this if and always use SELF_GOTOP()
            but it also means second table scan if all records filtered
            are out of filter so I do not want to do that. I will prefer
            explicit add SELF_GOEOF() method
          * /
         errCode = SELF_GOTO( pArea, 0 );
      }
      else
      {
         errCode = SELF_GOTOP( pArea );
         pArea->fBof = HB_TRUE;
      }
   }
   else
   {
      errCode = HB_SUCCESS;
   }

   RETURN errCode; 
*/		

VIRTUAL METHOD SkipRaw(nToSkip AS INT) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD SkipScope(nToSkip AS INT) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Delete( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GetRec( ) AS BYTE[]
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Pack( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD PutRec(aRec AS BYTE[]) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Recall( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Zap( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Close( ) AS LOGIC
	SELF:ClearFilter()
	SELF:ClearRel()
	SELF:ClearScope()
	// close all parent relations
	RETURN TRUE

VIRTUAL METHOD Create(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Open(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD ClearFilter( ) AS LOGIC
	IF SELF:_FilterInfo != NULL
		SELF:_FilterInfo:Clear()
	ELSE
		SELF:_FilterInfo := DbFilterInfo{}
	ENDIF
    RETURN TRUE
VIRTUAL METHOD ClearScope( ) AS LOGIC
	IF SELF:_ScopeInfo != NULL        
		SELF:_ScopeInfo:Clear()
	ELSE 
		SELF:_ScopeInfo := DbScopeInfo{}
	ENDIF 
	RETURN TRUE

VIRTUAL METHOD Continue( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GetScope( ) AS XSharp.RDD.DbScopeInfo
	IF SELF:_ScopeInfo != NULL_OBJECT
		RETURN SELF:_ScopeInfo:Clone()
	ENDIF
	RETURN DbScopeInfo{}

VIRTUAL METHOD ScopeInfo(nOrdinal AS INT) AS OBJECT
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD SetFilter(info AS XSharp.RDD.DbFilterInfo) AS LOGIC
	SELF:ClearFilter()
	IF (info != NULL_OBJECT)
		SELF:_FilterInfo := info:Clone()
		SELF:_FilterInfo:Active := TRUE
	ENDIF
	RETURN TRUE

VIRTUAL METHOD SetScope(info AS XSharp.RDD.DbScopeInfo) AS LOGIC
	SELF:ClearScope()
	IF (info != NULL_OBJECT)
		SELF:_ScopeInfo := info:Clone()
	ENDIF
    RETURN TRUE

VIRTUAL METHOD AddField(info AS DbFieldInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD CreateFields(fields AS RddFieldInfo[]) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD FieldIndex(fieldName AS STRING) AS INT
	LOCAL nMax AS INT
	nMax := SELF:FieldCount
	FOR VAR nFldPos := 0 TO nMax -1
		IF String.Compare(SELF:_Fields[nFldPos]:Name, fieldName, StringComparison.OrdinalIgnoreCase) == 0
			RETURN nFldPos+1
		ENDIF
	NEXT 
	RETURN 0
	
PRIVATE METHOD _FieldIndexValidate(nFldPos AS LONG) AS LOGIC
	LOCAL nMax AS INT
	nMax := (INT) SELF:_Fields?:Length  
	IF nFldPos <= 0 .or. nFldPos > nMax 
		THROW ArgumentException{"Invalid Field Index, must be between 1 and "+SELF:FieldCount:ToString(), nameof(nFldPos)}
	ENDIF
	RETURN TRUE	

VIRTUAL METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
	IF SELF:_FieldIndexValidate(nFldPos)
		nFldPos -= 1
	ENDIF
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD FieldName(nFldPos AS INT) AS STRING
	IF SELF:_FieldIndexValidate(nFldPos)
		nFldPos -= 1
		RETURN SELF:_Fields[nFldPos]:Name
	ENDIF          
	RETURN String.Empty

VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	RETURN _Memo:GetValueFile(nFldPos, fileName )

VIRTUAL METHOD GetValueLength(nFldPos AS INT) AS INT
	RETURN _Memo:GetValueLength(nFldPos )

VIRTUAL METHOD Flush( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GoCold( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD GoHot( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
	RETURN _Memo:PutValueFile(nFldPos, fileName )

VIRTUAL METHOD AppendLock(uiMode AS XSharp.RDD.DbLockMode) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD HeaderLock(uiMode AS XSharp.RDD.DbLockMode) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Lock(uiMode AS XSharp.RDD.DbLockMode) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD UnLock(oRecId AS OBJECT) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD CloseMemFile( ) AS LOGIC
	RETURN SELF:_Memo:CloseMemFile()

VIRTUAL METHOD CreateMemFile(info AS XSharp.RDD.DbOpenInfo) AS LOGIC
	RETURN SELF:_Memo:CreateMemFile(info)

VIRTUAL METHOD OpenMemFile( ) AS LOGIC
	RETURN SELF:_Memo:OpenMemFile()

#region Orders Not implemented

VIRTUAL METHOD OrderCondition(info AS XSharp.RDD.DbOrderCondInfo) AS LOGIC
	RETURN SELF:_Order:OrderCondition(info)

VIRTUAL METHOD OrderCreate(info AS XSharp.RDD.DbOrderCreateInfo) AS LOGIC
	RETURN SELF:_Order:OrderCreate(info)

VIRTUAL METHOD OrderDestroy(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN SELF:_Order:OrderDestroy(info)

VIRTUAL METHOD OrderInfo(nOrdinal AS INT) AS OBJECT
   /* CA-Cl*pper does not generate RT error when default ORDERINFO() method
    * is called
    */
   	RETURN SELF:_Order:OrderInfo(nOrdinal)

VIRTUAL METHOD OrderListAdd(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN SELF:_Order:OrderListAdd(info)

VIRTUAL METHOD OrderListDelete(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN SELF:_Order:OrderListDelete(info)

VIRTUAL METHOD OrderListFocus(info AS XSharp.RDD.DbOrderInfo) AS LOGIC
	RETURN SELF:_Order:OrderListFocus(info)

VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
	RETURN SELF:_Order:OrderListRebuild()

VIRTUAL METHOD Seek(info AS XSharp.RDD.DbSeekInfo) AS LOGIC
	RETURN SELF:_Order:Seek(info)
#endregion


#region Relations
            
VIRTUAL METHOD ChildEnd(info AS XSharp.RDD.DbRelInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD ChildStart(info AS XSharp.RDD.DbRelInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD ChildSync(info AS XSharp.RDD.DbRelInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD ClearRel( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD ForceRel( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD RelArea(nRelNum AS INT) AS INT
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD RelEval(info AS XSharp.RDD.DbRelInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD RelText(nRelNum AS INT) AS STRING
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD SetRel(info AS XSharp.RDD.DbRelInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD SyncChildren( ) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

#endregion

#region Trans not implemented

VIRTUAL METHOD Trans(info AS XSharp.RDD.DbTransInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD TransRec(info AS XSharp.RDD.DbTransInfo) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

#endregion

VIRTUAL METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Compile(sBlock AS STRING) AS LOGIC
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD EvalBlock(oBlock AS OBJECT) AS OBJECT
	THROW NotImplementedException{__ENTITY__}

VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
	LOCAL oResult AS OBJECT
	SWITCH nOrdinal
	CASE DBI_ISDBF
	CASE DBI_CANPUTREC
	CASE DBI_ISFLOCK
	CASE DBI_TRANSREC
	CASE DBI_RM_SUPPORTED
		oResult := FALSE      
	CASE DBI_SHARED
		oResult := SELF:_Shared
	CASE DBI_ISREADONLY
		oResult := SELF:_ReadOnly
	CASE DBI_GETDELIMITER
		oResult := SELF:_Delimiter
	CASE DBI_SEPARATOR
		oResult := SELF:_Separator
	CASE DBI_SETDELIMITER            
		oResult := SELF:_Separator		
		IF oNewValue != NULL .and. oNewValue:GetType() == typeof(STRING)
			SELF:_Separator	:= (STRING) oNewValue
		ENDIF
	CASE DBI_DB_VERSION
	CASE DBI_RDD_VERSION
		oResult := ""
	CASE DBI_GETHEADERSIZE
	CASE DBI_LOCKCOUNT
		oResult := 0     
	CASE DBI_GETRECSIZE
		oResult := SELF:_RecordLength
	CASE DBI_LASTUPDATE
		oResult := DateTime.MinValue 
	CASE DBI_GETLOCKARRAY
		oResult := <LONG>{}
	CASE DBI_BOF           
		oResult := SELF:_BOF
	CASE DBI_EOF           
		oResult := SELF:_EOF   
	CASE DBI_DBFILTER      
		oResult := SELF:_FilterInfo?:FilterText
	CASE DBI_FOUND
		oResult := SELF:_Found
	CASE DBI_FCOUNT
		oResult := (INT) _Fields?:Length
	CASE DBI_ALIAS
		oResult := _Alias
	CASE DBI_FULLPATH
		oResult := SELF:_FileName
	// CASE DBI_CHILDCOUNT:
	// CASE DBI_TABLEEXT
	// CASE DBI_SCOPEDRELATION
	// CASE DBI_POSITIONED
	// CASE DBI_CODEPAGE
	OTHERWISE
		oResult := NULL
	END SWITCH
	RETURN oResult
		

	VIRTUAL METHOD RecInfo(oRecID AS OBJECT, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT  
		LOCAL oResult AS OBJECT
		SWITCH nOrdinal

		// etc
		OTHERWISE
			oResult := NULL
		END SWITCH
		RETURN oResult

	VIRTUAL METHOD Sort(info AS XSharp.RDD.DbSortInfo) AS LOGIC
		THROW NotImplementedException{__ENTITY__}

	VIRTUAL PROPERTY Alias AS STRING GET _Alias

	VIRTUAL PROPERTY Area AS LONG GET _Area

	VIRTUAL PROPERTY BoF AS LOGIC GET _Bof

	VIRTUAL PROPERTY Deleted AS LOGIC GET FALSE

    VIRTUAL PROPERTY Driver AS STRING GET "Workarea"

	VIRTUAL PROPERTY EoF AS LOGIC GET _Eof

	VIRTUAL PROPERTY Exclusive AS LOGIC GET FALSE

	VIRTUAL PROPERTY FieldCount AS LONG GET (LONG) _Fields?:Length

	VIRTUAL PROPERTY FilterText AS STRING GET _FilterInfo?:FilterText

	VIRTUAL PROPERTY Found AS LOGIC GET _Order:Found SET _Order:Found := VALUE


	VIRTUAL PROPERTY RecCount AS INT GET 0

	VIRTUAL PROPERTY RecId AS OBJECT GET NULL
	VIRTUAL PROPERTY RecNo AS LONG GET   0	

	VIRTUAL PROPERTY Shared AS LOGIC GET FALSE

	VIRTUAL PROPERTY SysName AS STRING GET typeof(Workarea):ToString()

END CLASS
END NAMESPACE

