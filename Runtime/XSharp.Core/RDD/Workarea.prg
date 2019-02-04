//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.IO
USING System.Reflection
USING System.Collections.Generic
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Text
BEGIN NAMESPACE XSharp.RDD

	/// <summary>Base class for DBF based RDDs. Holds common properties such as the Workarea number, Alias, Fields list and various flags.</summary> 
	/// <seealso cref="T:XSharp.RDD.IRdd"/>
	CLASS Workarea IMPLEMENTS IRdd, IDisposable 
		// This class does NOT implement file based (DBF stuff). 
		// That is handled in the DBF class which inherits from RddBase
		#region Fields
		PROTECTED _Disposed     AS LOGIC
		/// <summary>Workarea Number (1 based) </summary>
		PUBLIC _Area			AS DWORD		
		/// <summary> Unique Alias </summary>
		PUBLIC _Alias			AS STRING	
		/// <summary>File name of the main file</summary>
		PUBLIC _FileName		AS STRING
		/// <summary>List of Fields</summary>
		PUBLIC _Fields		    AS RddFieldInfo[]
        PRIVATE _fieldNames     AS Dictionary<STRING, INT>
        PRIVATE _currentField   AS LONG
		/// <summary>Is at BOF ?</summary>
		PUBLIC _Bof			    AS LOGIC	
		/// <summary>Is at bottom ?</summary>
		PUBLIC _Bottom		    AS LOGIC	
		/// <summary>Is at EOF ?</summary>
		PUBLIC _Eof			    AS LOGIC	
		/// <summary>Result of last SEEK operation</summary>
		PUBLIC _Found			AS LOGIC	
		/// <summary>Is at top?</summary>
		PUBLIC _Top			    AS LOGIC	
		/// <summary>Result of last macro evaluation</summary>
		PUBLIC _Result		    AS OBJECT                
		/// <summary>Current Scope</summary>
		PUBLIC _ScopeInfo		AS DbScopeInfo
		/// <summary>Current Filter</summary>
		PUBLIC _FilterInfo	    AS DbFilterInfo  
		/// <summary>Current Order condition</summary>
		PUBLIC _OrderCondInfo	AS DbOrderCondInfo
		/// <summary>Current Relation info</summary>
		PUBLIC _RelInfo		    AS DbRelInfo
		PUBLIC _Relations       AS List<DbRelInfo>
		/// <summary># of parents</summary>
		PUBLIC _Parents		    AS LONG		
		/// <summary>Maximum fieldname length (Advantage supports field names > 10 characters)</summary>
		PUBLIC _MaxFieldNameLength AS LONG	// 
		
		// Some flags that are stored here but managed in subclasses
		PUBLIC _TransRec		AS LOGIC
		/// <summary>Size of record</summary>
		PUBLIC _RecordLength	AS LONG   	
		/// <summary>Current Record</summary>
		PUBLIC _RecordBuffer	AS BYTE[]	
		/// <summary>Field delimiter (for DELIM RDD)</summary>
		PUBLIC _Delimiter	:= "" AS STRING	
		/// <summary>Field separator (for DELIM RDD)</summary>
		PUBLIC _Separator	:= ""  AS STRING	
		/// <summary> Is the file opened ReadOnly ?</summary>
		PUBLIC _ReadOnly		AS LOGIC	
		/// <summary> Is the file opened Shared ?</summary>
		PUBLIC _Shared			AS LOGIC	
		/// <summary>File handle of the current file</summary>
		PUBLIC _hFile			AS IntPtr
		/// <summary>Should the file be flushed (it is dirty) ?</summary>
		PUBLIC _Flush			AS LOGIC		
		
		// Memo and Order Implementation
		/// <summary>Current memo implementation.</summary>
		PUBLIC _Memo			AS IMemo
		/// <summary>Current index implementation.</summary>
		PUBLIC _Order			AS IOrder
		
		/// <summary>Result of the last Block evaluation.</summary>
		PUBLIC _EvalResult    AS OBJECT
		#endregion
		
		#region Static Properties that point to Runtime state ?
		// Ansi
		// AutoOrder
		// AutoShare
		// etc
		#endregion
		/// <exclude />
		CONSTRUCTOR() 
			SELF:_FilterInfo := DbFilterInfo{}
			SELF:_ScopeInfo  := DbScopeInfo{}            
			SELF:_OrderCondInfo := DbOrderCondInfo{}
			SELF:_Relations  := List<DbRelInfo>{}
			SELF:_Parents	 := 0   
			SELF:_Memo		 := BaseMemo{SELF}
			SELF:_Order		 := BaseIndex{SELF}     
			SELF:_Result	 := NULL
			SELF:_FileName	 := String.Empty
			SELF:_Fields	 := NULL
			SELF:_Area		 := 0
			SELF:_Shared	 := FALSE
			SELF:_ReadOnly   := FALSE
			SELF:_MaxFieldNameLength := 10
			SELF:_RelInfo    := NULL
			SELF:_Alias		 := String.Empty
			SELF:_RecordBuffer := NULL
			SELF:_Disposed   := FALSE
            SELF:_FieldNames := Dictionary<STRING, INT>{Stringcomparer.OrdinalIgnoreCase}
		/// <exclude />	
		DESTRUCTOR()
			SELF:Dispose(FALSE)
			
		VIRTUAL METHOD Dispose(disposing AS LOGIC) AS VOID
			IF !_Disposed
				_Disposed := TRUE
			ENDIF
			
		VIRTUAL METHOD Dispose() AS VOID
			SELF:Dispose(TRUE)
			GC.SuppressFinalize(SELF)
			RETURN
			
			/// <inheritdoc />			
		VIRTUAL METHOD DbEval(info AS DbEvalInfo) AS LOGIC
			// fetch locals from info
            LOCAL nRecno   := 0 AS LONG
            LOCAL isOk     := TRUE AS LOGIC
            LOCAL lLimit   := FALSE AS LOGIC
            LOCAL lRecordOk := TRUE AS LOGIC
            LOCAL cbWhile   AS ICodeblock
            LOCAL cbFor     AS ICodeblock
            LOCAL cbEval    AS ICodeblock
            info:ScopeInfo:Compile(SELF)
            cbWhile  := info:ScopeInfo:WhileBlock 
            cbFor    := info:ScopeInfo:ForBlock   
            cbEval   := info:Block  
            IF info:ScopeInfo:RecId != NULL
                nRecno := Convert.ToInt32(info:ScopeInfo:RecId)
                isOk   := SELF:GoTo(nRecno)
                lLimit := TRUE
                nRecno := 1
            ELSEIF info:ScopeInfo:NextCount != 0
                lLimit := TRUE
                nRecno := info:ScopeInfo:NextCount
                IF nRecno < 1
                  RETURN TRUE
                ENDIF
            ELSE
                lLimit  := FALSE
                IF cbWhile == NULL .AND. ! info:ScopeInfo:Rest
                  isOk := SELF:GoTop()
                ENDIF
            ENDIF
            DO WHILE isOk .AND. ! SELF:_Eof 
                IF cbWhile != NULL
                    IF ! (LOGIC) SELF:EvalBlock(cbWhile)
                        EXIT
                    ENDIF
                ENDIF
                IF cbFor != NULL
                   lRecordOk := (LOGIC) SELF:EvalBlock(cbFor)
                ELSE
                   lRecordOk := TRUE
                ENDIF
                IF lRecordOk .AND. cbEval != NULL
                    isOk := (LOGIC) SELF:EvalBlock(cbEval)
                ENDIF
                IF lLimit
                    nRecno -= 1
                    IF nRecno == 0
                        EXIT
                    ENDIF
                ENDIF
                IF isOk 
                    isOk := SELF:Skip(1)
                ENDIF
            ENDDO
            RETURN isOk
			
			
			/// <inheritdoc />
		VIRTUAL METHOD GoTop( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD GoBottom( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD GoTo(nRec AS INT) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD GoToId(oRec AS OBJECT) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
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
				IF SELF:_BOF .OR. SELF:_EOF
					EXIT
				ENDIF
			ENDDO
			IF lToSkip < 0
				SELF:_EOF := FALSE		
			ELSE
				SELF:_Bof := FALSE
			ENDIF
			RETURN TRUE	
			
			/// <inheritdoc />
		VIRTUAL METHOD SkipFilter(nToSkip AS INT) AS LOGIC
			LOCAL fromBottom   AS LOGIC
            LOCAL fromTop      AS LOGIC
            LOCAL recordHidden AS LOGIC
            LOCAL result       AS LOGIC
            LOCAL cbFilter     AS ICodeblock
            LOCAL fRtDeleted   AS LOGIC
            fRtDeleted := RuntimeState.Deleted
            IF SELF:_FilterInfo:Active
                cbFilter    := SELF:_FilterInfo:FilterBlock
            ELSE
                cbFilter    := NULL
            ENDIF
            IF cbFilter == NULL .AND. ! fRtDeleted
                // No filter and not SetDeleted(TRUE), so nothing to do.
                RETURN TRUE
            ENDIF
            // Make sure we skip only one record at a time
            nToSkip     := IIF(nToSkip < 0, -1, 1)
			fromBottom  := SELF:_Bottom 
            fromTop     := SELF:_Top    
            recordHidden:= TRUE 
            result      := TRUE 
            
            DO WHILE !SELF:_Eof .AND. ! SELF:_Bof
                // Check deleted first, that is easier and has less overhead
                IF fRtDeleted
                    recordHidden := SELF:Deleted
                ELSE
                    recordHidden := FALSE
                ENDIF

                IF ! recordHidden .AND. cbFilter != NULL
                    recordHidden := ! (LOGIC) SELF:EvalBlock(cbFilter)
                ENDIF
                IF recordHidden 
                    result := SELF:SkipRaw(nToSkip)
                ELSE
                    EXIT
                ENDIF
             ENDDO
             IF result
                IF fromTop .AND. SELF:_Eof
                    SELF:_BOF := TRUE
                ELSEIF fromBottom .AND. SELF:_Bof
                    SELF:_EOF := TRUE
                ELSEIF SELF:_Bof .AND. nToSkip < 0
                    // note that this will recurse!
                    result := SELF:GoTop()
                    SELF:_Bof := TRUE
                    SELF:_Eof := FALSE
                ENDIF
             ENDIF
            RETURN result

			
		/// <inheritdoc />
		VIRTUAL METHOD SkipRaw(nToSkip AS INT) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD SkipScope(nToSkip AS INT) AS LOGIC
			LOCAL lFound    := FALSE AS LOGIC
            LOCAL lContinue := TRUE AS LOGIC
            LOCAL result    := TRUE AS LOGIC
            LOCAL nextCnt := SELF:_ScopeInfo:NextCount  AS LONG
            LOCAL cbWhile AS ICodeblock
            LOCAL cbFor   AS ICodeblock
            _ScopeInfo:Compile(SELF)
            cbWhile := _ScopeInfo:WhileBlock
            cbFor   := _ScopeInfo:ForBlock
            IF SELF:_ScopeInfo:RecId != NULL
                result     := SELF:GotoId(SELF:_ScopeInfo:RecId)
                lContinue  := ! SELF:_Eof
                IF lContinue
                    IF cbWhile != NULL
                        lContinue := (LOGIC) SELF:EvalBlock(cbWhile)
                    ENDIF
                    IF cbFor != NULL .AND.  lContinue 
                        lFound := (LOGIC) SELF:EvalBlock(cbFor)
                    ELSE
                        lFound := TRUE
                    ENDIF
                ENDIF
            ELSEIF nextCnt > 0
                DO WHILE lContinue .AND. ! SELF:_Eof .AND. nToSkip != 0 .AND. result
                    result := SELF:Skip(1)
                     IF cbWhile != NULL
                        lContinue := (LOGIC) SELF:EvalBlock(cbWhile)
                    ENDIF
                    IF ! lContinue
                        result := SELF:Skip(-1)
                        EXIT
                    ELSEIF cbFor != NULL
                        lFound := (LOGIC) SELF:EvalBlock(cbFor)
                    ELSE
                        lFound := TRUE
                    ENDIF
                    IF lFound
                        nToSkip -= 1
                    ENDIF
                    nextCnt -= 1
                    lContinue := nextCnt > 0
                ENDDO
            ELSEIF SELF:_ScopeInfo:Rest
                DO WHILE ! SELF:_Eof .AND. nToSkip != 0 .AND. result
                    result := SELF:Skip(1)
                    IF cbWhile != NULL
                        lContinue := (LOGIC) SELF:EvalBlock(cbWhile)
                    ELSE
                        lContinue := TRUE
                    ENDIF
                    IF ! lContinue
                        result := SELF:Skip(-1)
                        EXIT
                    ELSEIF cbFor != NULL
                        lFound := (LOGIC) SELF:EvalBlock(cbFor)
                    ELSE
                        lFound := TRUE
                    ENDIF
                    IF lFound
                        nToSkip -= 1
                    ENDIF
                    nextCnt -= 1
                    lContinue := nextCnt > 0
                ENDDO
            ELSE
                result := SELF:GoTop()
                DO WHILE ! SELF:_Eof .AND. nToSkip != 0 .AND. result
                    IF cbFor != NULL
                        lFound := (LOGIC) SELF:EvalBlock(cbFor)
                    ELSE
                        lFound := TRUE
                    ENDIF
                    IF lFound
                        nToSkip -= 1
                    ENDIF
                    IF nToSkip != 0
                        result := SELF:Skip(1)
                    ENDIF
                ENDDO
            ENDIF
            SELF:_Found := lFound
            RETURN result
            
			/// <inheritdoc />
		VIRTUAL METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD Delete( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD GetRec( ) AS BYTE[]
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD Pack( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD PutRec(aRec AS BYTE[]) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD Recall( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD Zap( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD Close( ) AS LOGIC
			SELF:ClearFilter()
			SELF:ClearRel()
			SELF:ClearScope()
			// close all parent relations
			IF SELF:_Parents > 0
				//
				LOCAL max AS DWORD
				max := Workareas.MaxWorkAreas //<- what's wrong here ??
				FOR VAR i := 1 TO max
					VAR rdd := XSharp.RuntimeState.Workareas:GetRDD( (DWORD)i )
					VAR wk := rdd ASTYPE Workarea
					IF ( wk != NULL ) .AND. ( wk != SELF )
						wk:_Relations:RemoveAll( SELF:isChildPredicate )
					ENDIF
				NEXT
			ENDIF
			RETURN TRUE
			
			/// <inheritdoc />
		VIRTUAL METHOD Create(info AS DbOpenInfo) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD Open(info AS DbOpenInfo) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD ClearFilter( ) AS LOGIC
			IF SELF:_FilterInfo != NULL
				SELF:_FilterInfo:Clear()
			ELSE
				SELF:_FilterInfo := DbFilterInfo{}
			ENDIF
			RETURN TRUE
			
			/// <inheritdoc />
		VIRTUAL METHOD ClearScope( ) AS LOGIC
			IF SELF:_ScopeInfo != NULL        
				SELF:_ScopeInfo:Clear()
			ELSE 
				SELF:_ScopeInfo := DbScopeInfo{}
			ENDIF 
			RETURN TRUE
			
			/// <inheritdoc />
		VIRTUAL METHOD Continue( ) AS LOGIC
			LOCAL result := FALSE AS LOGIC
            IF SELF:_ScopeInfo:ForBlock != NULL
                SELF:_ScopeInfo:NextCount := 0
                SELF:_ScopeInfo:Rest      := TRUE
                SELF:_ScopeInfo:RecId     := NULL
                result := SELF:SkipScope(1)
            ENDIF
            RETURN result
			
			/// <inheritdoc />
		VIRTUAL METHOD GetScope( ) AS DbScopeInfo
			IF SELF:_ScopeInfo != NULL_OBJECT
				RETURN SELF:_ScopeInfo:Clone()
			ENDIF
			RETURN DbScopeInfo{}
			
			/// <inheritdoc />
		VIRTUAL METHOD SetFilter(info AS DbFilterInfo) AS LOGIC
			SELF:ClearFilter()
			IF info != NULL_OBJECT
				SELF:_FilterInfo        := info:Clone()
                SELF:_FilterInfo:Compile(SELF)
                IF SELF:_FilterInfo:FilterBlock != NULL
				    SELF:_FilterInfo:Active := TRUE
                ENDIF
			ENDIF
			RETURN TRUE
			
		VIRTUAL METHOD SetFieldExtent( fieldCount AS LONG ) AS LOGIC
            SELF:_Fields        := RddFieldInfo[]{ fieldCount }
			SELF:_fieldNames	:= Dictionary<STRING, INT>{Stringcomparer.OrdinalIgnoreCase}
            SELF:_RecordLength  := 1 // 1 for DELETED
            SELF:_currentField  := 0
            RETURN TRUE
			
			/// <inheritdoc />
		VIRTUAL METHOD SetScope(info AS DbScopeInfo) AS LOGIC
			SELF:ClearScope()
			IF info != NULL_OBJECT
				SELF:_ScopeInfo := info:Clone()
                SELF:_ScopeInfo:Compile(SELF)
			ENDIF
			RETURN TRUE
            
        PROTECT VIRTUAL METHOD _checkFields(info AS RddFieldInfo) AS LOGIC
            RETURN TRUE

			/// <inheritdoc />
		VIRTUAL METHOD AddField(info AS RddFieldInfo) AS LOGIC
			LOCAL result AS LOGIC
            result := SELF:FieldIndex(info:Name) == 0
            IF result
              IF SELF:_currentField < SELF:_Fields:Length 
                SELF:_checkFields( info )
                IF _currentField > 0
                    LOCAL lastField AS RddFieldInfo
                    lastField := SELF:_Fields[_currentField -1]
                    info:OffSet := lastField:Offset + lastField:Length
                ELSE
                    info:Offset := 1
                ENDIF
                SELF:_Fields[ SELF:_currentField] := info 

                // the alias could be an empty string !
                IF !String.IsNullOrEmpty(info:Alias) 
                    SELF:_fieldNames:Add(info:Alias:Trim(), SELF:_currentField)
                ELSE
                    SELF:_fieldNames:Add(info:Name:Trim(), SELF:_currentField)
                ENDIF
                SELF:_currentField++
                SELF:_RecordLength += (WORD)info:Length
              ELSE
                result := FALSE
              ENDIF
            ENDIF
			RETURN result
            
			/// <inheritdoc />
		VIRTUAL METHOD CreateFields(aFields AS RddFieldInfo[]) AS LOGIC
            // fills the field list from the aFields array
            LOCAL fieldCount AS LONG
            LOCAL result := FALSE AS LOGIC
            fieldCount := aFields:Length
            IF  fieldCount > 0 
                result := SELF:SetFieldExtent( fieldCount )
                IF result
                    FOR VAR i := 0 TO fieldCount - 1
                        result := SELF:AddField( aFields[i] )
                        IF !result
                            EXIT
                        ENDIF
                    NEXT
                ENDIF
            ENDIF
            RETURN result
			
			/// <inheritdoc />
		VIRTUAL METHOD FieldIndex(fieldName AS STRING) AS INT
            IF SELF:_fieldNames:ContainsKey(fieldName)
                RETURN SELF:_fieldNames[fieldName]+1
            ENDIF
            RETURN 0
			
			/// <exclude/>
		PROTECTED METHOD _FieldIndexValidate(nFldPos AS LONG) AS LOGIC
			LOCAL nMax AS INT
			// Note that nFldPos is 1 based
			nMax := (INT) SELF:_Fields?:Length  
			IF nFldPos <= 0 .OR. nFldPos > nMax 
				THROW ArgumentException{"Invalid Field Index, must be between 1 and "+SELF:FieldCount:ToString(), NAMEOF(nFldPos)}
			ENDIF
			RETURN TRUE	
			
			/// <inheritdoc />
		VIRTUAL METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
            // Note that nFldPos is 1 based
            IF SELF:_FieldIndexValidate(nFldPos)
                LOCAL ofld AS RddFieldInfo
                ofld := SELF:_Fields[nFldPos-1]
                SWITCH nOrdinal
                CASE DbFieldInfo.DBS_NAME
                    RETURN ofld:Name
                CASE DbFieldInfo.DBS_TYPE
                    RETURN oFld:FieldType:ToString():Substring(0,1)
                CASE DbFieldInfo.DBS_LEN
                    RETURN oFld:Length
                CASE DbFieldInfo.DBS_DEC
                    RETURN oFld:Decimals
                CASE DbFieldInfo.DBS_ALIAS
                    IF oNewValue IS STRING
                        // set new alias for the field
                        IF _fieldNames:ContainsValue(nFldPos-1)
                            // Should always be the case, but better safe than sorry
                            FOREACH VAR pair IN SELF:_fieldNames
                                IF pair:value == nFldPos-1
                                    SELF:_fieldNames:Remove(pair:Key)
                                    EXIT
                                ENDIF
                            NEXT
                        ENDIF
                        oFld:Alias := ((STRING) oNewValue):ToUpperInvariant()
                        // the new alias could be an empty string !
                        // When it is then we use the fieldname, because every field
                        // must be represented in the Names table
                        VAR cNewName := oFld:Alias:Trim()
                        IF String.IsNullOrEmpty(cNewName)
                            oFld:Alias := NULL
                            cNewName  := oFld:Name:Trim()
                        ENDIF
                        SELF:_fieldNames:Add(cNewName, nFldPos-1)
                    ENDIF
                    IF oFld:Alias != NULL
                        RETURN oFld:Alias
                    ELSE
                        RETURN oFld:Name
                    ENDIF
                CASE DbFieldInfo.DBS_PROPERTIES
                    RETURN 5
                END SWITCH
            ENDIF
            RETURN NULL

			
		VIRTUAL METHOD GetField(nFldPos AS INT) AS RDDFieldInfo
			IF SELF:_FieldIndexValidate(nFldPos)
				RETURN SELF:_Fields[nFldPos]
			ENDIF          
			RETURN NULL
			
			
			/// <inheritdoc />
		VIRTUAL METHOD FieldName(nFldPos AS INT) AS STRING
			// Note that nFldPos is 1 based
			IF SELF:_FieldIndexValidate(nFldPos)
				RETURN SELF:_Fields[nFldPos-1]:Name
			ENDIF          
			RETURN String.Empty
			
			/// <inheritdoc />
		VIRTUAL METHOD GetValue(nFldPos AS INT) AS OBJECT
			// Note that nFldPos is 1 based
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
			// Note that nFldPos is 1 based
			RETURN _Memo:GetValueFile(nFldPos, fileName )
			
			/// <inheritdoc />
		VIRTUAL METHOD GetValueLength(nFldPos AS INT) AS INT
			// Note that nFldPos is 1 based
			RETURN _Memo:GetValueLength(nFldPos )
			
			/// <inheritdoc />
		VIRTUAL METHOD Flush( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD GoCold( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD GoHot( ) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
			// Note that nFldPos is 1 based
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
			// Note that nFldPos is 1 based
			RETURN _Memo:PutValueFile(nFldPos, fileName )
			
			/// <inheritdoc />
		VIRTUAL METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD Lock(uiMode REF DbLockInfo) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD UnLock(oRecId AS OBJECT) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL METHOD CloseMemFile( ) AS LOGIC
			RETURN SELF:_Memo:CloseMemFile()
			
			/// <inheritdoc />
		VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
			RETURN SELF:_Memo:CreateMemFile(info)
			
			/// <inheritdoc />
		VIRTUAL METHOD OpenMemFile( info AS DbOpenInfo) AS LOGIC
			RETURN SELF:_Memo:OpenMemFile(info )
			
			#region Orders Not implemented
			
			/// <inheritdoc />
			VIRTUAL METHOD OrderCondition(info AS DbOrderCondInfo) AS LOGIC
				RETURN SELF:_Order:OrderCondition(info)
				
				/// <inheritdoc />
			VIRTUAL METHOD OrderCreate(info AS DbOrderCreateInfo) AS LOGIC
				RETURN SELF:_Order:OrderCreate(info)
				
				/// <inheritdoc />
			VIRTUAL METHOD OrderDestroy(info AS DbOrderInfo) AS LOGIC
				RETURN SELF:_Order:OrderDestroy(info)
				
				/// <inheritdoc />
			VIRTUAL METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
				/* CA-Cl*pper does not generate RT error when default ORDERINFO() method
				* is called
				*/
				RETURN SELF:_Order:OrderInfo(nOrdinal, info)
				
				/// <inheritdoc />
			VIRTUAL METHOD OrderListAdd(info AS DbOrderInfo) AS LOGIC
				RETURN SELF:_Order:OrderListAdd(info)
				
				/// <inheritdoc />
			VIRTUAL METHOD OrderListDelete(info AS DbOrderInfo) AS LOGIC
				RETURN SELF:_Order:OrderListDelete(info)
				
				/// <inheritdoc />
			VIRTUAL METHOD OrderListFocus(info AS DbOrderInfo) AS LOGIC
				RETURN SELF:_Order:OrderListFocus(info)
				
				/// <inheritdoc />
			VIRTUAL METHOD OrderListRebuild( ) AS LOGIC
				RETURN SELF:_Order:OrderListRebuild()
				
				/// <inheritdoc />
			VIRTUAL METHOD Seek(info AS DbSeekInfo) AS LOGIC
				RETURN SELF:_Order:Seek(info)
				#endregion
				
				
			#region Relations
			
			/// <inheritdoc />
			VIRTUAL METHOD ChildEnd(info AS DbRelInfo) AS LOGIC
				SELF:_Parents --
				RETURN TRUE
				
				/// <inheritdoc />
			VIRTUAL METHOD ChildStart(info AS DbRelInfo) AS LOGIC
				SELF:_Parents ++
				RETURN TRUE
				
				/// <inheritdoc />
			VIRTUAL METHOD ChildSync(info AS DbRelInfo) AS LOGIC
				THROW NotImplementedException{__ENTITY__}

			PRIVATE METHOD isChildPredicate( info AS DbRelInfo ) AS LOGIC
				// Is Child ?
				RETURN ( info:Child == SELF )
								
				/// <inheritdoc />
			VIRTUAL METHOD ClearRel( ) AS LOGIC
				LOCAL isOk AS LOGIC
				//
				isOk := TRUE
				//
				FOREACH info AS DbRelInfo IN SELF:_Relations
					isOk := isOk .AND. info:Child:ChildEnd( info )
				NEXT
				SELF:_Relations:Clear()
				RETURN isOk
				
				/// <inheritdoc />
			VIRTUAL METHOD ForceRel( ) AS LOGIC
				THROW NotImplementedException{__ENTITY__}
				
				/// <inheritdoc />
			VIRTUAL METHOD RelArea(nRelNum AS DWORD) AS DWORD
				LOCAL areaNum := 0 AS DWORD
				IF ( nRelNum < SELF:_Relations:Count )
					areaNum := SELF:_Relations[ (INT)nRelNum ]:Child:Area
				ENDIF
				RETURN areaNum
				
				/// <inheritdoc />
			VIRTUAL METHOD RelEval(relinfo AS DbRelInfo) AS LOGIC
                // Evaluate block in the Area of the Parent
                RETURN relinfo:Parent:EvalBlock(relinfo:Block) != NULL
				
				/// <inheritdoc />
			VIRTUAL METHOD RelText(nRelNum AS DWORD) AS STRING
				LOCAL textRelation := "" AS STRING
				IF ( nRelNum < SELF:_Relations:Count )
					textRelation := SELF:_Relations[ (INT)nRelNum ]:Key
				ENDIF
				RETURN textRelation
				
				/// <inheritdoc />
			VIRTUAL METHOD SetRel(info AS DbRelInfo) AS LOGIC
				IF !SELF:_Relations:Contains( info )
					SELF:_Relations:Add( info )
                    info:Compile()
				ENDIF
				RETURN info:Child:ChildStart( info )

				/// <inheritdoc />
			VIRTUAL METHOD SyncChildren( ) AS LOGIC
				LOCAL isOk AS LOGIC
				//
				isOk := TRUE
				//
				FOREACH info AS DbRelInfo IN SELF:_Relations
					isOk := info:Child:ChildSync( info )
					IF !isOk
						EXIT
					ENDIF
				NEXT
				RETURN isOk
				
				#endregion
				
			#region Trans not implemented
			
			/// <inheritdoc />
			VIRTUAL METHOD Trans(info AS DbTransInfo) AS LOGIC
			    LOCAL cbFor     := info:Scope:ForBlock AS ICodeblock
                LOCAL cbWhile   := info:Scope:WhileBlock AS ICodeblock
                LOCAL result    := TRUE AS LOGIC
                LOCAL lQualified:= TRUE AS LOGIC
                LOCAL lLimit    := TRUE AS LOGIC
                LOCAL nRecno    := 0    AS LONG
                IF SELF:_Relations:Count > 0
                    SELF:ForceRel()
                ENDIF
                IF info:Scope:RecId != NULL
                    nRecno := Convert.ToInt32(info:Scope:RecId)
                    result := SELF:Goto(nRecno)
                    lLimit := TRUE
                ELSEIF info:Scope:NextCount != 0
                    lLimit := TRUE
                    nRecno := info:Scope:NextCount
                    IF nRecno < 1
                        RETURN TRUE
                    ENDIF
                ELSE
                    lLimit  := FALSE
                    IF cbWhile == NULL .AND. ! info:Scope:Rest
                        result := SELF:GoTop()
                    ENDIF
                ENDIF
                DO WHILE result .AND. ! SELF:_Eof 
                    IF cbWhile != NULL
                        IF ! (LOGIC) SELF:EvalBlock(cbWhile)
                            EXIT
                        ENDIF
                    ENDIF
                    IF cbFor != NULL
                        lQualified := (LOGIC) SELF:EvalBlock(cbFor)
                    ELSE
                        lQualified := TRUE
                    ENDIF
                    IF result .AND. lQualified
                        result := SELF:TransRec(info)
                    ENDIF
                    IF lLimit
                        nRecno -= 1
                        IF nRecno == 0
                            EXIT
                        ENDIF
                    ENDIF
                    IF result 
                        SELF:Skip(1)
                    ENDIF
                ENDDO 
                RETURN result
				
				/// <inheritdoc />
			VIRTUAL METHOD TransRec(info AS DbTransInfo) AS LOGIC
                LOCAL oDest  AS IRDD
                LOCAL result AS LOGIC
                LOCAL oValue AS OBJECT
                oDest := info:Destination
                result := oDest:Append(TRUE)
                IF _AND(info:Flags , DbTransInfo.PutRec) != 0
                    VAR buffer := SELF:GetRec()
                    result := oDest:PutRec(buffer)
                ELSE
                    FOR VAR i := 0 TO info:ItemCount
                        LOCAL oItem AS DbTransItem
                        oItem  := info:Items[i]
                        oValue := SELF:GetValue(oItem:Source)
                        result := oDest:PutValue(oItem:Destination, oValue)
                        IF ! result
                            EXIT
                        ENDIF
                    NEXT
                ENDIF
                IF result .AND. SELF:Deleted
                    result := oDest:Delete()
                ENDIF
                RETURN result
				#endregion
				
			/// <inheritdoc />
		VIRTUAL METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
			THROW NotImplementedException{__ENTITY__}

        PRIVATE STATIC oCbType AS System.Type     

        PRIVATE STATIC METHOD FindCbType AS VOID
            FOREACH VAR asm IN AppDomain.CurrentDomain:GetAssemblies()
                IF asm:GetName():Name:ToLower() == "xsharp.rt"
                    oCbType := asm:GetType("XSharp._Codeblock")
                    IF oCbType == NULL
                        FOREACH VAR oT IN asm:GetTypes()
                            IF oT:FullName:ToLower() == "xsharp._codeblock"
                                oCbType := oT
                                EXIT
                            ENDIF
                        NEXT
                    ENDIF
                ENDIF
                IF oCbType != NULL
                    EXIT
                ENDIF
            NEXT
            RETURN

			/// <inheritdoc />
		VIRTUAL METHOD Compile(sBlock AS STRING) AS ICodeblock
			LOCAL oBlock := NULL AS ICodeblock
			TRY
				LOCAL oC AS IMacroCompiler
				oC          := XSharp.RuntimeState.MacroCompiler
				LOCAL oType := typeof(Workarea) AS System.Type
				IF oC != NULL
					LOCAL isBlock       AS LOGIC
                    LOCAL addsMemvars   AS LOGIC
					oBlock := oC:Compile(sBlock, TRUE, oType:Module, OUT isBlock, OUT addsMemVars)
                    // Convert to _CodeBlock when needed
                    IF oBlock IS XSharp.RuntimeCodeBlock
                        IF (oCbType == NULL)
                            FindCbType()
                        ENDIF
                        IF oCbType != NULL
                            oBlock := Activator.CreateInstance(oCbType, <OBJECT>{oBlock, sBlock, isBlock, addsMemVars})
                        ENDIF
                    ENDIF
				ENDIF
			CATCH e AS Exception
				XSharp.RuntimeState.LastRddError := e
			END TRY
			RETURN oBlock
			
			/// <inheritdoc />
		VIRTUAL METHOD EvalBlock(oBlock AS ICodeblock) AS OBJECT
				LOCAL currentWk AS DWORD
                LOCAL result AS OBJECT
				currentWk := XSharp.RuntimeState.Workareas:CurrentWorkAreaNO
                // Only switch workarea when needed
                IF currentWk != SELF:Area
				    TRY
					    XSharp.RuntimeState.CurrentWorkArea := SELF:Area
                        SELF:_EvalResult := result := oBlock:EvalBlock()
                    CATCH
                        THROW
				    FINALLY
					    XSharp.RuntimeState.Workareas:CurrentWorkAreaNO := currentWk
                    END TRY
                ELSE
                    SELF:_EvalResult := result := oBlock:EvalBlock()
                ENDIF
				RETURN result 
			
			/// <inheritdoc />
		VIRTUAL METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
			LOCAL oResult AS OBJECT
			// todo check basic implementation
			SWITCH nOrdinal
			    CASE DbInfo.DBI_ISDBF
				CASE DbInfo.DBI_CANPUTREC
				CASE DbInfo.DBI_ISFLOCK
				CASE DbInfo.DBI_TRANSREC
				CASE DbInfo.DBI_RM_SUPPORTED
					oResult := FALSE      
				CASE DbInfo.DBI_SHARED
					oResult := SELF:_Shared
				CASE DbInfo.DBI_ISREADONLY
                CASE DbInfo.DBI_READONLY
					oResult := SELF:_ReadOnly
				CASE DbInfo.DBI_GETDELIMITER
					oResult := SELF:_Delimiter
				CASE DbInfo.DBI_SEPARATOR
					oResult := SELF:_Separator
				CASE DbInfo.DBI_SETDELIMITER            
					oResult := SELF:_Separator		
					IF oNewValue != NULL .AND. oNewValue:GetType() == TYPEOF(STRING)
						SELF:_Separator	:= (STRING) oNewValue
					ENDIF
				CASE DbInfo.DBI_DB_VERSION
				CASE DbInfo.DBI_RDD_VERSION
					oResult := ""
				CASE DbInfo.DBI_GETHEADERSIZE
				CASE DbInfo.DBI_LOCKCOUNT
					oResult := 0     
				CASE DbInfo.DBI_GETRECSIZE
					oResult := SELF:_RecordLength
				CASE DbInfo.DBI_LASTUPDATE
					oResult := DateTime.MinValue 
				CASE DbInfo.DBI_GETLOCKARRAY
					oResult := <DWORD>{}
				CASE DbInfo.DBI_BOF           
					oResult := SELF:_BOF
				CASE DbInfo.DBI_EOF           
					oResult := SELF:_EOF   
				CASE DbInfo.DBI_DBFILTER
                    IF SELF:_FilterInfo != NULL
					    oResult := SELF:_FilterInfo:FilterText
                    ELSE
                        oResult := String.Empty
                    ENDIF
				CASE DbInfo.DBI_FOUND
					oResult := SELF:_Found
				CASE DbInfo.DBI_FCOUNT
					oResult := (INT) _Fields?:Length
				CASE DbInfo.DBI_ALIAS
					oResult := _Alias
				CASE DbInfo.DBI_FULLPATH
					oResult := SELF:_FileName
                CASE DbInfo.DBI_CHILDCOUNT
                    oResult := SELF:_Relations:Count
				OTHERWISE
					oResult := NULL
				END SWITCH
			RETURN oResult
			
			
			/// <inheritdoc />
		VIRTUAL METHOD RecInfo(nOrdinal AS LONG, oRecID AS OBJECT, oNewValue AS OBJECT) AS OBJECT  

			RETURN NULL
			
			/// <inheritdoc />
		VIRTUAL METHOD Sort(info AS DbSortInfo) AS LOGIC
			THROW NotImplementedException{__ENTITY__}
			
			/// <inheritdoc />
		VIRTUAL PROPERTY Alias AS STRING GET _Alias SET _Alias := VALUE
		
		/// <inheritdoc />
		VIRTUAL PROPERTY Area AS DWORD GET _Area SET _Area := VALUE 
		
		/// <inheritdoc />
		VIRTUAL PROPERTY BoF AS LOGIC GET _Bof
		
		/// <inheritdoc />
		VIRTUAL PROPERTY Deleted AS LOGIC GET FALSE
		
		/// <inheritdoc />
		VIRTUAL PROPERTY Driver AS STRING GET "Workarea"
		
		/// <inheritdoc />
		VIRTUAL PROPERTY EoF AS LOGIC GET _Eof
		
		/// <inheritdoc />
		VIRTUAL PROPERTY Exclusive AS LOGIC GET FALSE
		
		/// <inheritdoc />
		VIRTUAL PROPERTY FieldCount AS LONG GET (LONG) _Fields?:Length
		
		/// <inheritdoc />
		VIRTUAL PROPERTY FilterText AS STRING GET _FilterInfo?:FilterText
		
		/// <inheritdoc />
		VIRTUAL PROPERTY Found AS LOGIC GET _Order:Found SET _Order:Found := VALUE
		
		
		/// <inheritdoc />
		VIRTUAL PROPERTY RecCount AS INT GET 0
		
		/// <inheritdoc />
		VIRTUAL PROPERTY RecId AS OBJECT GET NULL
		/// <inheritdoc />
		VIRTUAL PROPERTY RecNo AS LONG GET   0	
		
		/// <inheritdoc />
		VIRTUAL PROPERTY Shared AS LOGIC GET FALSE
		
		/// <inheritdoc />
		VIRTUAL PROPERTY SysName AS STRING GET TYPEOF(Workarea):ToString()
		
	END CLASS

END NAMESPACE


