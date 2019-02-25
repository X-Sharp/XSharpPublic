//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Diagnostics
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Collections.Generic

BEGIN NAMESPACE XSharp.RDD.CDX

    INTERNAL DELEGATE ValueBlock( sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC

    INTERNAL PARTIAL SEALED CLASS CdxTag IMPLEMENTS IRddSortWriter

        // Methods for Creating Indices
        PUBLIC METHOD Create(createInfo AS DbOrderCreateInfo ) AS LOGIC
            LOCAL ordCondInfo AS DbOrderCondInfo
            LOCAL isOk AS LOGIC
            LOCAL orderInfo AS DbOrderInfo
            LOCAL hasForCond AS LOGIC
            
            ordCondInfo := SELF:_oRdd:_OrderCondInfo
            IF string.IsNullOrEmpty(createInfo:BagName)
                SELF:_oRDD:_dbfError(  SubCodes.EDB_CREATEINDEX, GenCode.EG_ARG,"OrdCreate", "Missing Orderbag Name")
                RETURN FALSE
            ENDIF
            isOk := SELF:_oRdd:GoCold()
            orderInfo := DbOrderInfo{}
            IF !ordCondInfo:Scoped
                orderInfo:AllTags := TRUE
                SELF:_oRdd:OrderListDelete(orderInfo)
            ENDIF
            IF ordCondInfo:ForBlock != NULL
                hasForCond := TRUE
                SELF:_ForCodeBlock := ordCondInfo:ForBlock
            ELSE
                hasForCond := FALSE
            ENDIF
            SELF:_KeyExpr := createInfo:Expression
            IF ordCondInfo != NULL .AND. ordCondInfo:ForExpression != NULL
                SELF:_ForExpr := ordCondInfo:ForExpression
            ELSE
                SELF:_ForExpr := string.Empty
            ENDIF
            SELF:_oRdd:__Goto(1)
            IF ! SELF:EvaluateExpressions()
                RETURN FALSE
            ENDIF
            SELF:_orderName := (STRING)createInfo:Order
            IF string.IsNullOrEmpty(SELF:_orderName)
                SELF:_oRDD:_dbfError( GenCode.EG_ARG, SubCodes.EDB_CREATEINDEX)
            ENDIF
            IF !isOk .OR. SELF:_keySize == 0
                SELF:Close()
                SELF:_oRDD:_dbfError(  SubCodes.EDB_CREATEINDEX, GenCode.EG_ARG,"OrdCreate", "Missing Order Name")
                RETURN FALSE
            ENDIF
            IF SELF:_keySize > 0
                SELF:_newKeyBuffer   := BYTE[]{_Keysize+1}
            ENDIF
            
            SELF:_Unique := createInfo:Unique
            SELF:_Ansi := SELF:_oRdd:_Ansi
            SELF:_Conditional := FALSE
            SELF:_Descending := FALSE
            SELF:_Custom := ordCondInfo:Scoped
            IF ordCondInfo:Active
                SELF:_Descending := ordCondInfo:Descending
                IF hasForCond .AND. !string.IsNullOrEmpty(ordCondInfo:ForExpression)
                    SELF:_Conditional := TRUE
                ENDIF
            ENDIF
            IF ! SELF:_HeaderCreate()
                SELF:Close()
                SELF:_oRdd:_dbfError(GenCode.EG_CREATE,  SubCodes.ERDD_WRITE,"OrdCreate", "Could not write Header ")
                RETURN FALSE
            ENDIF
            IF SELF:_oRdd:RecCount == 0
                isOk := SELF:_CreateEmpty()
            ELSEIF !SELF:Unique .AND. !SELF:_Conditional .AND. !ordCondInfo:Scoped
                isOk := SELF:_CreateNormalIndex()
            ELSE
                isOk := SELF:_CreateUnique(ordCondInfo)
            ENDIF
            IF !isOk
                SELF:Flush()
                SELF:Close()
                RETURN isOk
            ENDIF
            SELF:Flush()
            RETURN isOk

        PRIVATE METHOD _HeaderCreate() AS LOGIC
            SELF:_Header            := CdxTagHeader{_bag, -1 ,_orderName}
            SELF:_Header:Tag        := SELF
            SELF:_Header:Descending := SELF:_Descending
            SELF:_Header:Version    := SELF:_version
            SELF:_Header:Signature  := 1
            SELF:_Header:RootPage   := 0
            SELF:_Header:FreeList   := 0
            SELF:_Header:Version    := 0
            SELF:_Header:KeySize    := _keySize
            SELF:_Header:KeyExprPos := 0
            SELF:_Header:KeyExprLen := (WORD)(_KeyExpr:Length + 1)
            SELF:_Header:ForExprPos := (WORD) (SELF:_Header:KeyExprLen )
            SELF:_Header:KeyExpression := _KeyExpr
            VAR options := CdxOptions.Compact + CdxOptions.Tag
            IF SELF:_Unique
                options |= CdxOptions.Unique
            ENDIF
            IF SELF:_Custom
                options |= CdxOptions.Custom
            ENDIF
            IF SELF:_Conditional
                SELF:_Header:ForExprLen    := (WORD)(_ForExpr:Length +1)
                SELF:_Header:ForExpression := _ForExpr
                options |= CdxOptions.HasFor
            ELSE
                SELF:_Header:ForExprLen    := 1
            ENDIF
            SELF:_Header.Options := options
            RETURN SELF:_Header:Write()
            
        INTERNAL METHOD _determineSize(toConvert AS OBJECT ) AS LOGIC

            VAR type := SELF:_oRdd:_getUsualType(toConvert)
            SWITCH type
            CASE __UsualType.String
                SELF:_keySize := (WORD) ((STRING)toConvert):Length
            CASE __UsualType.Long
            CASE __UsualType.Float
            CASE __UsualType.Date
                SELF:_keySize := 8      // all stored as numeric
            CASE __UsualType.Logic
                SELF:_keySize := 1
            OTHERWISE
                SELF:_keySize := 0
                RETURN FALSE
            END SWITCH
            
            RETURN TRUE
            
        PRIVATE METHOD _CondCreate(ordCondInfo AS DbOrderCondInfo ) AS LOGIC
            LOCAL isOk          := TRUE AS LOGIC
            LOCAL leadingOrder  := NULL AS CdxTag
            LOCAL lUseOrder     := FALSE AS LOGIC
            LOCAL hasWhile      := FALSE AS LOGIC
            LOCAL hasEvalBlock  := FALSE AS LOGIC
            LOCAL record        := 1 AS LONG
            LOCAL count         := 1 AS LONG
            LOCAL toDo          := 0 AS LONG
            LOCAL done          := 0 AS LONG
            LOCAL nextRecord    := 0 AS LONG
            LOCAL start         := 0 AS LONG
            LOCAL result        := FALSE AS LOGIC
            
            start := ordCondInfo:StartRecNo
            IF ordCondInfo:Scoped
                IF ordCondInfo:StartRecNo > 0
                    record := ordCondInfo:StartRecNo
                ENDIF
                IF SELF:_oRdd:_indexList:CurrentOrder != NULL
                    leadingOrder := SELF:_oRdd:_indexList:CurrentOrder
                    lUseOrder    := leadingOrder != NULL
                ENDIF
                IF ordCondInfo:All
                    // All overrules start record no
                    IF lUseOrder
                        // start from first record in index
                        record := leadingOrder:_locateKey(NULL, 0, SearchMode.Top)
                    ELSE
                        record := 1 // start from first record in file
                    ENDIF
                ENDIF
            ENDIF
            IF ordCondInfo:RecNo > 0
                // start with record indicated. This overrules ALL again
                record := ordCondInfo:RecNo
                toDo := 1
            ENDIF
            IF ordCondInfo:NextCount > 0
                toDo := ordCondInfo:NextCount
            ENDIF
            SELF:_oRdd:__Goto(record)
            // IF record is EOF then do nothing
            IF !SELF:_oRdd:_isValid .AND. !SELF:_oRdd:_Eof
                SELF:_oRdd:__Goto(start)
                SELF:ClearStack()
                RETURN FALSE
            ENDIF
            IF ordCondInfo:WhileBlock != NULL
                hasWhile := TRUE
            ENDIF
            IF ordCondInfo:EvalBlock != NULL
                hasEvalBlock := TRUE
            ENDIF
            IF lUseOrder .AND. leadingOrder:_topStack != 0
                result := leadingOrder:_GoToRecno(SELF:_RecNo)
                IF !result
                    RETURN result
                ENDIF
            ENDIF
            DO WHILE TRUE
                IF hasWhile
                    isOk := TRUE
                    TRY
                        isOk := (LOGIC) SELF:_oRdd:EvalBlock(ordCondInfo:WhileBlock)
                    CATCH
                        SELF:_oRdd:_dbfError(SubCodes.ERDD_KEY_EVAL, GenCode.EG_DATATYPE, SELF:fileName)
                        isOk := FALSE
                    END TRY
                    IF ! isOk
                        EXIT
                    ENDIF
                    
                ENDIF
                IF !SELF:_keyUpdate( SELF:_RecNo, TRUE)
                    EXIT
                ENDIF
                IF hasEvalBlock
                    IF count >= ordCondInfo:StepSize
                        isOk := TRUE
                        TRY
                            isOk := (LOGIC) SELF:_oRdd:EvalBlock(ordCondInfo:EvalBlock)
                        CATCH
                            SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_EVAL,GenCode.EG_DATATYPE, SELF:fileName)
                            isOk := FALSE
                        END TRY
                        IF ! isOk
                            EXIT
                        ENDIF
                        count := 1
                    ELSE
                        count++
                    ENDIF
                ENDIF
                done++
                IF lUseOrder 
                    nextRecord := leadingOrder:_getNextKey(SkipDirection.Forward)
                ELSE
                    nextRecord := SELF:_RecNo + 1
                ENDIF
                SELF:_oRdd:__Goto( nextRecord)
                IF todo != 0 .AND. done >= todo
                    EXIT
                ENDIF
                IF SELF:_oRdd:_Eof 
                    EXIT
                ENDIF
            ENDDO

            SELF:_oRdd:__Goto(start)
            SELF:ClearStack()
            SELF:Flush()
            RETURN TRUE

        PRIVATE METHOD _CreateEmpty() AS LOGIC
            LOCAL oPage AS CdxLeafPage
            oPage := SELF:_newLeafPage()
            SELF:_finishCreate()
            RETURN TRUE

        INTERNAL METHOD _InitSort(lRecCount AS LONG, sourceIndex OUT LONG, lAscii OUT LOGIC) AS RDDSortHelper
            LOCAL sorting AS RddSortHelper
            LOCAL sortInfo AS DbSortInfo
            LOCAL fType  := 0 AS DbFieldType
            sourceIndex := 0
            lAscii := FALSE
            sortInfo := DbSortInfo{0,1}     // 0 trans items, 1 sort item
            //SELF:_levelsCount := 1
            IF SELF:_SingleField != -1
                fType := SELF:_oRdd:_Fields[SELF:_SingleField]:fieldType
                // 'C', 'N', 'D'
                SWITCH fType
                CASE DbFieldType.Character
                CASE DbFieldType.Number
                CASE DbFieldType.Date
                    sourceIndex := SELF:_oRdd:_Fields[SELF:_SingleField]:OffSet
                OTHERWISE
                    fType := 0
                END SWITCH
            ENDIF
            
            sorting := RddSortHelper{sortInfo, lRecCount}
            sortInfo:Items[0]:Length := SELF:_keySize
            IF SELF:_KeyExprType == __UsualType.String
                lAscii := FALSE
                sortInfo:Items[0]:Flags := DbSortFlags.Default
            ELSE
                lAscii := TRUE
                sortInfo:Items[0]:Flags := DbSortFlags.Ascii
            ENDIF
            //IF SELF:_oRdd:_OrderCondInfo:Descending
            //    sortInfo:Items[0]:Flags += DbSortFlags.Descending
            //ENDIF
            sortInfo:Items[0]:OffSet := 0
            RETURN sorting

        INTERNAL METHOD _CreateNormalIndex() AS LOGIC
            LOCAL evalCount AS LONG
            LOCAL lRecCount AS LONG
            LOCAL result AS LOGIC
            LOCAL hasBlock AS LOGIC
            LOCAL ic AS CdxSortCompare
            LOCAL sourceIndex := 0 AS LONG
            LOCAL lAscii AS LOGIC
           
            evalCount := 0
            lRecCount := SELF:_oRdd:RecCount
            IF lRecCount == 0
                RETURN SELF:_CreateEmpty()
            ENDIF
            VAR sortHelper := SELF:_initSort(lRecCount, OUT sourceIndex, OUT lAscii)
            hasBlock    := SELF:_oRdd:_OrderCondInfo:EvalBlock != NULL
            evalCount := 1
            SELF:_oRdd:GoTo(1)
            VAR buffer := BYTE[]{ SELF:_keySize }
            REPEAT
                result := TRUE
                SELF:_oRdd:ReadRecord()
                result := getKeyValue(sourceIndex, buffer)
                IF !result
                    EXIT
                ENDIF
                VAR toSort := SortRecord{buffer, SELF:_RecNo}
                sortHelper:Add(toSort)
                IF hasBlock
                    IF evalCount >= SELF:_oRdd:_OrderCondInfo:StepSize
                        TRY
                            SELF:_oRdd:EvalBlock(SELF:_oRdd:_OrderCondInfo:EvalBlock)
                        CATCH
                            result := FALSE
                        END TRY
                        evalCount := 1
                    ELSE
                        evalCount++
                    ENDIF
                ENDIF
                result := SELF:_oRdd:GoTo(SELF:_RecNo + 1)
            UNTIL ! (result .AND. SELF:_oRdd:_isValid)
            IF result
                IF lAscii
                    ic := CdxSortCompareAscii{SELF:_oRdd, sortHelper}
                ELSE
                    ic := CdxSortCompareDefault{SELF:_oRdd, sortHelper}
                ENDIF
                result := sortHelper:Sort(ic)
            ENDIF
            SELF:_oneItem := CdxNode{SELF:_keySize, 0}
            SELF:ClearStack()

            IF result
                SELF:_newLeafPage()
                result := sortHelper:Write(SELF)
            ENDIF
            SELF:_finishCreate()
            sortHelper:Clear()
            sortHelper := NULL
            RETURN result
            
            // IRddSortWriter Interface, used by RddSortHelper
        PUBLIC METHOD WriteSorted(si AS DbSortInfo , record AS SortRecord) AS LOGIC
          // place item on current leaf node.
            // When Leafnode is full then allocate a new leaf node
            LOCAL oLeaf AS CdxLeafPage
            oLeaf := _currentLeaf
            IF ! oLeaf:Add(record:Recno, record:Data)
                // this means that it did not fit
                oLeaf := SELF:_newLeafPage()
                IF ! oLeaf:Add(record:Recno, record:Data)
                    // Exception, this should not happen
                    RETURN FALSE
                ENDIF
            ENDIF
            RETURN TRUE            
            
            
        INTERNAL METHOD _CreateUnique(ordCondInfo AS DbOrderCondInfo ) AS LOGIC
            LOCAL Ok AS LOGIC
            Ok := SELF:_CreateEmpty()
            IF Ok
                IF ordCondInfo:Active
                    RETURN SELF:_CondCreate(ordCondInfo)
                ENDIF
                SELF:_oRdd:GoTo(1)
                IF SELF:_oRdd:_isValid
                    REPEAT
                        SELF:_keyUpdate( SELF:_RecNo, TRUE)
                        SELF:_oRdd:GoTo(SELF:_RecNo + 1)
                    UNTIL ! SELF:_oRdd:_isValid
                ENDIF
                SELF:ClearStack()
            ENDIF
            SELF:Flush()
            RETURN Ok


        PRIVATE METHOD _finishCreate() AS LOGIC
            // at the end of the index creation we will create the branch pages (when needed)
            // that point to the leaf pages that were allocated in this process.
            // To do that:
            // - first write each leaf node to disk, so we have a page number
            //   to do that quickly we write from Right to left, so we only have to
            //   write each page once
            // - based on the number of leaf pages we know how many branch keys we have
            //   and based on that we can determine the # of levels we need.
            //   when there was only one leaf page then we don't create a branch, otherwise
            //   we create as many branches as necessary.
            LOCAL oLeaf AS CdxLeafPage
            LOCAL oLeafs   AS List<CdxLeafPage>
            oLeafs      := List<CdxLeafPage>{}
            oLeaf       := _currentLeaf
            DO WHILE oLeaf != NULL
                oLeaf:Write()
                oLeafs:Insert(0, oLeaf)
                oLeaf := oLeaf:Left

            ENDDO
            IF oLeafs:Count > 1
                VAR nodes := List<CdxPageNode>{}
                // one level is needed
                // we collect the last nodes on every leaf page and then create branch pages from this list
                FOREACH VAR leaf IN oLeafs
                    VAR node := leaf:LastNode
                    nodes:Add(node)
                NEXT
                LOCAL max AS LONG
                max := SELF:MaxKeysPerPage
                LOCAL oBranch AS CdxBranchePage
                IF oLeafs:Count <= max
                    oBranch := SELF:_newBranchPage(nodes, 0)
                ELSE
                    // implement later becayse this will create a list of branches and a level above it.
                ENDIF
            ELSE
                SELF:Header:RootPage := oLeafs[0]:PageNo
                SELF:Header:Write()
            ENDIF
            RETURN TRUE

        PRIVATE _currentLeaf AS CdxLeafPage

        PRIVATE METHOD _newBranchPage(nodes AS IList<CdxPageNode>, nOffSet AS LONG) AS CdxBranchePage
           LOCAL oBranch AS CdxBranchePage 
            LOCAL buffer AS BYTE[]
            LOCAL last AS Int32
            buffer := _bag:AllocBuffer()
            oBranch := CdxBranchePage{_bag, -1, buffer, _keySize}
            last  := Math.Min(nodes:Count - nOffSet, oBranch:MaxKeys)
            FOR VAR i := 0 TO last -1
                oBranch:Add( nodes[nOffset+i] )
            NEXT
            RETURN oBranch



        PRIVATE METHOD _newLeafPage() AS CdxLeafPage
            LOCAL oLeaf AS CdxLeafPage
            LOCAL buffer AS BYTE[]
            buffer := _bag:AllocBuffer()
            oLeaf := CdxLeafPage{_bag, -1, buffer, _keySize}
            oLeaf:Initialize(_keySize)
            oLeaf:Right  := NULL
            oLeaf:Left   := _currentLeaf
            IF _currentLeaf != NULL
                _currentLeaf:Right  := oLeaf
            ENDIF
            _currentLeaf := oLeaf
            oLeaf:Tag := SELF
            RETURN oLeaf
 
        PUBLIC METHOD Truncate() AS LOGIC
            // Find all pages of the tag and delete them
            // then also delete the tag header and return everything to the OrderBag 
            RETURN TRUE
            
    END CLASS
END NAMESPACE
