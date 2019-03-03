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
        PRIVATE _sorter AS CdxSortHelper
        
        // Methods for Creating Indices
        PUBLIC METHOD Create(createInfo AS DbOrderCreateInfo ) AS LOGIC
            LOCAL ordCondInfo AS DbOrderCondInfo
            LOCAL isOk AS LOGIC
            LOCAL orderInfo AS DbOrderInfo
            LOCAL hasForCond AS LOGIC
            LOCAL ic AS CdxSortCompare

            ordCondInfo := SELF:_oRdd:_OrderCondInfo
            IF string.IsNullOrEmpty(createInfo:BagName)
                SELF:_oRDD:_dbfError(  SubCodes.EDB_CREATEINDEX, GenCode.EG_ARG,"OrdCreate", "Missing Orderbag Name")
                RETURN FALSE
            ENDIF
            isOk := SELF:_oRdd:GoCold()
            orderInfo := DbOrderInfo{}
            IF !ordCondInfo:Scoped
                //orderInfo:AllTags := FALSE
                //SELF:_oRdd:OrderListDelete(orderInfo)
                SELF:_oRdd:OrderListFocus(orderInfo)
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
            SELF:_orderName := SELF:_orderName:ToUpper()

            IF !isOk .OR. SELF:_keySize == 0
                SELF:Close()
                SELF:_oRDD:_dbfError(  SubCodes.EDB_CREATEINDEX, GenCode.EG_ARG,"OrdCreate", "Missing Order Name")
                RETURN FALSE
            ENDIF

            // now verify if the order already exists in the bag and when so, then delete it from the orderbag
            FOREACH VAR tag IN SELF:OrderBag:Tags
                IF string.Compare(tag:OrderName, SELF:OrderName, StringComparison.OrdinalIgnoreCase) == 0
                    SELF:OrderBag:Destroy(tag)
                    EXIT
                ENDIF
            NEXT

            IF SELF:_keySize > 0
                SELF:AllocateBuffers()
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
            IF !SELF:Unique .AND. !SELF:_Conditional .AND. !ordCondInfo:Scoped
                isOk := SELF:_CreateNormalIndex()
            ELSE
                isOk := SELF:_CreateUnique(ordCondInfo)
            ENDIF
            IF isOk
                IF _sorter:Ascii
                    ic := CdxSortCompareAscii{SELF:_oRdd, _sorter}
                ELSE
                    ic := CdxSortCompareDefault{SELF:_oRdd, _sorter}
                ENDIF
                isOk := _sorter:Sort(ic)
            ENDIF
            IF isOk
                SELF:_sorter:StartWrite()
                isOk := _sorter:Write(SELF)
            ENDIF
            IF isOk
                SELF:_sorter:EndWrite()
            ENDIF
            SELF:_sorter := NULL
            SELF:ClearStack()

            IF !isOk
                SELF:Flush()
                SELF:Close()
                RETURN isOk
            ENDIF
            SELF:Flush()
            SELF:OrderBag:Flush()
            RETURN isOk
        PRIVATE METHOD _sortGetRecord() AS LOGIC
            // Get Key Values
            SELF:_oRdd:ReadRecord()
            VAR result := getKeyValue(_sorter:SourceIndex, _newValue:Key)
            IF result
                _sorter:Add(SortRecord{_newValue:Key, SELF:_RecNo})
            ENDIF
            RETURN result

        PRIVATE METHOD _HeaderCreate() AS LOGIC
            SELF:_Header            := CdxTagHeader{_bag, -1 ,_orderName}
            SELF:_Header:Tag        := SELF
            SELF:_Header:Descending := SELF:_Descending
            SELF:_Header:Version    := 0
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
            LOCAL includeRecord := TRUE AS LOGIC
            
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
                    IF ! SELF:_EvalBlock(ordCondInfo:WhileBlock, TRUE)
                        EXIT
                    ENDIF
                ENDIF
                IF SELF:_Conditional
                    includeRecord := SELF:_EvalBlock(ordCondInfo:ForBlock, TRUE)
                ENDIF
                IF includeRecord
                    IF ! SELF:_sortGetRecord()
                        EXIT
                    ENDIF
                ENDIF
                IF hasEvalBlock
                    IF count >= ordCondInfo:StepSize
                        IF ! SELF:_EvalBlock(ordCondInfo:EvalBlock,FALSE)
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
            // evaluate the block once more at eof
            IF hasEvalBlock
                SELF:_EvalBlock(ordCondInfo:EvalBlock,FALSE)
            ENDIF
            SELF:_oRdd:__Goto(start)
            SELF:ClearStack()
            SELF:Flush()
            RETURN TRUE

        PRIVATE METHOD _EvalBlock(oBlock AS ICodeBlock, lMustBeLogic AS LOGIC) AS LOGIC
            LOCAL isOk  := FALSE AS LOGIC
            LOCAL error := FALSE AS LOGIC
            TRY
                VAR res := SELF:_oRdd:EvalBlock(oBlock)
                IF res IS LOGIC 
                    isOk := (LOGIC) res
                ELSEIF lMustBeLogic
                    error := TRUE
                ELSE
                    isOk := TRUE
                ENDIF
            CATCH 
                error := TRUE
                isOk := FALSE
            END TRY
            IF error
                SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_EVAL,GenCode.EG_DATATYPE, SELF:fileName)
            ENDIF
            RETURN isOk
            
        INTERNAL METHOD _InitSort(lRecCount AS LONG) AS LOGIC
            LOCAL sortInfo AS DbSortInfo
            LOCAL fType  := 0 AS DbFieldType
            sortInfo := DbSortInfo{0,1}     // 0 trans items, 1 sort item
            SELF:_sorter := CdxSortHelper{SELF:_oRDD, sortInfo, lRecCount, SELF}
            IF SELF:_SingleField != -1
                fType := SELF:_oRdd:_Fields[SELF:_SingleField]:fieldType
                // 'C', 'N', 'D'
                SWITCH fType
                CASE DbFieldType.Character
                CASE DbFieldType.Number
                CASE DbFieldType.Date
                CASE DbFieldType.Logic
                    SELF:_sorter:SourceIndex := SELF:_oRdd:_Fields[SELF:_SingleField]:OffSet
                OTHERWISE
                    fType := 0
                    SELF:_sorter:SourceIndex := -1
                END SWITCH
            ENDIF
            sortInfo:Items[0]:Length := SELF:_keySize
            IF SELF:_KeyExprType == __UsualType.String .OR. SELF:_KeyExprType == __UsualType.LOGIC
                SELF:_sorter:AScii := FALSE
                sortInfo:Items[0]:Flags := DbSortFlags.Default
            ELSE
                SELF:_sorter:AScii := TRUE
                sortInfo:Items[0]:Flags := DbSortFlags.Ascii
            ENDIF
            sortInfo:Items[0]:OffSet := 0
            RETURN TRUE

        INTERNAL METHOD _CreateNormalIndex() AS LOGIC
            LOCAL evalCount AS LONG
            LOCAL lRecCount AS LONG
            LOCAL result    AS LOGIC
            LOCAL hasBlock  AS LOGIC
           
            evalCount := 0
            lRecCount := SELF:_oRdd:RecCount
            // create sorthelper 
            SELF:_initSort(lRecCount)
            IF lRecCount == 0
                RETURN TRUE
            ENDIF
            hasBlock    := SELF:_oRdd:_OrderCondInfo:EvalBlock != NULL
            evalCount := 1
            SELF:_oRdd:GoTo(1)
            result    := TRUE
            REPEAT
                IF ! SELF:_sortGetRecord()
                    EXIT
                ENDIF
                IF hasBlock
                    IF evalCount >= SELF:_oRdd:_OrderCondInfo:StepSize
                        IF ! SELF:_EvalBlock(SELF:_oRdd:_OrderCondInfo:EvalBlock, FALSE)
                            EXIT
                        ENDIF
                        evalCount := 1
                    ELSE
                        evalCount++
                    ENDIF
                ENDIF
                result := SELF:_oRdd:GoTo(SELF:_RecNo + 1)
            UNTIL ! (result .AND. SELF:_oRdd:_isValid)
            RETURN result
            
            // IRddSortWriter Interface, used by RddSortHelper
        PUBLIC METHOD WriteSorted(si AS DbSortInfo , record AS SortRecord) AS LOGIC
            RETURN _sorter:AddRecord(record:Recno, record:Data)
            
        INTERNAL METHOD _CreateUnique(ordCondInfo AS DbOrderCondInfo ) AS LOGIC
            LOCAL LRecCount AS LONG
            lRecCount := SELF:_oRdd:RecCount
            // create sorthelper
            SELF:_initSort(lRecCount)
            SELF:_sorter:StartWrite()
            IF ordCondInfo:Active
                RETURN SELF:_CondCreate(ordCondInfo)
            ENDIF
            SELF:_oRdd:GoTo(1)
            IF SELF:_oRdd:_isValid
                REPEAT
                    SELF:_oRdd:GoTo(SELF:_RecNo + 1)
                    IF ! SELF:_sortGetRecord()
                        EXIT
                    ENDIF
                UNTIL ! SELF:_oRdd:_isValid
            ENDIF
            SELF:ClearStack()
            SELF:Flush()
            RETURN TRUE


        PUBLIC METHOD Truncate() AS LOGIC
            // Find all pages of the tag and delete them
            // then also delete the tag header and return everything to the OrderBag 
            RETURN TRUE
            
    END CLASS
    INTERNAL CLASS CdxSortHelper INHERIT RddSortHelper
        INTERNAL PROPERTY CurrentLeaf    AS CdxLeafPage AUTO
        INTERNAL PROPERTY SourceIndex    AS INT AUTO
        INTERNAL PROPERTY Ascii          AS LOGIC AUTO
        PRIVATE _bag                     AS CdxOrderBag
        PRIVATE _tag                     AS CdxTag
        INTERNAL CONSTRUCTOR( rdd AS DBF, sortInfo   AS DbSortInfo , len AS LONG, tag AS CdxTag )
            SUPER(rdd, sortInfo, len)
            _tag := tag
            _bag := tag:OrderBag
            CurrentLeaf := NULL

        INTERNAL METHOD AddRecord(nRecno AS LONG, data AS BYTE[]) AS LOGIC
            VAR oLeaf    := CurrentLeaf
            // place item on current leaf node.
            // When Leafnode is full then allocate a new leaf node
            // and add this leaf to the parent
            _tag:_newValue:Recno := nRecno
            _tag:_newValue:Key   := data
            VAR result := oLeaf:Add(nRecno, data)
            IF result != CdxResult.OK
                IF result == CdxResult.SplitLeaf
                    result := CdxResult.AddLeaf
                ENDIF
                result := _tag.DoAction(result)
                oLeaf  := CurrentLeaf := _tag:CurrentLeaf
                result := oLeaf:Add(nRecno, data)
                // this may be another CdxResult.ExpandRecnos for a new page
                IF result != CdxResult.Ok
                    result := _tag.DoAction(result)
                    result := oLeaf:Add(nRecno, data)
                ENDIF
                IF result != CdxResult.OK
                    Error("CdxSortHelper.AddRecord","Could not add record to leaf")
                ENDIF
            ENDIF
            _tag:CurrentStack:Pos++
            RETURN TRUE

        INTERNAL METHOD StartWrite() AS LOGIC
            VAR page := SELF:_tag:NewLeafPage()
            SELF:_tag:PushPage(page)
            CurrentLeaf := page
            RETURN TRUE

        INTERNAL METHOD EndWrite() AS LOGIC
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
            LOCAL oLeaf    AS CdxLeafPage
            LOCAL oParent   := NULL AS CdxBranchPage
            oLeaf           := CurrentLeaf
            // Write the last key in leaf and its parents all the way up into the tree
            oParent  := _tag:GetParent(oLeaf)
            VAR node := oLeaf:LastNode
            DO WHILE oParent != NULL
                VAR result := oParent:Add(node)
                // It can happen and will happen that the last key does not fit on the parent
                // when that happens we will have to create a new parent page
                IF result != CdxResult.OK
                    _tag:DoAction(result)
                ENDIF
                /*
                SWITCH result
                CASE CdxResult.Ok
                    NOP
                CASE CdxResult.Split
                    oParent:Write()
                    oParent := SELF:NewBranchPage(oLeaf:LastNode, oParent)
                END SWITCH
                */
                node    := oParent:LastNode
                oParent := _tag:GetParent(oParent)
            ENDDO
            VAR rootPageNo := _tag:CurrentStack:Page
            DO WHILE _tag:CurrentStack:Page != 0
                rootPageNo := _tag:CurrentStack:Page
                _tag:Poppage()
            ENDDO 
            IF rootPageNo != 0
                VAR root := _tag:GetPage(rootPageNo)
                IF root != NULL
                    SetRoot(root)
                ENDIF
            ENDIF
            SELF:Clear()
            RETURN TRUE


        PRIVATE METHOD SetRoot(oPage AS CdxTreePage) AS VOID
            _tag:Header:RootPage := oPage:PageNo
            _tag:Header:Write()
            oPage:SetRoot()
            oPage:Write()
            RETURN

        PRIVATE METHOD Error(strFunction AS STRING, strMessage AS STRING) AS VOID
            SELF:_tag:RDD:_dbfError(ERDD.CREATE_ORDER, GenCode.EG_CORRUPTION, strFunction, strMessage)
            RETURN

    END CLASS
END NAMESPACE
