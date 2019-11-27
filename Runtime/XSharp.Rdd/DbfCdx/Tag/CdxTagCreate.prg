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
#undef SHOWTIMES

BEGIN NAMESPACE XSharp.RDD.CDX

    INTERNAL DELEGATE ValueBlock( sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC

    INTERNAL PARTIAL SEALED CLASS CdxTag IMPLEMENTS IRddSortWriter
        PRIVATE _sorter AS CdxSortHelper
        
        // Methods for Creating Indices
        PUBLIC METHOD Create(createInfo AS DbOrderCreateInfo ) AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL hasForCond AS LOGIC
            SELF:_ordCondInfo := SELF:_oRdd:_OrderCondInfo:Clone()
            #ifdef SHOWTIMES
            ? DateTime.Now, "Create", createInfo:Order, "# of records", SELF:_oRDD:RecCount
            #endif
            IF string.IsNullOrEmpty(createInfo:BagName)
                SELF:_oRDD:_dbfError(  SubCodes.EDB_CREATEINDEX, GenCode.EG_ARG,"OrdCreate", "Missing Orderbag Name")
                RETURN FALSE
            ENDIF
            isOk := SELF:_oRdd:GoCold()
            IF !_ordCondInfo:Scoped
                LOCAL orderInfo AS DbOrderInfo
                orderInfo := DbOrderInfo{}
                SELF:_oRdd:OrderListFocus(orderInfo)
            ENDIF
            IF _ordCondInfo:ForBlock != NULL
                hasForCond := TRUE
                SELF:_ForCodeBlock := _ordCondInfo :ForBlock
            ELSE
                hasForCond := FALSE
            ENDIF
            SELF:_KeyExpr := createInfo:Expression
            IF _ordCondInfo  != NULL .AND. _ordCondInfo :ForExpression != NULL
                SELF:_ForExpr := _ordCondInfo :ForExpression
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
            SELF:_Custom := _ordCondInfo:Custom
            IF _ordCondInfo :Active
                SELF:_Descending := _ordCondInfo :Descending
                IF hasForCond .AND. !string.IsNullOrEmpty(_ordCondInfo :ForExpression)
                    SELF:_Conditional := TRUE
                ENDIF
            ENDIF
            isOk := SELF:_Build()
            SELF:_bag:FlushPages()
            Gc.Collect()
            RETURN isOk


        INTERNAL METHOD Rebuild() AS LOGIC
            SELF:_ordCondInfo := DbOrderCondInfo{}
            SELF:_ordCondInfo:Descending := SELF:_Descending
            SELF:_ordCondInfo:ForExpression := SELF:_ForExpr
            SELF:_ordCondInfo:Compile(SELF:_oRDD)
            SELF:_ordCondInfo:Active := TRUE
            SELF:_ordCondInfo:Validate()
            RETURN SELF:_Build()

        PRIVATE METHOD _Build() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL ic AS CdxSortCompare
            IF ! SELF:_HeaderCreate()
                SELF:Close()
                SELF:_oRdd:_dbfError(GenCode.EG_CREATE,  SubCodes.ERDD_WRITE,"OrdCreate", "Could not write Header ")
                RETURN FALSE
            ENDIF
            #ifdef SHOWTIMES
            ? DateTime.Now, "Read"
            #endif
            IF !SELF:Unique .AND. !SELF:_Conditional .AND. !_ordCondInfo:Scoped .AND. ! _ordCondInfo:Custom
                isOk := SELF:_CreateNormalIndex()
            ELSE
                isOk := SELF:_CreateUnique(_ordCondInfo )
            ENDIF
            #ifdef SHOWTIMES
            ? DateTime.Now, "Sort, # of keys", _sorter:_dataBuffer:Count
            #endif
            IF isOk
                IF _sorter:Ascii
                    ic := CdxSortCompareAscii{SELF:_oRdd, _sorter}
                ELSE
                    ic := CdxSortCompareDefault{SELF:_oRdd, _sorter}
                ENDIF
                isOk := _sorter:Sort(ic)
            ENDIF
            #ifdef SHOWTIMES
            ? DateTime.Now, "Write"
            #endif
            IF isOk
                SELF:_sorter:StartWrite()
                isOk := _sorter:Write(SELF)
            ENDIF
            IF isOk
                SELF:_sorter:EndWrite()
            ENDIF
            #ifdef SHOWTIMES
            ? DateTime.Now, "End"
            #endif

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
            SELF:_Header            := CdxTagHeader{_bag, -1 ,_orderName, SELF}
            SELF:_Header:Descending := SELF:_Descending
            SELF:_Header:Version    := 0
            SELF:_Header:Signature  := 1
            SELF:_Header:RootPage   := 0
            SELF:_Header:FreeList   := 0
            SELF:_Header:Version    := 0
            SELF:_Header:KeySize    := _keySize
            SELF:_Header:KeyExprPos := 0
            SELF:_Header:KeyExprLen := (WORD) _KeyExpr:Length + 1
            SELF:_Header:ForExprPos := SELF:_Header:KeyExprLen 
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
                SELF:_sourceKeySize := SELF:_keySize := (WORD) ((STRING)toConvert):Length
            CASE __UsualType.Long
            CASE __UsualType.Float
            CASE __UsualType.Date
                SELF:_sourceKeySize := SELF:_keySize := 8      // all stored as numeric
            CASE __UsualType.Logic
                SELF:_sourceKeySize := SELF:_keySize := 1
            OTHERWISE
                SELF:_sourceKeySize := SELF:_keySize := 0
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
                    lUseOrder    := leadingOrder != NULL .AND. ordCondInfo:UseCurrent
                ENDIF
                IF ordCondInfo:All
                    // All overrules start record no
                    IF lUseOrder
                        // start from first record in index
                        record := leadingOrder:_locateKey(NULL, 0, SearchMode.Top,0)
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
            IF lUseOrder .AND. !leadingOrder:Stack:Empty
                result := leadingOrder:_GoToRecno(SELF:_RecNo)
                IF !result
                    RETURN result
                ENDIF
            ENDIF
            IF hasWhile .OR. hasEvalBlock .OR. todo != 0 .OR. lUseOrder .OR. ! SELF:_Conditional
                LOCAL cbSkip AS @@Func<LONG>
                IF lUseOrder
                    cbSKip := { => leadingOrder:_getNextKey(SkipDirection.Forward)}
                ELSE
                    cbSKip := { =>SELF:_RecNo + 1}
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
                    nextRecord := cbSkip()
                    SELF:_oRdd:__Goto( nextRecord)
                    IF todo != 0 .AND. done >= todo
                        EXIT
                    ENDIF
                    IF SELF:_oRdd:_Eof 
                        EXIT
                    ENDIF
                ENDDO
            ELSE
                DO WHILE !_oRdd:_Eof
                    // Only conditions, nothing else
                    includeRecord := SELF:_EvalBlock(ordCondInfo:ForBlock, TRUE)
                    IF includeRecord
                        IF ! SELF:_sortGetRecord()
                            EXIT
                        ENDIF
                    ENDIF
                    SELF:_oRdd:__Goto( SELF:_RecNo + 1)
                ENDDO
            ENDIF
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
                IF res IS LOGIC VAR ok
                    isOk := ok
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
            LOCAL fType  := 0 AS DbFieldType
            LOCAL sortInfo AS DbSortInfo
            sortInfo := DbSortInfo{0,1}     // 0 trans items, 1 sort item
            SELF:_sorter := CdxSortHelper{SELF:_oRDD, sortInfo, lRecCount, SELF}
            IF SELF:_SingleField != -1
                fType := SELF:_oRdd:_Fields[SELF:_SingleField]:fieldType
                // 'C', 'N', 'D', 'L'
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
	        ELSE
	    	    SELF:_sorter:sourceIndex := -1
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
            RETURN _sorter:AddRecord(record:Recno, record:Data, record:Duplicate)
            
        INTERNAL METHOD _CreateUnique(ordCondInfo AS DbOrderCondInfo ) AS LOGIC
            LOCAL LRecCount AS LONG
            lRecCount := SELF:_oRdd:RecCount
            // create sorthelper
            SELF:_initSort(lRecCount)
            SELF:_sorter:Unique := SELF:_Unique
            IF ordCondInfo:Active
                RETURN SELF:_CondCreate(ordCondInfo)
            ENDIF
            SELF:_oRdd:GoTo(1)
            IF SELF:_oRdd:_isValid
                IF ! _ordCondInfo:Custom
                    REPEAT
                        SELF:_oRdd:GoTo(SELF:_RecNo + 1)
                        IF ! SELF:_sortGetRecord()
                            EXIT
                        ENDIF
                    UNTIL ! SELF:_oRdd:_isValid
                ENDIF
            ENDIF
            SELF:ClearStack()
            SELF:Flush()
            RETURN TRUE


        PUBLIC METHOD Truncate() AS LOGIC
            // Find all pages of the tag and delete them
            // then also delete the tag header and return everything to the OrderBag 
            RETURN TRUE
            
    END CLASS
    INTERNAL SEALED CLASS CdxSortHelper INHERIT RddSortHelper
        INTERNAL PROPERTY SourceIndex    AS INT AUTO
        INTERNAL PROPERTY Ascii          AS LOGIC AUTO
        INTERNAL PROPERTY Unique         AS LOGIC AUTO
        //PRIVATE written                  AS LONG
        PRIVATE _tag                     AS CdxTag

        INTERNAL CONSTRUCTOR( rdd AS DBF, sortInfo   AS DbSortInfo , len AS LONG, tag AS CdxTag )
            SUPER(rdd, sortInfo, len)
            _tag := tag

        INTERNAL METHOD AddRecord(nRecno AS LONG, data AS BYTE[], duplicate AS LOGIC) AS LOGIC
            // place item on current leaf node.
            // the code inside Doaction takes care of adding extra leaf pages etc.
            IF SELF:Unique .AND. duplicate
                // Do not write
                RETURN TRUE
            ENDIF
            VAR action := CdxAction.AddKey(nRecno, data)
            action := _tag:DoAction(action)
            IF action:Type != CdxActionType.OK
                Error("CdxSortHelper.AddRecord","Could not add record to leaf")
                RETURN FALSE
            ENDIF
//            ++Written
//            IF Written % 1000 == 0
//                ? "Written", written                
//            ENDIF
            RETURN TRUE

        INTERNAL METHOD StartWrite() AS LOGIC
            VAR page := SELF:_tag:NewLeafPage()
            SELF:_tag:PushPage(page)
            SELF:_tag:SetRoot(page)
            RETURN TRUE

        INTERNAL METHOD EndWrite() AS LOGIC
            // Write the header
            // Write the last node for all branch pages
            LOCAL oLeaf  AS CdxLeafPage
            oLeaf       := (CdxLeafPage) _tag:Stack:Top:Page
            VAR oLast   := oLeaf:LastNode
            VAR root    := _tag:Stack:Root?:Page
            IF root != NULL
                _tag:SetRoot(root)
            ENDIF
            root:Write()
            VAR oPage := root
            DO WHILE oPage IS CdxBranchPage VAR oBranch
                VAR nLast := oPage:NumKeys-1
                VAR nPage := oBranch:GetChildPage(nLast)
                oBranch:SetData(nLast, oLast:Recno, nPage, oLast:KeyBytes)
                oPage := SELF:_tag:GetPage(nPage)
            ENDDO
                
            SELF:Clear()
            RETURN TRUE

        PRIVATE METHOD Error(strFunction AS STRING, strMessage AS STRING) AS VOID
            SELF:_tag:RDD:_dbfError(ERDD.CREATE_ORDER, GenCode.EG_CORRUPTION, strFunction, strMessage)
            RETURN

    END CLASS
END NAMESPACE
