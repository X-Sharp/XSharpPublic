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
        PRIVATE METHOD _sortGetRecord(buffer AS BYTE[]) AS LOGIC
            // Get Key Values
            SELF:_oRdd:ReadRecord()
            VAR result := getKeyValue(_sorter:SourceIndex, buffer)
            IF result
                _sorter:Add(SortRecord{buffer, SELF:_RecNo})
            ENDIF
            RETURN result

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
            VAR buffer := BYTE[]{ SELF:_keySize }
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
                IF ! SELF:_sortGetRecord(buffer)
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
            SELF:_sorter:StartWrite()
            SELF:_sorter:EndWrite()
            SELF:_sorter := NULL
            RETURN TRUE

        INTERNAL METHOD _InitSort(lRecCount AS LONG) AS LOGIC
            LOCAL sortInfo AS DbSortInfo
            LOCAL fType  := 0 AS DbFieldType
            sortInfo := DbSortInfo{0,1}     // 0 trans items, 1 sort item
            SELF:_sorter := CdxSortHelper{sortInfo, lRecCount, SELF}
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
            LOCAL result AS LOGIC
            LOCAL hasBlock AS LOGIC
           
            evalCount := 0
            lRecCount := SELF:_oRdd:RecCount
            // create sorthelper 
            SELF:_initSort(lRecCount)
            IF lRecCount == 0
                RETURN SELF:_CreateEmpty()
            ENDIF
            hasBlock    := SELF:_oRdd:_OrderCondInfo:EvalBlock != NULL
            evalCount := 1
            SELF:_oRdd:GoTo(1)
            VAR buffer := BYTE[]{ SELF:_keySize }
            REPEAT
                result := TRUE
                IF ! SELF:_sortGetRecord(buffer)
                    EXIT
                ENDIF
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
                VAR buffer := BYTE[]{ SELF:_keySize }
                REPEAT
                    SELF:_oRdd:GoTo(SELF:_RecNo + 1)
                    IF ! SELF:_sortGetRecord(buffer)
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
        INTERNAL CONSTRUCTOR( sortInfo   AS DbSortInfo , len AS LONG, tag AS CdxTag )
            SUPER(sortInfo, len)
            _tag := tag
            _bag := tag:OrderBag
            CurrentLeaf := NULL

        INTERNAL METHOD NewLeafPage(parent AS CdxBranchPage) AS CdxLeafPage
            LOCAL oLeaf AS CdxLeafPage
            LOCAL buffer AS BYTE[]
            buffer := _bag:AllocBuffer()
            oLeaf  := CdxLeafPage{_bag, -1, buffer, _tag:KeyLength}
            oLeaf:InitBlank()
            oLeaf:Right  := NULL
            oLeaf:Left   := CurrentLeaf
            IF CurrentLeaf != NULL
               CurrentLeaf:Right  := oLeaf
            ENDIF
            oLeaf:Tag := _tag
            oLeaf:Parent := parent
            oLeaf:Write()
            CurrentLeaf := oLeaf
            RETURN oLeaf

        INTERNAL METHOD newBranchPage(node AS CdxPageNode, brother AS CdxBranchPage) AS CdxBranchPage
            LOCAL oBranch AS CdxBranchPage
            LOCAL oParent AS CdxBranchPage
            LOCAL buffer AS BYTE[]
            // Allocate new Branch Page
            buffer  := _bag:AllocBuffer()
            oBranch := CdxBranchPage{_bag, -1, buffer, _tag:KeyLength}
            oBranch:Initialize()
            oBranch:Write()
            IF brother != NULL
                // When we allocate a 2nd page at the same level
                // then we must also have a parent level above this one
                oParent := brother:Parent
                IF oParent == NULL
                    oParent := newBranchPage(brother:LastNode, NULL)
                ELSE
                    oParent:Add(brother:LastNode)
                ENDIF
                brother:Parent := oParent
                brother:Right   := oBranch
                brother:Write()
            ELSE
                // No brother page, so this is the first Branch page in the tree
                oParent := NULL 
            ENDIF
            
            oBranch:Tag    := _tag
            oBranch:Parent := oParent
            oBranch:Left   := brother
            node:Page:Parent := oBranch
            oBranch:Add(node)
            oBranch:Write()
            RETURN oBranch

        INTERNAL METHOD AddRecord(nRecno AS LONG, data AS BYTE[]) AS LOGIC
            VAR oLeaf    := CurrentLeaf
            VAR oParent  := CurrentLeaf:Parent
            // place item on current leaf node.
            // When Leafnode is full then allocate a new leaf node
            // and add this leaf to the parent
            IF ! oLeaf:Add(nRecno, data)
                // this means that it did not fit
                oLeaf:Right  := SELF:NewLeafPage(oParent)
                IF oParent == NULL
                   oParent := SELF:NewBranchPage(oLeaf:LastNode, NULL)
                ELSE
                    IF ! oParent:Add(oLeaf:LastNode)
                        // Write Full parent to disk
                        oParent:Write()
                        oParent := SELF:NewBranchPage(oLeaf:LastNode, oParent)
                    ENDIF
                ENDIF
                oLeaf:Write()
                oLeaf := oLeaf:Right
                oLeaf:Parent := oParent
                IF ! oLeaf:Add(nRecno, data)
                    // Exception, this should not happen
                    RETURN FALSE
                ENDIF
            ENDIF
            RETURN TRUE

        INTERNAL METHOD StartWrite() AS LOGIC
            SELF:NewLeafPage(NULL)
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
            oParent  := oLeaf:Parent
            VAR node := oLeaf:LastNode
            DO WHILE oParent != NULL
                IF ! oParent:Add(node)      // It can happen and will happen that the last key does not fit and we need another branch
                    oParent:Write()
                    oParent := SELF:NewBranchPage(oLeaf:LastNode, oParent)
                ENDIF
                node    := oParent:LastNode
                oParent := oParent:Parent
            ENDDO
            // write the whole leaf level to disk
            DO WHILE oLeaf != NULL
                oLeaf:Write()
                oLeaf := oLeaf:Left
            ENDDO
            // Walk the tree to the root level
            // and write all pages
            oParent     := CurrentLeaf:Parent
            LOCAL oRoot := NULL AS CdxBranchPage
            VAR oPages  := List<CdxBranchPage>{}
            // Recursively add all pages, we start at the rightmost leaf node
            IF SELF:AddParents(oParent, oPages)
                // Walk all pages and force them to be written
                FOREACH oPage AS CDxBranchPage IN oPages
                    IF oPage:Parent == NULL // this must be the root
                        oRoot := oPage
                    ENDIF
                    oPage:Write()
                NEXT
            ENDIF
            IF oRoot != NULL
                SELF:SetRoot(oRoot)
            ELSE
                SELF:SetRoot(CurrentLeaf)
            ENDIF
            SELF:Clear()
            RETURN TRUE

         PRIVATE METHOD AddParents(oPage AS CdxBranchPage, oPages AS List<CdxBranchPage>) AS LOGIC
            // Recursively add all pages using the parent level and the left pointers to left siblings
            DO WHILE oPage != NULL
                IF ! oPages:Contains(oPage)
                    oPages:Add(oPage)
                ENDIF
                IF oPage:Parent != NULL .AND. ! oPages:Contains(oPage:Parent)
                    oPages:Add(oPage:Parent)
                    AddParents(oPage:Parent, oPages)
                ENDIF
                oPage := oPage:Left
            ENDDO
            RETURN oPages:Count > 0

        PRIVATE METHOD SetRoot(oPage AS CdxTreePage) AS VOID
            _tag:Header:RootPage := oPage:PageNo
            _tag:Header:Write()
            oPage:SetRoot()
            oPage:Write()
            RETURN
    END CLASS
END NAMESPACE
