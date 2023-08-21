//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Diagnostics
USING System.Globalization
USING System.IO
USING System.Reflection
USING System.Text
USING System.Threading
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.CDX



    INTERNAL SEALED PARTIAL CLASS CdxTag

        PRIVATE _errors AS List<STRING>
        PRIVATE METHOD _addError(sError AS STRING) AS VOID
            _errors:Add( sError)
            RETURN

        INTERNAL METHOD _validate() AS LOGIC
            LOCAL aLevels   AS List<LONG>
            LOCAL lOk := TRUE AS LOGIC

            TRY
                IF SELF:Xlock()
                    aLevels     := List<LONG>{}
                    _errors     := List<STRING>{}

                    SELF:GoTop()
                    // The stack should contain the first pages of each level
                    FOREACH VAR entry IN SELF:Stack:Entries
                        aLevels:Add(entry:Page:PageNo)
                    NEXT
                    aLevels:Reverse()
                    // Now the first level in the list contains the leaves
                    FOREACH VAR level IN aLevels
                        VAR page := SELF:GetPage(level)
                        IF page IS CdxLeafPage VAR leafPage
                            SELF:_validateLeaves(leafPage)
                        ELSEIF page IS CdxBranchPage VAR branchPage
                            SELF:_validateBranches(branchPage)
                        ENDIF
                    NEXT
                    VAR cFile := SELF:_bag:FullPath+"_"+SELF:OrderName+".ERR"

                    IF _errors:Count > 0
                        System.IO.File.WriteAllLines(cFile, _errors:ToArray())
                        lOk := FALSE
                    ELSE
                        lOk := TRUE
                        IF System.IO.File.Exists(cFile)
                            FErase(cFile)
                        ENDIF
                    ENDIF
                    SELF:_errors := NULL

                ENDIF
            FINALLY
                SELF:UnLock()
            END TRY
            RETURN lOk

        PRIVATE METHOD _validateLeaves(firstPage AS CdxLeafPage) AS VOID
            LOCAL aRecordList AS BitArray
            LOCAL currentPage AS CdxLeafPage
            aRecordList := BitArray{SELF:_oRdd:RecCount+1}
            currentPage := firstPage
            IF firstPage:HasLeft
               SELF:_addError( i"First page {firstPage.PageNo} in leaf level has a LeftPointer {firstPage.LeftPtr:X}")
            ENDIF
            DO WHILE currentPage != NULL
                VAR prevKey := BYTE[]{_keySize}
                VAR currKey := BYTE[]{_keySize}
                LOCAL prevRec := 0 AS LONG
                FOREACH leaf AS CdxLeaf IN currentPage:Keys
                    IF aRecordList[leaf:Recno]
                        SELF:_addError( i"Record {leaf.Recno} is found more than once at the Leaf level. Second occurrence was on page on page {currentPage.PageNo:X} ")
                    ELSE
                        aRecordList[leaf:Recno] := TRUE
                    ENDIF
                    IF SELF:__Compare(prevKey, leaf:Key, _keySize, prevRec, leaf:Recno) > 0
                        VAR cLeft   := prevKey:ToAscii(FALSE)
                        VAR cRight  := leaf:Key:ToAscii(FALSE)
                        SELF:_addError( i"Key value in the index for record {leaf.Recno} is not in the right order, found '{cLeft}' '{cRight}' ")
                    ENDIF
                    SELF:_oRdd:__Goto(leaf:Recno)
                    SELF:getKeyValue(SELF:_SourceIndex, currKey)
                    IF SELF:__Compare(currKey, leaf:Key, _keySize, leaf:Recno, leaf:Recno) != 0
                        VAR cLeft   := currKey:ToAscii(FALSE)
                        VAR cRight  := leaf:Key:ToAscii(FALSE)
                        SELF:_addError( i"Key value in the index for record {leaf.Recno} on page {currentPage.PageNo:X} is not up to date, the page contains the key '{cRight}' but that should be '{cLeft}'")
                    ENDIF
                    prevKey := leaf:Key
                    prevRec := leaf:Recno
                NEXT

                IF currentPage:HasRight
                    VAR nextPage := SELF:GetPage(currentPage:RightPtr)
                    IF nextPage:LeftPtr != currentPage:PageNo
                        SELF:_addError( i"Left Link on {currentPage.PageType} page {currentPage.PageNo:X} contains incorrect pointer")
                    ENDIF
                    IF nextPage IS CdxLeafPage VAR lp
                        currentPage := lp
                    ELSE
                        SELF:_addError( i"Page {currentPage.RightPtr:X} should have been a leafpage but instead it is a {nextPage.PageType}")
                        EXIT
                    ENDIF
                ELSE
                    currentPage := NULL_OBJECT
                ENDIF
            ENDDO
            // now validate if we have seen all records
            IF String.IsNullOrEmpty(SELF:_ForExpr)
                FOR VAR i := 1 TO SELF:_oRdd:RecCount
                    IF aRecordList[i]
                        // Ok
                        NOP
                    ELSE
                        SELF:_addError( i"Record {i} is not recorded at the Leaf level")
                    ENDIF
                NEXT
            ELSE
                FOR VAR i := 1 TO SELF:_oRdd:RecCount
                    SELF:_oRdd:__Goto(i)
                    LOCAL oInclude AS OBJECT
                    oInclude  := SELF:_oRdd:EvalBlock(SELF:_ForCodeBlock)
                    IF oInclude IS LOGIC VAR lInclude
                        IF lInclude .AND. ! aRecordList[i]
                            SELF:_addError( i"Record {i} is not recorded at the Leaf level, but it DOES match the for condition")
                        ELSEIF ! lInclude .AND. aRecordList[i]
                            SELF:_addError( i"Record {i} is recorded at the Leaf level, but it DOES NOT match the for condition")
                        ENDIF
                    ELSE
                        SELF:_addError( i"FOR condition '{SELF._ForExpr}' for index does not return a logical value")
                        EXIT
                    ENDIF
                NEXT
            ENDIF
            RETURN

        PRIVATE METHOD _validateBranches(firstPage AS CdxBranchPage) AS VOID
            LOCAL currentPage AS CdxBranchPage
            currentPage := firstPage
            IF firstPage:HasLeft
                SELF:_addError( i"First page {firstPage.PageNo} in branch level has a LeftPointer {firstPage.LeftPtr:X}")
            ENDIF
            DO WHILE currentPage != NULL
                VAR prevKey := BYTE[]{_keySize}
                LOCAL prevRecno := 0 AS LONG
                FOR VAR nBranch := 0 TO currentPage:Keys:Count-1
                    VAR branch := currentPage:Keys[nBranch]
                    IF SELF:__Compare(prevKey, branch:Key, _keySize, prevRecno, branch:Recno) > 0
                        SELF:_addError( i"Key value in the index for page {branch.ChildPage:X}, record {branch.Recno} is not in the right order")
                    ENDIF
                    VAR childPage  := SELF:GetPage(branch:ChildPage)
                    VAR lastNode   := childPage:LastNode
                    IF lastNode != NULL
                        IF branch:Recno != lastNode:Recno
                            SELF:_addError( i"Page {currentPage.PageNoX}, position {nBranch} branch points to page {branch.ChildPageX} and record {branch.Recno} but the last record on page {branch.ChildPageX} is {lastNode.Recno}")
                        ENDIF
                        IF SELF:__Compare(lastNode:KeyBytes, branch:Key, _keySize, lastNode:Recno, branch:Recno) != 0
                            SELF:_addError( i"Branch has another keyvalue as page {branch.ChildPage:X} ")
                        ENDIF
                    ELSE
                        SELF:_addError( i"{childPage.PageType} Page {childPage.PageNo:X} has no lastNode. Keycount: {childPage.NumKeys} ")
                    ENDIF
                    prevKey := branch:Key
                    prevRecno := branch:Recno
                NEXT
                IF currentPage:HasRight
                    VAR nextPage := SELF:GetPage(currentPage:RightPtr)
                    IF nextPage:LeftPtr != currentPage:PageNo
                        SELF:_addError( i"Left Link on {nextPage.PageType} page {nextPage.PageNo:X} contains incorrect pointer")
                    ENDIF
                    IF nextPage IS CdxBranchPage VAR bp
                        currentPage := bp
                    ELSE
                        SELF:_addError( i"Page {nextPage.PageNo:X} should have been a branchpage but instead it is a {nextPage.PageType}")
                        EXIT
                    ENDIF
                ELSE
                    currentPage := NULL
                ENDIF
            ENDDO
            RETURN

    END CLASS
END NAMESPACE
