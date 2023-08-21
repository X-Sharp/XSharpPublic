/*
BRANCH Page
- Branch pages are used to link the tree. Their contents is
BYTE     attr    [ 2 ];    node type
BYTE     nKeys   [ 2 ];    number of keys
BYTE     leftPtr [ 4 ];    offset of left node or -1
BYTE     rightPtr[ 4 ];    offset of right node or -1
// array of key entries
// each key entry is keyLen + 8 bytes long
// BYTE Key data [keyLen]
// BYTE record number[ 4]
// BYTE child page [4]

*/
#include "CdxDebug.xh"

#define USEATTRIB
#ifdef USEATTRIB
#XTRANSLATE \[HIDDEN\] => \[DebuggerBrowsable(DebuggerBrowsableState.Never)\]
#XTRANSLATE \[INLINE\] => \[MethodImpl(MethodImplOptions.AggressiveInlining)\]
#XTRANSLATE \[NODEBUG\] => \[DebuggerStepThroughAttribute\]
#else
#XTRANSLATE \[HIDDEN\] =>
#XTRANSLATE \[INLINE\] =>
#XTRANSLATE \[NODEBUG\] =>
#endif


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Diagnostics
BEGIN NAMESPACE XSharp.RDD.CDX
    [DebuggerDisplay("{DebugString,nq}")];
    INTERNAL SEALED CLASS CdxBranch
        INTERNAL Recno AS LONG
        INTERNAL ChildPage AS LONG
        INTERNAL Key   AS BYTE[]
        INTERNAL KeyBinary AS LOGIC
        INTERNAL PROPERTY KeyText       AS STRING GET SELF:Key:ToAscii(KeyBinary)
        INTERNAL PROPERTY ChildPageX    AS STRING GET ChildPage:ToString("X8")
        INTERNAL PROPERTY DebugString   AS STRING GET SELF:KeyText:Trim() +" # "+SELF:Recno:ToString()+" P " + ChildPageX
        INTERNAL CONSTRUCTOR (nRecno AS LONG, nChild AS LONG, bKey AS BYTE[],binary as LOGIC)
            SELF:Recno := nRecno
            SELF:ChildPage := nChild
            SELF:Key   := (BYTE[]) bKey:Clone()
            SELF:KeyBinary := binary
            RETURN
        OVERRIDE METHOD ToString() AS STRING => DebugString

    END CLASS
    /// <summary>
    /// CdxBranchePage. this class maps the Branch page from the file in memory
    /// Manipulating the page is implemented in the CdxTag class
    /// </summary>
    /*
    From the FoxPro Docs, where this is called "Compact Index Interior Node Record"
    Byte offset  Description
    ========================
    00 - 01     Node attributes (any of the following numeric values or their sums):
                a.   0 - index node
                b.   1 - root node
                c.   2 - leaf node
    02 - 03     Number of keys present (0, 1 or many)
    04 - 07     Pointer to node directly to left of current node (on same level, -1 if not present)
    08 - 11     Pointer to node directly to right of current node (on same level; -1 if not present)
    12 - 511    Up to 500 characters containing the key value for the length of the key with a four-byte hexadecimal number
                (stored in normal left-to-right format):
                This node always contains the index key, record number and intra-index pointer.2
                The key/four-byte hexadecimal number combinations will occur the number of times indicated in bytes 02 ? 03.
    */
    INTERNAL SEALED CLASS CdxBranchPage INHERIT CdxTreePage
        PRIVATE _keyLen    AS Int32
        PRIVATE _dataLen   as Int32
        PRIVATE _pageNoOffSet as Int32
        PRIVATE _maxKeys   AS Int32

       #region Constants
        PRIVATE CONST CDXBRANCH_HEADERLEN           := 12	AS WORD     // Type (2), NumKeys (2), LeftPtr (4), RightPtr (4)
        PRIVATE CONST CDXBRANCH_BYTESFREE           := 500  AS WORD
        #endregion

        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer)
            SELF:_keyLen        := nKeyLen
            SELF:_dataLen       := _keyLen + 8
            SELF:_pageNoOffSet  := _keyLen + 4
            SELF:_maxKeys       := CDXBRANCH_BYTESFREE / _dataLen



            //? "Branch Page", SELF:PageNoX, SELF:NumKeys, "Startswith ", GetRecno(0), _bag:_oRDD:_Encoding:GetString(GetKey(0),0,_keyLen)
        INTERNAL OVERRIDE METHOD InitBlank(oTag AS CdxTag) AS VOID
            SELF:PageType   := CdxPageType.Branch
            SELF:NumKeys    := 0
            SELF:LeftPtr    := SELF:RightPtr   := -1
            SELF:Tag        := oTag
            RETURN

        PROTECTED OVERRIDE METHOD _clear() AS VOID
            SUPER:_clear()
            SELF:_keyLen        := 0
            SELF:_dataLen       := 0
            SELF:_pageNoOffSet  := 0
            SELF:_maxKeys       := 0

        INTERNAL OVERRIDE METHOD Clear() AS VOID
            SUPER:Clear()
            SELF:_clear()

        INTERNAL OVERRIDE METHOD Read() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Read()
            RETURN lOk

        INTERNAL OVERRIDE METHOD GetKey(nPos AS Int32) AS BYTE[]
            LOCAL nStart AS INT
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            RETURN SELF:_GetBytes( nStart, _keyLen)

        [INLINE];
        INTERNAL OVERRIDE METHOD GetRecno(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            RETURN SELF:_GetLongLE(nStart+_keyLen)

        INTERNAL METHOD SetData(nPos as Int32, nRecord as Int32, nPage as Int32, key as Byte[]) AS CdxAction
            LOCAL nStart AS INT
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            Array.Copy(key, 0, _buffer, nStart, _keyLen)
            SELF:_SetLongLE(nStart+_keyLen, nRecord)
            SELF:_SetLongLE(nStart+_pageNoOffSet, nPage)
            return CdxAction.Ok

        INTERNAL OVERRIDE METHOD GetChildPage(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            RETURN SELF:_GetLongLE(nStart+_pageNoOffSet)

        INTERNAL OVERRIDE METHOD GetChildren as IList<LONG>
            // used in the dump routine to avoid recursion
            VAR oList := List<LONG>{}
            FOR VAR i := 0 to SELF:NumKeys - 1
                oList:Add(SELF:GetChildPage(i))
            NEXT
            RETURN oList


        #region Properties
        // We read the values from our cache but write back to the cache and the buffer at the same time
        // The _Set.. methods set the isHot flag of the page automatically


        INTERNAL OVERRIDE PROPERTY LastNode AS CdxPageNode GET IIF(SELF:NumKeys == 0, NULL, SELF[(WORD) (SELF:NumKeys-1)])

        INTERNAL PROPERTY MaxKeys AS LONG GET _maxKeys

        #endregion


        INTERNAL METHOD Add(node AS CdxPageNode) AS CdxAction
            RETURN SELF:Add(node:Recno, node:Page:PageNo, node:KeyBytes)

         INTERNAL METHOD Add(recno AS LONG, childPageNo AS LONG, key AS BYTE[]) AS CdxAction
            //SELF:Debug(recno, childPageNo:ToString("X6"))
            IF SELF:NumKeys >= SELF:MaxKeys
                //Debug( "triggers addBranch ","Rec", recno, "Child", childPageNo:ToString("X8"))
                RETURN CdxAction.AddBranch(SELF, childPageNo, recno, key)
            ENDIF
            IF SELF:IsDuplicate(recno, childPageNo, key)
                RETURN CdxAction.Ok
            ENDIF
            // make sure that this key is larger than the last key
            IF SELF:NumKeys > 0
                VAR lastKey := SELF:GetKey(SELF:NumKeys-1)
                VAR nDiff := SELF:Tag:__Compare(lastKey, key, key:Length, SELF:GetRecno(SELF:NumKeys-1), recno)
                IF nDiff > 0
                    // New key needs to be inserted and not added to the page
                    VAR Pos := SELF:FindKey(key, recno, key:Length)
                    IF Pos > 0 .and. Pos < SELF:NumKeys
                        // insert before this key
                        RETURN SELF:Insert(Pos, recno, childPageNo, key)
                    ELSEIF Pos == SELF:NumKeys
                        RETURN SELF:Add(recno, childPageNo, key)
                    ENDIF
                ENDIF
            ENDIF
            VAR nPos := SELF:NumKeys
            SELF:NumKeys++
            SELF:SetData(nPos, recno, childPageNo, key)
            RETURN CdxAction.Ok

        INTERNAL METHOD Insert(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            RETURN SELF:Insert(nPos, node:Recno, node:Page:PageNo, node:KeyBytes)

        INTERNAL METHOD Insert(nPos AS LONG, recno AS LONG, childPageNo AS LONG, key AS BYTE[]) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            // we allow to write at the position nMax
            //SELF:Debug(nPos, recno, childPageNo:ToString("X6"))
            IF SELF:IsDuplicate(recno, childPageNo, key)
                RETURN CdxAction.Ok
            ENDIF
            IF SELF:NumKeys >= SELF:MaxKeys
                //Debug( "triggers split ", "Pos", nPos, "Rec", recno, "Child", childPageNo:ToString("X8"))
                RETURN CdxAction.SplitBranch(SELF, childPageNo, recno, key, nPos)
            ENDIF
            IF nPos == nMax                 // Insert at end of list
                SELF:NumKeys += 1
                SELF:SetData(nMax, recno, childPageNo, key)
#ifdef TESTCDX
                SELF:Validate()
#endif
                RETURN CdxAction.Ok
            ENDIF
            IF nPos < 0 .OR. nPos > nMax
                RETURN CdxAction.OutOfBounds(SELF)
            ENDIF
            // copy nodes up
            // TODO Use ArrayCopy for whole block for better performance
            SELF:NumKeys += 1
            var workBuffer := Byte[]{_dataLen}
            FOR VAR nI := nMax-1 DOWNTO nPos
                SELF:_copyNode(nI, nI+1,workBuffer)
                IF nI == 0
                    EXIT
                ENDIF
            NEXT
            // and insert at the right spot
            SELF:SetData(nPos, recno, childPageNo, key)
            SELF:Write()
            RETURN CdxAction.Ok

        INTERNAL METHOD Delete(nPos AS LONG) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            LOCAL result AS CdxAction
            Debug.Assert(nMax > 0, "Branchpage is unexpectedly empty")
            IF nMax == 0
                RETURN CdxAction.DeletePage(SELF)
            ENDIF
            Debug.Assert(nPos >= 0 .AND. nPos < nMax, "Delete key that is not on the page")
            IF nPos < 0 .OR. nPos > nMax-1
                RETURN CdxAction.OutOfBounds(SELF)
            ENDIF
            // node contains recno & keydata
            // node:Page has value for ChildPageNo
            // TODO Use MemCopy or ArrayCopy in stead for better performance
            IF nMax > 1
                var workBuffer := Byte[]{_dataLen}
                FOR VAR nI := nPos TO nMax-2
                    SELF:_copyNode(nI+1, nI,workBuffer)
                NEXT
            ENDIF
            SELF:NumKeys -= 1
            IF SELF:NumKeys == 0
                result := CdxAction.DeletePage(SELF)
            ELSEIF nPos == SELF:NumKeys
                result := CdxAction.ChangeParent(SELF)
            ELSE
                result := CdxAction.Ok
            ENDIF
            SELF:Write()
            RETURN result



        INTERNAL METHOD Split(oTarget AS CdxBranchPage, action AS CdxAction, lAdd AS LOGIC) AS CdxAction
            VAR  keys  := (List<CdxBranch>) SELF:Keys
            VAR nPos := SELF:FindKey(action:Key,action:Recno, action:Key:Length)
            //Self:Debug("Split to page", oTarget:PageNoX)
            if nPos < self:NumKeys -1
                keys:Insert(nPos, CdxBranch{action:Recno, action:ChildPage, action:Key,SELF:Tag:Binary})
            elseif nPos >= self:NumKeys
                keys:Add( CdxBranch{action:Recno, action:ChildPage, action:Key,SELF:Tag:Binary})
            ELSE
                keys:Insert(nPos, CdxBranch{action:Recno, action:ChildPage, action:Key,SELF:Tag:Binary})
            ENDIF
            LOCAL half AS INT
            IF lAdd
                half := SELF:MaxKeys-1
            ELSE
                half := keys:Count /2
            ENDIF
            SELF:NumKeys := 0
            FOR VAR i := 0 TO half
                VAR node := keys[i]
                SELF:Add(node:Recno, node:ChildPage, node:Key)
            NEXT
            Debug.Assert(NumKeys > 0)
            FOR VAR i := half+1 TO keys:Count-1
                VAR node := keys[i]
                oTarget:Add(node:Recno, node:ChildPage, node:Key)
            NEXT
            SELF:Write()
            oTarget:Write()
            RETURN CdxAction.Ok


        INTERNAL METHOD Replace(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            IF nPos < 0 .OR. nPos >= nMax
                RETURN CdxAction.OutOfBounds(SELF)
            ENDIF
            VAR result := SELF:SetData(nPos, node:Recno, node:Page:PageNo, node:KeyBytes)
            SELF:Write()
            RETURN result


        // Helper methods, these do not verify the bounds
        PRIVATE METHOD _copyNode(nSrc AS LONG, nTrg AS LONG,tmpBuffer as Byte[] ) AS CdxAction
            IF nSrc != nTrg
                var nPos1 := CDXBRANCH_HEADERLEN + nSrc * _dataLen
                var nPos2 := CDXBRANCH_HEADERLEN + nTrg * _dataLen
                System.Array.Copy(_buffer, nPos1, tmpBuffer, 0, _dataLen)
                System.Array.Copy(tmpBuffer, 0, _buffer, nPos2, _dataLen)
            ENDIF
            RETURN CdxAction.Ok

        INTERNAL OVERRIDE METHOD Dump AS STRING
            LOCAL sb AS StringBuilder
            LOCAL i := 0 AS WORD
            sb := StringBuilder{}
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}, MaxKeys: {3}", SELF:PageType, SELF:PageNo, SELF:NumKeys, SELF:MaxKeys))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            FOREACH branch AS CdxBranch IN SELF:Keys
                sb:AppendLine(String.Format("Item {0,2}, Page {1:X6}, Record {2,6} : {3} ", i, branch:ChildPage, branch:Recno, branch:KeyText))
                i++
            NEXT
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            RETURN sb:ToString()

         INTERNAL PROPERTY Keys AS IList<CdxBranch>
            GET
                LOCAL oList AS List<CdxBranch>
                LOCAL nMax AS LONG
                nMax := SELF:NumKeys
                oList := List<CdxBranch>{nMax}
                FOR VAR i := 0 TO nMax -1
                    oList:Add( CdxBranch{SELF:GetRecno(i), SELF:GetChildPage(i), SELF:GetKey(i),SELF:Tag:Binary})
                NEXT
                RETURN oList
            END GET
         END PROPERTY

         INTERNAL METHOD FindPage(nPage AS LONG) AS LONG
            FOR VAR i := 0 TO NumKeys -1
                IF SELF:GetChildPage(i) == nPage
                    RETURN i
                ENDIF
            NEXT
            RETURN -1

         INTERNAL OVERRIDE METHOD FindKey(key AS BYTE[], recno AS LONG, keyLength AS LONG) AS WORD
            // return page number of page where pageKey > key
            // or page where pageKey == key and RecordNo > recno
            LOCAL nI AS WORD
            IF SELF:NumKeys > 0
                // Branchpage can't really be empty...
                FOR nI := 0 TO SELF:NumKeys -1
                    VAR pageKey := SELF:GetKey(nI)
                    VAR nDiff := SELF:Tag:__Compare(pageKey, key, keyLength, SELF:GetRecno(nI), recno)
                    SWITCH nDiff
                    CASE 0
                        IF SELF:GetRecno(nI) > recno
                            RETURN nI
                        ENDIF

                    CASE 1
                        // pageKey is larger
                        RETURN nI
                    OTHERWISE
                        // continue
                        NOP
                    END SWITCH
               NEXT
            ENDIF
           RETURN SELF:NumKeys
#ifdef TESTCDX

        METHOD ValidateLevel() AS LOGIC
            VAR page := SELF
            LOCAL lOk := TRUE AS LOGIC
            page := (CdxBranchPage) SELF:FirstPageOnLevel
            DO WHILE page != NULL
                lOk := page:ValidateKeys()
                IF page:HasRight
                    VAR leftNode  := page:Keys[page:NumKeys-1]
                    VAR rightPage := (CdxBranchPage) SELF:_tag:GetPage(page:RightPtr)
                    VAR rightNode := rightPage:Keys[0]
                    VAR nDiff := SELF:Tag:__Compare(leftNode:Key, rightNode:Key, leftNode:Key:Length, leftNode:Recno, rightNode:Recno)
                    IF nDiff > 0
                        SELF:Debug(page:PageNoX, "Keys in wrong order", leftNode:DebugString, rightNode:DebugString)
                        SELF:DumpKeys()
                        rightPage:DumpKeys()
                        lOk := FALSE
                    ENDIF
                    page := rightPage
                ELSE
                    page := NULL
                ENDIF
            ENDDO
            IF lOk
                VAR child := SELF:Keys[0]:ChildPage
                VAR childPage := SELF:_tag:GetPage(child)
                childPage:ValidateLevel()
            ENDIF
            RETURN lOk
#endif
        METHOD DumpKeys AS VOID
        Debug("Dump keys for branch page", SELF:PageNoX)
        FOREACH VAR Branch IN SELF:Keys
            Debug("Child", Branch:ChildPageX, "Rec", Branch:Recno, Branch:KeyText:Trim())
        NEXT
#ifdef TESTCDX
        METHOD ValidateKeys() AS LOGIC
            LOCAL oLast := NULL AS CdxBranch
            LOCAL lOk := TRUE AS LOGIC
            FOREACH VAR Branch IN SELF:Keys
                IF oLast != NULL
                   VAR nDiff := SELF:Tag:__Compare(oLast:Key, Branch:Key, oLast:Key:Length, oLast:Recno, Branch:Recno)
                    IF nDiff > 0
                        SELF:Debug("Branch page: ",SELF:PageNoX, "Keys in wrong order", oLast:DebugString, Branch:DebugString)
                        lOk := FALSE
                    ENDIF
               ENDIF
               oLast := Branch
            NEXT
           RETURN lOk

      INTERNAL METHOD ValidateKeys(oLeft AS CdxBranch, oRight AS CdxBranch) AS LOGIC
            LOCAL nDiff AS LONG
            IF SELF:Tag == NULL
                RETURN TRUE
            ENDIF
            nDiff := SELF:Tag:__Compare(oLeft:Key, oRight:Key, oLeft:Key:Length, oLeft:Recno, oRight:Recno)
            IF nDiff == -1
                RETURN TRUE
            ELSEIF nDiff == 0 .AND. oLeft:Recno < oRight:Recno
                RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD ValidateSiblings AS LOGIC
            LOCAL lOk := TRUE AS LOGIC
            IF SELF:NumKeys > 0
                IF SELF:HasLeft
                    VAR leftPage  := (CdxBranchPage) SELF:_tag:GetPage(SELF:LeftPtr)
                    VAR leftNode  := leftPage:Keys[leftPage:NumKeys -1]
                    VAR rightNode  := SELF:Keys[0]
                    IF ! ValidateKeys(leftNode, rightNode)
                         SELF:Debug("Corruption detected: Last key on left page",leftPage:PageNoX," is not < our first key", leftNode:DebugString,  rightNode:DebugString)
                         lOk := FALSE
                    ENDIF
                ENDIF
                IF SELF:HasRight
                    VAR leftNode  := SELF:Keys[SELF:NumKeys-1]
                    VAR rightPage := (CdxBranchPage) SELF:_tag:GetPage(SELF:RightPtr)
                    IF rightPage:NumKeys > 0
                        VAR rightNode := rightPage:Keys[0]
                        IF ! ValidateKeys(leftNode, rightNode)
                           SELF:Debug("Corruption detected: Last key on our page is not < first key on right page", rightPage:PageNoX, leftNode:DebugString, rightNode:DebugString)
                           lOk := FALSE
                        ENDIF
//                    ELSE
//                        System.Diagnostics.Debugger.Break()
                    ENDIF
                ENDIF
            ENDIF
            RETURN lOk

        METHOD Validate() AS VOID
            LOCAL last := NULL AS CdxBranch
            FOREACH VAR node IN SELF:Keys
                IF last != NULL
                    VAR nDiff := SELF:Tag:__Compare(last:Key, node:Key, last:Key:Length, last:Recno, node:Recno)
                    IF nDiff > 0
                        ? PageType, PageNoX, "Keys in wrong order", last:DebugString, node:DebugString
                    ENDIF
                ENDIF
                last := node
            NEXT
            SELF:ValidateSiblings()
            RETURN
#endif

        INTERNAL METHOD IsDuplicate(nRecno AS LONG, nChildPage AS LONG, cKey AS BYTE[]) AS LOGIC
            IF SELF:NumKeys > 0
                FOR VAR i := 0 TO SELF:NumKeys -1
                    VAR nRec  := SELF:GetRecno(i)
                    VAR nPage := SELF:GetChildPage(i)
                    IF nRec == nRecno
                        RETURN TRUE
                    ENDIF
                    IF nPage == nChildPage
                        RETURN TRUE
                    ENDIF
                NEXT
            ENDIF
            RETURN FALSE

        INTERNAL OVERRIDE METHOD Write() AS LOGIC
#ifdef TESTCDX
            IF ! SELF:ValidateKeys()
               SELF:_tag:ThrowException(Subcodes.ERDD_WRITE_NTX,Gencode.EG_CORRUPTION,  "CdxBranchPage.Write")
            ENDIF
#endif
            RETURN SUPER:Write()


    END CLASS
END NAMESPACE

