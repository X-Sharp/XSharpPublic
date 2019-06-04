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
USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Diagnostics
BEGIN NAMESPACE XSharp.RDD.CDX
    [DebuggerDisplay("{DebuggerDisplay,nq}")];
    INTERNAL CLASS CdxBranch
        INTERNAL Recno AS LONG
        INTERNAL ChildPage AS LONG
        INTERNAL Key   AS BYTE[]
        INTERNAL PROPERTY KeyText AS STRING GET SELF:Key:ToAscii()
        INTERNAL PROPERTY DebuggerDisplay AS STRING GET  String.Format("{0,6} {1,6:X} {2}",   Recno, ChildPage, KeyText)

        CONSTRUCTOR (nRecno AS LONG, nChild AS LONG, bKey AS BYTE[])
            SELF:Recno := nRecno
            SELF:ChildPage := nChild
            SELF:Key   := (BYTE[]) bKey:Clone()
            RETURN
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
    INTERNAL CLASS CdxBranchPage INHERIT CdxTreePage 
        PROTECTED _keyLen    AS Int32
        PROTECTED _dataLen   as Int32
        protected _pageNoOffSet as Int32
        PROTECTED _maxKeys   AS Int32
        PRIVATE   _numKeys   AS WORD
        PRIVATE   _leftPtr   AS LONG
        PRIVATE   _rightPtr  AS LONG
       
       #region Constants
        PRIVATE CONST CDXBRANCH_OFFSET_NUMKEYS		:= 2	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_LEFTPTR		:= 4	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_RIGHTPTR		:= 8	AS WORD 
        PRIVATE CONST CDXBRANCH_HEADERLEN           := 12	AS WORD     // Type (2), NumKeys (2), LeftPtr (4), RightPtr (4)
        PRIVATE CONST CDXBRANCH_BYTESFREE           := 500  AS WORD
        #endregion
        
        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer)
            SELF:_keyLen        := nKeyLen
            SELF:_dataLen       := _keyLen + 8
            SELF:_pageNoOffSet  := _keyLen + 4
            SELF:_maxKeys       := CDXBRANCH_BYTESFREE / _dataLen
            SELF:_getValues()

        PRIVATE METHOD _getValues() AS VOID
            _numKeys  := _GetWord(CDXBRANCH_OFFSET_NUMKEYS)
            _leftPtr  := _GetLong(CDXBRANCH_OFFSET_LEFTPTR)
            _rightPtr := _GetLong(CDXBRANCH_OFFSET_RIGHTPTR)

            
            //? "Branch Page", SELF:PageNo:ToString("X"), SELF:NumKeys, "Startswith ", GetRecno(0), _bag:_oRDD:_Encoding:GetString(GetKey(0),0,_keyLen)
        INTERNAL VIRTUAL METHOD InitBlank(oTag AS CdxTag) AS VOID
            SELF:PageType   := CdxPageType.Branch
            SELF:NumKeys    := 0
            SELF:LeftPtr    := SELF:RightPtr   := -1
            SELF:Tag        := oTag
            RETURN

        PROTECTED INTERNAL VIRTUAL METHOD Read() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Read()
            IF lOk
                SELF:_getValues()
            ENDIF
            RETURN lOk
            
        PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
            LOCAL nStart AS INT
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            RETURN _GetBytes( nStart, _keyLen)
            
        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            RETURN _GetLongLE(nStart+_keyLen)
            
        PUBLIC METHOD SetData(nPos as Int32, nRecord as Int32, nPage as Int32, key as Byte[]) AS CdxAction
            LOCAL nStart AS INT
            System.Diagnostics.Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            Array.Copy(key, 0, _buffer, nStart, _keyLen)
            _SetLongLE(nStart+_keyLen, nRecord)
            _SetLongLE(nStart+_pageNoOffSet, nPage)
            return CdxAction.Ok
            
        METHOD GetChildPage(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            nStart := CDXBRANCH_HEADERLEN + nPos * _dataLen
            RETURN _GetLongLE(nStart+_pageNoOffSet)
            
        PUBLIC METHOD GetChildren as IList<LONG>
            // used in the dump routine to avoid recursion
            VAR oList := List<LONG>{}
            FOR VAR i := 0 to SELF:NumKeys - 1
                oList:Add(SELF:GetChildPage(i))
            NEXT
            RETURN oList
            
            
        #region Properties
        // We read the values from our cache but write back to the cache and the buffer at the same time
        // The _Set.. methods set the isHot flag of the page automatically
            
        PUBLIC PROPERTY NumKeys AS WORD     GET _numkeys ;
            SET _SetWord(CDXBRANCH_OFFSET_NUMKEYS, VALUE),  _numKeys := VALUE
            
        INTERNAL PROPERTY LeftPtr AS Int32  GET _leftPtr ;
            SET _SetLong(CDXBRANCH_OFFSET_LEFTPTR, VALUE),  _leftPtr := VALUE
            
        INTERNAL PROPERTY RightPtr AS Int32 GET _rightPtr ; 
            SET _SetLong(CDXBRANCH_OFFSET_RIGHTPTR, VALUE),  _rightPtr := VALUE

        INTERNAL PROPERTY LastNode AS CdxPageNode GET IIF(SELF:NumKeys == 0, NULL, SELF[SELF:NumKeys-1])
        
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
                RETURN CdxAction.OK
            ENDIF
            // make sure that this key is larger than the last key
            IF SELF:NumKeys > 0
                VAR lastKey := SELF:GetKey(SELF:NumKeys-1)
                VAR nDiff := SELF:Tag:__Compare(lastKey, key, key:Length)
                IF nDiff > 0
                    // New key needs to be inserted and not added to the page
                    VAR Pos := SELF:FindKey(key, recno, key:Length)
                    IF Pos > 0 .and. pos < SELF:NumKeys
                        // insert before this key
                        RETURN SELF:Insert(pos, recno, childPageNo, key)
                    ELSEIF Pos == SELF:Numkeys
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
                RETURN CdxAction.OK
            ENDIF
            IF SELF:NumKeys >= SELF:MaxKeys
                //Debug( "triggers split ", "Pos", nPos, "Rec", recno, "Child", childPageNo:ToString("X8"))
                RETURN CdxAction.SplitBranch(SELF, ChildPageNo, recno, key, nPos)
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
            SetData(nPos, recno, childPageNo, key)
            SELF:Write()
            RETURN CdxAction.Ok
            
        INTERNAL METHOD Delete(nPos AS LONG) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            LOCAL result AS CdxAction
            IF nMax == 0 
                RETURN CdxAction.DeletePage(SELF)
            ENDIF
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
                //SELF:Tag:SetChildToProcess(SELF:PageNo)
                result := CdxAction.DeletePage(SELF)
            ELSEIF nPos == SELF:NumKeys
               //SELF:Tag:SetChildToProcess(SELF:PageNo)
                result := CdxAction.ChangeParent(SELF)
            ELSE
                result := CdxAction.OK
            ENDIF
            SELF:Write()
            RETURN result


 
        INTERNAL METHOD Split(oTarget AS CdxBranchPage, action AS CdxAction) AS CdxAction
            LOCAL branches  := SELF:Branches AS List<CdxBranch>
            VAR nPos := SELF:FindKey(action:Key,action:Recno, action:Key:Length)
            //Self:Debug("Split to page", oTarget:PageNo:ToString("X"))
            if nPos < self:NumKeys -1
                branches:Insert(nPos, CdxBranch{action:Recno, action:ChildPage, action:Key})
            elseif nPos >= self:NumKeys
                branches:Add( CdxBranch{action:Recno, action:ChildPage, action:Key})
            ELSE
                branches:Insert(nPos, CdxBranch{action:Recno, action:ChildPage, action:Key})
            endif
            var half := branches:Count /2
            SELF:NumKeys := 0
            FOR VAR i := 0 TO half
                VAR node := branches[i]
                SELF:Add(node:Recno, node:ChildPage, node:Key)
            NEXT
            FOR VAR I := half+1 TO branches:Count-1
                VAR node := branches[i]
                oTarget:Add(node:Recno, node:ChildPage, node:Key)
            NEXT
            RETURN CdxAction.Ok

            
        INTERNAL METHOD Replace(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            IF nPos < 0 .OR. nPos >= nMax
                RETURN CdxAction.OutOfBounds(SELF)
            ENDIF
            VAR result := SELF:SetData(npos, node:Recno, node:Page:PageNo, node:KeyBytes)
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

        INTERNAL METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            LOCAL i := 0 AS WORD
            sb := stringBuilder{}
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}", SELF:PageType, SELF:PageNo, SELF:NumKeys))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            FOREACH branch AS CdxBranch IN SELF:Branches
                sb:AppendLine(String.Format("Item {0,2}, Page {1:X6}, Record {2,6} : {3} ", i, branch:ChildPage, branch:Recno, branch:KeyText))
                i++
            NEXT
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            RETURN sb:ToString()


         INTERNAL PROPERTY Branches AS IList<CdxBranch>
            GET
                LOCAL oList AS List<CdxBranch>
                LOCAL nMax AS LONG
                nMax := SELF:NumKeys
                oList := List<CdxBranch>{nMax}
                FOR VAR i := 0 TO nMax -1
                    oList:Add( CdxBranch{SELF:GetRecno(i), SELF:GetChildPage(i), SELF:GetKey(i)})
                NEXT
                RETURN oList
            END GET
         END PROPERTY

         INTERNAL METHOD FindPage(nPage AS LONG) AS LONG
            FOR VAR i := 0 TO NumKeys -1
                IF GetChildPage(i) == nPage
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
                FOR nI := 0 TO SELF:Numkeys -1
                    VAR pageKey := SELF:GetKey(nI)
                    VAR nDiff := SELF:Tag:__Compare(pageKey, key, keyLength)
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
        METHOD ValidateLevel() AS VOID 
            var page := SELF
            DO WHILE page != NULL .and. page:HasLeft 
                page := (CdxBranchPage) SELF:_tag:GetPage(page:LeftPtr)
            ENDDO
            IF page != NULL
                DO WHILE page:HasRight
                    var leftKey   := page:Branches[page:NumKeys-1]
                    var rightPage := (CdxBranchPage) SELF:_tag:GetPage(page:RightPtr)
                    var rightKey  := rightPage:Branches[0]
                    VAR nDiff := SELF:Tag:__Compare(leftKey:key, rightKey:key, leftKey:key:Length)
                    IF nDiff > 0 .or. (nDiff == 0 .and. LeftKey:Recno >= RightKey:Recno)
                        SELF:Debug(page:PageNo:ToString("X"), "Keys in wrong order", LeftKey:Key:ToAscii():Trim(), LeftKey:Recno, RightKey:Key:ToAscii():Trim(), RightKey:Recno)
                        SELF:DumpKeys()
                        rightPage:DumpKeys()
                    ENDIF
                    page := rightPage
                enddo
            endif
            RETURN
        METHOD DumpKeys AS VOID
        Debug("Dump keys for page", SELF:PageNo:ToString("X"))
        FOREACH var Branch in SELF:Branches
            Debug("Child", Branch:ChildPage:ToString("X"), "Rec", Branch:Recno, Branch:KeyText:Trim())
        NEXT

        METHOD Validate() AS VOID
            LOCAL last := NULL AS CdxBranch
            FOREACH VAR Branch IN SELF:Branches
                IF last != NULL
                    VAR nDiff := SELF:Tag:__Compare(last:key, branch:key, last:key:Length)
                    IF nDiff > 0
                        ? PageType, PageNo:ToString("X"), "Keys in wrong order", last:Key:ToAscii(), last:Recno, branch:Key:ToAscii(), branch:Recno
                    ENDIF
                ENDIF
                last := branch
            NEXT
            IF SELF:HasLeft
                local oPage := SELF:_tag:GetPage(SELF:LeftPtr) AS CdxBranchPage
                IF oPage != NULL
                    if oPage:NumKeys > 0
                        VAR oLeftKey    := oPage:Branches[oPage:NumKeys-1]
                        VAR oRightKey   := SELF:Branches[0]
                        VAR nDiff := SELF:Tag:__Compare(oLeftKey:key, oRightKey:key, oLeftKey:key:Length)
                        IF nDiff > 0 .or. (nDiff == 0 .and. oLeftKey:Recno >= oRightKey:Recno)
                            SELF:Debug("Left Sibling", oPage:PageNo:ToString("X"), "Keys in wrong order", oLeftKey:Key:ToAscii():Trim(), oLeftKey:Recno, oRightKey:Key:ToAscii():Trim(), oRightKey:Recno)
                            oPage:DumpKeys()
                            SELF:DumpKeys()

                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            IF SELF:HasRight
                local oPage := SELF:_tag:GetPage(SELF:RightPtr) AS CdxBranchPage
                IF oPage != NULL
                    if oPage:NumKeys > 0
                        VAR oLeftKey   := SELF:Branches[SELF:NumKeys-1]
                        VAR oRightKey  := oPage:Branches[0]
                        VAR nDiff := SELF:Tag:__Compare(oLeftKey:key, oRightKey:key, oLeftKey:key:Length)
                        IF nDiff > 0 .or. (nDiff == 0 .and. oLeftKey:Recno >= oRightKey:Recno)
                            SELF:Debug("Right Sibling", oPage:PageNo:ToString("X"), "Keys in wrong order", oLeftKey:Key:ToAscii():Trim(), oLeftKey:Recno, oRightKey:Key:ToAscii():Trim(), oRightKey:Recno, self:NumKeys, oPage:NumKeys)
                            SELF:DumpKeys()
                            oPage:DumpKeys()
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
#endif            


        METHOD IsDuplicate(nRecno AS LONG, nChildPage AS LONG, cKey AS BYTE[]) AS LOGIC
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
    END CLASS
END NAMESPACE 
