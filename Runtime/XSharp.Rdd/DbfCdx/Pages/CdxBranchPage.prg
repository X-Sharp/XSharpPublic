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
        INTERNAL PROPERTY DebuggerDisplay as STRING GET  String.Format("{0,6} {1,6:X} {2}",   Recno, ChildPage, KeyText)

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
    - Branch pages are used to link the tree. Their contents is
    BYTE     attr    [ 2 ];    node type 
    BYTE     nKeys   [ 2 ];    number of keys 
    BYTE     leftPtr [ 4 ];    offset of left node or -1 
    BYTE     rightPtr[ 4 ];    offset of right node or -1
    BYTE     freeSpc [ 2 ];    free space available in a page 
    // array of key entries
    // each key entry is keyLen + 8 bytes long
    // BYTE Key data [keyLen]
    // BYTE record number[ 4]
    // BYTE child page [4]
    */
    INTERNAL CLASS CdxBranchPage INHERIT CdxTreePage 
        PROTECTED _keyLen    AS Int32
        PROTECTED _maxKeys   AS Int32
        PRIVATE   _numKeys   AS WORD
        PRIVATE   _leftPtr   AS LONG
        PRIVATE   _rightPtr  AS LONG
       
       #region Constants
        PRIVATE CONST CDXBRANCH_OFFSET_NUMKEYS		:= 2	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_LEFTPTR		:= 4	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_RIGHTPTR		:= 8	AS WORD 
        PRIVATE CONST CDXBRANCH_HEADERLEN           := 12	AS WORD
        PRIVATE CONST CDXBRANCH_BYTESFREE           := 500  AS WORD
        #endregion
        
        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS WORD)
            SUPER(bag, nPage, buffer)
            SELF:_keyLen  := nKeyLen
            SELF:_maxKeys := MaxKeysPerPage(nKeyLen)
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
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            RETURN _GetBytes( nStart, _keyLen)
            
        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            RETURN _GetLongLE(nStart+_keyLen)
            
        PUBLIC METHOD SetKey(nPos AS Int32, key AS BYTE[]) AS VOID
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            Array.Copy(key, 0, _buffer, nStart, _keyLen)
                 
            
        PUBLIC METHOD SetRecno(nPos AS Int32, nRecord AS Int32) AS VOID
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            _SetLongLE(nStart+_keyLen, nRecord)
            
            
        METHOD GetChildPage(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            RETURN _GetLongLE(nStart+_keyLen+4)
            
        METHOD SetChildPage(nPos AS Int32, nPage AS Int32) AS VOID
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            _SetLongLE(nStart+_keyLen+4, nPage)
            RETURN
            
        INTERNAL STATIC METHOD MaxKeysPerPage(nKeyLen AS WORD) AS WORD
            RETURN  CDXBRANCH_BYTESFREE / (nKeyLen + 8)
            
            
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
                return CdxAction.OK
            ENDIF
            // make sure that this key is larger than the last key
            if self:NumKeys > 0
                var lastKey := self:GetKey(SELF:NumKeys-1)
                VAR nDiff := SELF:Tag:__Compare(lastKey, key, key:Length)
                IF nDiff > 0
                    // New key needs to be inserted and not added to the page
                    var Pos := self:FindKey(key, recno, key:Length)
                    IF Pos > 0
                        // insert before this key
                        return SELF:Insert(pos, recno, childPageNo, key)
                    endif
                endif
            ENDIF
            var nPos := SELF:NumKeys
            SELF:NumKeys++
            SELF:SetRecno(nPos, recno)
            SELF:SetChildPage(nPos, childPageNo)
            SELF:SetKey(nPos, key)
            SELF:Write()
            RETURN CdxAction.Ok
            
        INTERNAL METHOD Insert(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            return SELF:Insert(nPos, node:Recno, node:Page:PageNo, node:KeyBytes)

        INTERNAL METHOD Insert(nPos AS LONG, recno as LONG, childPageNo as LONG, key as byte[]) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            // we allow to write at the position nMax
            //SELF:Debug(nPos, recno, childPageNo:ToString("X6"))
            IF SELF:IsDuplicate(recno, childPageNo, key)
                return CdxAction.OK
            ENDIF
            IF SELF:NumKeys >= SELF:MaxKeys
                //Debug( "triggers split ", "Pos", nPos, "Rec", recno, "Child", childPageNo:ToString("X8"))
                RETURN CdxAction.SplitBranch(SELF, ChildPageNo, recno, key, nPos)
            ENDIF
            IF nPos == nMax                 // Insert at end of list
                SELF:NumKeys += 1
                SELF:_setNode(nMax, recno, childPageNo, key)
                SELF:Validate()
                RETURN CdxAction.Ok
            ENDIF
            IF nPos < 0 .OR. nPos > nMax
                RETURN CdxAction.OutOfBounds(SELF)
            ENDIF
            // copy nodes up
            // TODO Use MemCopy or ArrayCopy in stead for better performance
            SELF:NumKeys += 1
            FOR VAR nI := nMax-1 DOWNTO nPos
                SELF:_copyNode(nI, nI+1)
                IF nI == 0
                    EXIT
                ENDIF
            NEXT
            // and insert at the right spot
            _setNode(nPos, recno, childPageNo, key)
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
                FOR VAR nI := nPos TO nMax-2
                    SELF:_copyNode(nI+1, nI)
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
            var nPos := SELF:FindKey(action:Key,action:Recno, action:Key:Length)
            if nPos >= SELF:NumKeys
                branches:Add( CdxBranch{action:Recno, action:ChildPage, action:Key})
            else
                branches:Insert(nPos, CdxBranch{action:Recno, action:ChildPage, action:Key})
            endif
            SELF:NumKeys := 0
            LOCAL half := branches:Count / 2 AS LONG
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
            VAR result := _setNode(nPos, node)
            SELF:Validate()
            SELF:Write()
            RETURN result

            
        // Helper methods, these do not verify the bounds
        PRIVATE METHOD _copyNode(nSrc AS LONG, nTrg AS LONG) AS CdxAction
            IF nSrc != nTrg
                SELF:SetRecno(nTrg, SELF:GetRecno(nSrc))
                SELF:SetChildPage(nTrg, SELF:GetChildPage(nSrc))
                SELF:SetKey(nTrg, SELF:GetKey(nSrc))
            ENDIF
            RETURN CdxAction.Ok

        PRIVATE METHOD _setNode(nPos AS LONG, recno as LONG, ChildPageNo as LONG, key as Byte[]) AS CdxAction
            // Todo: Combine the 3 operations so we do not have to calculate the offset 3 times
            //SELF:Debug(nPos, recno, childPageNo:ToString("X6"))
            SELF:SetRecno(nPos, recno)
            SELF:SetChildPage(nPos, childPageNo)
            SELF:SetKey(nPos, key)
            RETURN CdxAction.Ok

            
        PRIVATE METHOD _setNode(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            return SELF:_setNode(npos, node:Recno, node:Page:PageNo, node:KeyBytes)
            
        INTERNAL METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            LOCAL i := 0 AS WORD
            sb := stringBuilder{}
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}", SELF:PageType, SELF:PageNo, SELF:NumKeys))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            FOREACH branch as CdxBranch in SELF:Branches
                sb:AppendLine(String.Format("Item {0,2}, Page {1:X6}, Record {2,5} : {3} ", i, branch:ChildPage, branch:Recno, branch:KeyText))
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

         INTERNAL override METHOD FindKey(key as byte[], recno as long, keyLength as LONG) as WORD
            // return page number of page where pageKey > key
            // or page where pageKey == key and RecordNo > recno
            LOCAL nI as WORD
            IF SELF:NumKeys > 0
                // Branchpage can't really be empty...
                for nI := 0 to self:Numkeys -1
                    var pageKey := self:GetKey(nI)
                    var nDiff := SELF:Tag:__Compare(pageKey, key, keyLength)
                    SWITCH nDiff
                    CASE 0
                        if SELF:GetRecno(nI) > recno
                            return nI
                        endif
                    CASE 1
                        // pageKey is larger
                        return nI
                    OTHERWISE
                        // continue
                        NOP
                    end SWITCH
               next
            ENDIF
           return SELF:NumKeys-1

        METHOD Validate() AS VOID
            local last := NULL as CdxBranch
            FOREACH var Branch in SELF:Branches
                if last != null
                    var nDiff := SELF:Tag:__Compare(last:key, branch:key, last:key:Length)
                    if nDiff > 0
                        ? PageType, PageNo:ToString("X"), "Keys in wrong order", last:Key:ToAscii(), last:Recno, branch:Key:ToAscii(), branch:Recno
                    endif
                ENDIF
                last := branch
            NEXT
            


        METHOD IsDuplicate(nRecno as LONG, nChildPage as LONG, cKey as Byte[]) AS LOGIC
            IF SELF:NumKeys > 0
                FOR var i := 0 to SELF:NumKeys -1
                    var nRec  := SELF:GetRecno(i)
                    var nPage := SELF:GetChildPage(i)
                    IF nRec == nRecno
                        RETURN TRUE
                    ENDIF
                    IF nPage == nChildPage
                        return TRUE
                    ENDIF
                NEXT
            ENDIF
            RETURN FALSE
    END CLASS
END NAMESPACE 
