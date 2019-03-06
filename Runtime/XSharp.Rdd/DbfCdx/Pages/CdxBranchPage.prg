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
    [DebuggerDisplay("{Recno} {ChildPage} {KeyText}")];
    INTERNAL STRUCTURE CdxBranch
        INTERNAL Recno AS LONG
        INTERNAL ChildPage AS LONG
        INTERNAL Key   AS BYTE[]
        INTERNAL PROPERTY KeyText AS STRING GET SELF:Key:ToAscii()
        CONSTRUCTOR (nRecno AS LONG, nChild AS LONG, bKey AS BYTE[])
            SELF:Recno := nRecno
            SELF:ChildPage := nChild
            SELF:Key   := (BYTE[]) bKey:Clone()
            RETURN
    END STRUCTURE
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
       #region Constants
        PRIVATE CONST CDXBRANCH_OFFSET_NUMKEYS		:= 2	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_LEFTPTR		:= 4	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_RIGHTPTR		:= 8	AS WORD 
        PRIVATE CONST CDXBRANCH_HEADERLEN           := 12	AS WORD
        PRIVATE CONST CDXBRANCH_BYTESFREE           := 500  AS WORD
        #endregion
        
        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS Word)
            SUPER(bag, nPage, buffer)
            SELF:_keyLen  := nKeyLen
            SELF:_maxKeys := MaxKeysPerPage(nKeyLen)
            
            //? "Branch Page", SELF:PageNo:ToString("X"), SELF:NumKeys, "Startswith ", GetRecno(0), _bag:_oRDD:_Encoding:GetString(GetKey(0),0,_keyLen)
           INTERNAL VIRTUAL METHOD InitBlank(oTag AS CdxTag) AS VOID
            SELF:PageType   := CdxPageType.Branch
            SELF:NumKeys    := 0
            SELF:LeftPtr    := SELF:RightPtr   := -1
            SELF:Tag := oTag
            RETURN

            
        PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            RETURN _GetBytes(SELF:Buffer, nStart, _keyLen)
            
        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            RETURN _GetLongLE(nStart+_keyLen)
            
        PUBLIC METHOD SetKey(nPos AS Int32, key AS BYTE[]) AS VOID
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            Array.Copy(key, 0, SELF:Buffer, nStart, _keyLen)
            
            
        PUBLIC METHOD SetRecno(nPos AS Int32, nRecord AS Int32) AS VOID
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_HEADERLEN + nPos * (_keyLen + 8)
            _SetLongLE(nStart+_keyLen, nRecord)
            
            
        METHOD GetChildPage(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
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
            
        PUBLIC PROPERTY NumKeys AS WORD ;
        GET _GetWord(CDXBRANCH_OFFSET_NUMKEYS) ;
        SET _SetWord(CDXBRANCH_OFFSET_NUMKEYS, VALUE), isHot := TRUE
            
        INTERNAL PROPERTY LeftPtr AS Int32 ;
        GET _GetLong(CDXBRANCH_OFFSET_LEFTPTR) ;
        SET _SetLong(CDXBRANCH_OFFSET_LEFTPTR, VALUE), isHot := TRUE
            
        INTERNAL PROPERTY RightPtr AS Int32 ;
        GET _GetLong(CDXBRANCH_OFFSET_RIGHTPTR) ; 
        SET _SetLong(CDXBRANCH_OFFSET_RIGHTPTR, VALUE), isHot := TRUE
        #endregion                
         INTERNAL PROPERTY LastNode AS CdxPageNode GET SELF[SELF:NumKeys-1]
        
        INTERNAL PROPERTY MaxKeys AS LONG GET _maxKeys
        
        
        INTERNAL METHOD Add(node AS CdxPageNode) AS CdxAction
            IF SELF:NumKeys >= SELF:MaxKeys
                RETURN CdxAction.SplitBranch(SELF, node:Page:PageNo, node:Recno, node:KeyBytes)
            ENDIF
            LOCAL nPos := SELF:NumKeys AS WORD
            SELF:NumKeys++
            // node contains recno & keydata
            // node:Page has value for ChildPageNo
            SELF:_setNode(nPos, node)
            RETURN CdxAction.Ok

         INTERNAL METHOD Add(recno AS LONG, childPage AS LONG, key AS BYTE[]) AS CdxAction
            IF SELF:NumKeys >= SELF:MaxKeys
                RETURN CdxAction.SplitBranch(SELF, childPage, recno, key)
            ENDIF
            LOCAL nPos := SELF:NumKeys AS WORD
            SELF:NumKeys++
            // node contains recno & keydata
            // node:Page has value for ChildPageNo
            // TODO Combine the 3 operations so we do not have to calculate the offset 3 times
            SELF:SetRecno(nPos, recno)
            SELF:SetChildPage(nPos, childPage)
            SELF:SetKey(nPos, key)
            RETURN CdxAction.Ok
            
        INTERNAL METHOD Insert(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            // we allow to write at the position nMax
            IF nPos >= SELF:MaxKeys -1
                RETURN CdxAction.SplitBranch(SELF, node:ChildPageNo, node:Recno, node:KeyBytes)
            ENDIF
            IF nPos == nMax                 // Insert at end of list
                SELF:NumKeys += 1
                SELF:_setNode(nMax, node)
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
                if nI == 0
                    exit
                endif
            NEXT
            // and insert at the right spot
            _setNode(nPos, node)
            
            RETURN CdxAction.Ok
            
        INTERNAL METHOD Delete(nPos AS LONG) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            LOCAL result AS CdxAction
            IF nMax == 0 
                RETURN CdxAction.Delete(SELF)
            ENDIF
            IF nPos < 0 .OR. nPos > nMax-1
                RETURN CdxAction.OutOfBounds(SELF)
            ENDIF
            // node contains recno & keydata
            // node:Page has value for ChildPageNo
            // TODO Use MemCopy or ArrayCopy in stead for better performance
            if nMax > 1
                FOR VAR nI := nPos TO nMax-2
                    SELF:_copyNode(nI+1, nI)
                NEXT
            ENDIF
            SELF:NumKeys -= 1
            IF SELF:NumKeys == 0
                //SELF:Tag:SetChildToProcess(SELF:PageNo)
                result := CdxAction.Delete(SELF)
            ELSEIF nPos == SELF:NumKeys
               //SELF:Tag:SetChildToProcess(SELF:PageNo)
                result := CdxAction.ChangeParent(SELF)
            ELSE
                result := CdxAction.OK
            ENDIF
            RETURN result


            
        INTERNAL METHOD Replace(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            LOCAL nMax := SELF:NumKeys AS WORD
            IF nPos < 0 .OR. nPos >= nMax
                RETURN CdxAction.OutOfBounds(SELF)
            ENDIF
            RETURN _setNode(nPos, node)
            
        // Helper methods, these do not verify the bounds
        PRIVATE METHOD _copyNode(nSrc AS LONG, nTrg AS LONG) AS CdxAction
            IF nSrc != nTrg
                SELF:SetRecno(nTrg, SELF:GetRecno(nSrc))
                SELF:SetChildPage(nTrg, SELF:GetChildPage(nSrc))
                SELF:SetKey(nTrg, SELF:GetKey(nSrc))
            ENDIF
            RETURN CdxAction.Ok
            
        PRIVATE METHOD _setNode(nPos AS LONG, node AS CdxPageNode) AS CdxAction
            // Todo: Combine the 3 operations so we do not have to calculate the offset 3 times
            SELF:SetRecno(nPos, node:Recno)
            SELF:SetChildPage(nPos, node:Page:PageNo)
            SELF:SetKey(nPos, node:KeyBytes)
            RETURN CdxAction.Ok
            
        INTERNAL METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            LOCAL i as WORD
            sb := stringBuilder{}
            VAR item := SELF[0]
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}", SELF:PageType, SELF:PageNo, SELF:NumKeys))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            FOR i := 0 TO SELF:NumKeys-1
                item:Pos := i
                sb:AppendLine(String.Format("Item {0,2}, Page {1:X6}, Record {2,5} : {3} ", i, item:ChildPageNo, item:Recno, item:KeyText))
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
            
    END CLASS
END NAMESPACE 
