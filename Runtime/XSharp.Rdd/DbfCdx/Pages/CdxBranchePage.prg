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
	INTERNAL CLASS CdxBranchePage INHERIT CdxTreePage 
        PROTECTED _keyLen    AS Int32
        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS Int32)
            SUPER(bag, nPage, buffer)
            SELF:_keyLen := nKeyLen
            //? "Branch Page", SELF:PageNo:ToString("X"), SELF:NumKeys, "Startswith ", GetRecno(0), _bag:_oRDD:_Encoding:GetString(GetKey(0),0,_keyLen)

        #region ICdxKeyValue
        PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            RETURN _GetBytes(SELF:Buffer, nStart, _keyLen)

        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            RETURN _GetLongLE(nStart+_keyLen)
            
        #endregion
        
        METHOD GetChildPage(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            RETURN _GetLongLE(nStart+_keyLen+4)

            
            
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
#region Constants
     	PRIVATE CONST CDXBRANCH_OFFSET_NUMKEYS		:= 2	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_LEFTPTR		:= 4	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_RIGHTPTR		:= 8	AS WORD 
        PRIVATE CONST CDXBRANCH_KEY_OFFSET          := 12	AS WORD
#endregion

      METHOD Dump AS STRING
            LOCAL Sb AS stringBuilder
            sb := stringBuilder{}
            VAR item := SELF[0]
            sb:AppendLine("--------------------------")
            sb:AppendLine(String.Format("{0} Page {1:X6}, # of keys: {2}", SELF:PageType, SELF:PageNo, SELF:NumKeys))
            sb:AppendLine(String.Format("Left page reference {0:X6}", SELF:LeftPtr))
            FOR VAR i := 0 TO SELF:NumKeys-1
                item:Pos := i
                sb:AppendLine(String.Format("Item {0,2}, Page {1:X6}, Record {2,5} : {3} ", i, item:ChildPageNo, item:Recno, item:KeyText))
            NEXT
            sb:AppendLine(String.Format("Right page reference {0:X6}", SELF:RightPtr))
            RETURN sb:ToString()
	END CLASS
END NAMESPACE 
