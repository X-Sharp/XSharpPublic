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
	INTERNAL CLASS CdxBranchePage INHERIT CdxTreePage IMPLEMENTS ICdxKeyValue
		PROTECTED _keyLen AS Int32
        
        INTERNAL CONSTRUCTOR( bag AS CdxOrderBag , nPage AS Int32 , buffer AS BYTE[], nKeyLen AS Int32)
            SUPER(bag, nPage, buffer)
            _KeyLen := nKeyLen

        #region ICdxKeyValue
        PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            RETURN _GetBytes(SELF:Buffer, nStart, _KeyLen)

        PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            RETURN _GetLong(nStart+_KeyLen)
            
        #endregion
        
        METHOD GetChildPage(nPos AS Int32) AS Int32
            LOCAL nStart AS INT
            Debug.Assert(nPos >= 0 .AND. nPos < SELF:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            RETURN _GetLongLE(nStart+_KeyLen+4)

            
            
#region Properties
        PUBLIC PROPERTY NumKeys AS WORD ;
          GET _GetWord(CDXBRANCH_OFFSET_NUMKEYS) ;
          SET _SetWord(CDXBRANCH_OFFSET_NUMKEYS, VALUE), isHot := TRUE

        PROPERTY LeftPtr AS Int32 ;
          GET _GetLong(CDXBRANCH_OFFSET_LEFTPTR) ;
          SET _SetLong(CDXBRANCH_OFFSET_LEFTPTR, VALUE), isHot := TRUE

        PROPERTY RightPtr AS Int32 ;
          GET _GetLong(CDXBRANCH_OFFSET_RIGHTPTR) ;
          SET _SetLong(CDXBRANCH_OFFSET_RIGHTPTR, VALUE), isHot := TRUE
#endregion                
#region Constants
        PRIVATE CONST CDXBRANCHE_NODEATTR	        := 0	AS WORD 
     	PRIVATE CONST CDXBRANCH_OFFSET_NUMKEYS		:= 2	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_LEFTPTR		:= 4	AS WORD 
        PRIVATE CONST CDXBRANCH_OFFSET_RIGHTPTR		:= 8	AS WORD 
        PRIVATE CONST CDXBRANCH_KEY_OFFSET          := 12	AS WORD
#endregion        
	END CLASS
END NAMESPACE 
