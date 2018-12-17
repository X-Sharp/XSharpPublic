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
using System.Diagnostics
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
		PROTECTED _keyLen as Int32
        
        INTERNAL CONSTRUCTOR( fileHandle AS IntPtr, nPage as Int32 , nKeyLen as Int32)
            SUPER(fileHandle, nPage)

        #region ICdxKeyValue
        Method GetKey(nPos as Int32) as BYTE[]
            LOCAL nStart as int
            Debug.Assert(nPos >= 0 .and. nPos < Self:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            return _GetBytes(SELF:Buffer, nStart, _KeyLen)

        METHOD GetRecno(nPos as Int32) as Int32
            LOCAL nStart as int
            Debug.Assert(nPos >= 0 .and. nPos < Self:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            return _GetLong(nStart+_KeyLen)
            
        #endregion
        
        METHOD GetChildPage(nPos as Int32) as Int32
            LOCAL nStart as int
            Debug.Assert(nPos >= 0 .and. nPos < Self:NumKeys)
            nStart := CDXBRANCH_KEY_OFFSET + nPos * (_keyLen + 8)
            return _GetLong(nStart+_KeyLen+4)
            
#region Properties
        PROPERTY NumKeys as WORD ;
          GET _GetWord(CDXBRANCH_OFFSET_NUMKEYS) ;
          SET _SetWord(CDXBRANCH_OFFSET_NUMKEYS, value), isHot := TRUE

        PROPERTY LeftPtr as Int32 ;
          GET _GetLong(CDXBRANCH_OFFSET_LEFTPTR) ;
          SET _SetLong(CDXBRANCH_OFFSET_LEFTPTR, value), isHot := TRUE

        PROPERTY RightPtr as Int32 ;
          GET _GetLong(CDXBRANCH_OFFSET_RIGHTPTR) ;
          SET _SetLong(CDXBRANCH_OFFSET_RIGHTPTR, value), isHot := TRUE
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
