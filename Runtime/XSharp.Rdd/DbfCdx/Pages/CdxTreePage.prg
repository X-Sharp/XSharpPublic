// CdxBlock.prg
// Created by    : fabri
// Creation Date : 10/25/2018 10:43:18 PM
// Created for   : 
// WorkStation   : FABPORTABLE

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxPageBase class.
	/// </summary>
	INTERNAL ABSTRACT CLASS CdxTreePage INHERIT CdxPage 

        PROTECTED CONST CDXPAGE_TYPE	:= 0	AS WORD // WORD

	    PROTECTED INTERNAL CONSTRUCTOR( oBag AS CdxOrderBag, nPage AS Int32, buffer AS BYTE[] )
            SUPER(oBag, nPage, buffer)
            _left := _right := parent := NULL
		RETURN
        #region Fields
        PROTECTED _right  := NULL AS CdxTreePage
        PROTECTED _left   := NULL AS CdxTreePage
        PROTECTED _parent := NULL AS CdxBranchPage
        #endregion

        #region Properties
        INTERNAL OVERRIDE PROPERTY PageType AS CdxPageType ;
          GET (CdxPageType) _GetWord(CDXPAGE_TYPE) ;
          SET _SetWord(CDXPAGE_TYPE, VALUE), isHot := TRUE

        PROPERTY HasLeft    AS LOGIC GET LeftPtr    != 0 .AND. LeftPtr  != -1
        PROPERTY HasRight   AS LOGIC GET RightPtr   != 0 .AND. RightPtr != -1

        // Retrieve an index node in the current Page, at the specified position
        INTERNAL VIRTUAL PROPERTY SELF[ index AS LONG ] AS CdxPageNode
            GET
                RETURN CdxPageNode{ IIF(SELF:Tag != NULL, SELF:Tag:KeyLength,0), SELF, index }
            END GET
        END PROPERTY

        ABSTRACT INTERNAL PROPERTY LeftPtr		AS Int32 GET SET
        ABSTRACT INTERNAL PROPERTY RightPtr		AS Int32 GET SET
        ABSTRACT PUBLIC   PROPERTY NumKeys      AS WORD  GET
        ABSTRACT INTERNAL PROPERTY BuffLen      AS WORD  GET

        INTERNAL PROPERTY Right  AS CdxTreePage   GET _right    SET _right  := VALUE
        INTERNAL PROPERTY Left   AS CdxTreePage   GET _left     SET _left   := VALUE
        INTERNAL PROPERTY Parent AS CdxBranchPage GET _parent   SET _parent := VALUE

        #endregion
        
        
        ABSTRACT PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetChildPage(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]

        PROTECTED INTERNAL VIRTUAL METHOD Write() AS LOGIC
            IF SELF:Right != NULL
                SELF:RightPtr := SELF:Right:PageNo
            ELSE
                SELF:RightPtr := -1
            ENDIF
            IF SELF:Left  != NULL
                SELF:LeftPtr := SELF:Left:PageNo
            ELSE
                SELF:LeftPtr := -1
            ENDIF
            RETURN SUPER:Write()

        METHOD SetRoot() AS VOID
            SELF:PageType |= CdxPageType.Root
            RETURN
	END CLASS
END NAMESPACE 
