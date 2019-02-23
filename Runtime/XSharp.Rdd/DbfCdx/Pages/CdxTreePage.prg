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
		RETURN
        #region Properties
        INTERNAL OVERRIDE PROPERTY PageType AS CdxPageType ;
          GET (CdxPageType) _GetWord(CDXPAGE_TYPE) ;
          SET _SetWord(CDXPAGE_TYPE, VALUE), isHot := TRUE

        INTERNAL PROPERTY Parent    AS CdxPage AUTO
        PROPERTY HasLeft    AS LOGIC GET LeftPtr    != 0 .AND. LeftPtr  != -1
        PROPERTY HasRight   AS LOGIC GET RightPtr   != 0 .AND. RightPtr != -1

        // Retrieve an index node in the current Page, at the specified position
        INTERNAL VIRTUAL PROPERTY SELF[ index AS LONG ] AS CdxPageNode
            GET
                RETURN CdxPageNode{ SELF:KeyLength, SELF, index }
            END GET
        END PROPERTY

        ABSTRACT INTERNAL PROPERTY LeftPtr		AS Int32 GET
        ABSTRACT INTERNAL PROPERTY RightPtr		AS Int32 GET
        ABSTRACT PUBLIC   PROPERTY NumKeys AS WORD  GET
        #endregion
        
        
        ABSTRACT PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetChildPage(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
        
	END CLASS
END NAMESPACE 
