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

BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxPageBase class.
	/// </summary>
	INTERNAL ABSTRACT CLASS CdxTreePage INHERIT CdxPage
	    PROTECTED INTERNAL CONSTRUCTOR( oBag AS CdxOrderBag, nPage AS Int32, buffer AS BYTE[] )
            SUPER(oBag, nPage, buffer)
		RETURN

        PROPERTY NodeAttribute AS CdxNodeAttribute ;
          GET (CdxNodeAttribute) _GetWord(CDXPAGE_NODEATTR) ;
          SET _SetWord(CDXPAGE_NODEATTR, VALUE), isHot := TRUE
        ABSTRACT PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
        ABSTRACT PUBLIC PROPERTY NumKeys AS WORD  GET
    PROTECTED CONST CDXPAGE_NODEATTR	:= 0	AS WORD // WORD


	END CLASS
END NAMESPACE 
