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
    [DebuggerDisplay(e"{PageType} {PageNo.ToString(\"X\"),nq} Keys: {NumKeys}")];
	INTERNAL ABSTRACT CLASS CdxTreePage INHERIT CdxPage 

        PROTECTED CONST CDXPAGE_TYPE	:= 0	AS WORD // WORD

	    PROTECTED INTERNAL CONSTRUCTOR( oBag AS CdxOrderBag, nPage AS Int32, buffer AS BYTE[] )
            SUPER(oBag, nPage, buffer)
            RETURN

 
        #region Properties
        INTERNAL OVERRIDE PROPERTY PageType AS CdxPageType ;
          GET (CdxPageType) _GetWord(CDXPAGE_TYPE) ;
          SET _SetWord(CDXPAGE_TYPE, VALUE), isHot := TRUE

        // FoxPro stores empty pointers as -1, FoxBASE as 0
        PROPERTY HasLeft    AS LOGIC GET LeftPtr    != 0 .AND. LeftPtr  != -1
        PROPERTY HasRight   AS LOGIC GET RightPtr   != 0 .AND. RightPtr != -1

        // Retrieve an index node in the current Page, at the specified position
        INTERNAL VIRTUAL PROPERTY SELF[ index AS WORD ] AS CdxPageNode
            GET
                RETURN CdxPageNode{ IIF(SELF:Tag != NULL, SELF:Tag:KeyLength,0), SELF, index }
            END GET
        END PROPERTY

        ABSTRACT INTERNAL PROPERTY LeftPtr		AS Int32 GET SET    // FoxPro stores empty pointers as -1, FoxBASE as 0
        ABSTRACT INTERNAL PROPERTY RightPtr		AS Int32 GET SET    // FoxPro stores empty pointers as -1, FoxBASE as 0
        ABSTRACT PUBLIC   PROPERTY NumKeys      AS WORD  GET
        ABSTRACT INTERNAL PROPERTY LastNode     AS CdxPageNode GET
        INTERNAL PROPERTY NextFree AS Int32 GET LeftPtr SET LeftPtr := Value

  
        #endregion
        
        ABSTRACT INTERNAL METHOD InitBlank(oTag AS CdxTag) AS VOID
        ABSTRACT PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetChildPage(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]

        PROTECTED INTERNAL VIRTUAL METHOD Write() AS LOGIC
            IF SELF:LeftPtr == 0
                SELF:LeftPtr := -1
            ENDIF
            IF SELF:RightPtr == 0
                SELF:RightPtr := -1
            ENDIF            
           IF SELF:PageNo != -1
                Debug.Assert(SELF:PageNo != SELF:RightPtr)
                Debug.Assert(SELF:PageNo != SELF:LeftPtr)
            ENDIF
            RETURN SUPER:Write()

        METHOD SetRoot() AS VOID
            SELF:PageType |= CdxPageType.Root
            RETURN
	END CLASS
END NAMESPACE 
