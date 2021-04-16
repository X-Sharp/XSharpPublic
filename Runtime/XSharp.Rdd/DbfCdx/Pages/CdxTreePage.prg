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
#include "CdxDebug.xh"
BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxPageBase class.
	/// </summary>
    [DebuggerDisplay(e"{DebuggerDisplay,nq}")];
	INTERNAL ABSTRACT CLASS CdxTreePage INHERIT CdxPage 

        INTERNAL CONST CDXPAGE_TYPE	:= 0	AS WORD // WORD

        INTERNAL CONSTRUCTOR( oBag AS CdxOrderBag, nPage AS Int32, buffer AS BYTE[] )
            SUPER(oBag, nPage, buffer)
            SELF:_getValues()
            RETURN
        INTERNAL PROPERTY DebuggerDisplay AS STRING GET String.Format("{0} {1:X} Keys: {2}",PageType, PageNo, NumKeys)
        PRIVATE _pageType AS CdxPageType
        PRIVATE METHOD _getValues() AS VOID
            _pageType := (CdxPageType) SELF:_GetWord(CDXPAGE_TYPE)

        #region Properties
        INTERNAL OVERRIDE PROPERTY PageType AS CdxPageType ;
          GET _pageType ;
          SET SELF:_SetWord(CDXPAGE_TYPE, value), IsHot := TRUE, _pageType := value

        // FoxPro stores empty pointers as -1, FoxBASE as 0
        INTERNAL PROPERTY HasLeft    AS LOGIC GET LeftPtr    != 0 .AND. LeftPtr  != -1
        INTERNAL PROPERTY HasRight   AS LOGIC GET RightPtr   != 0 .AND. RightPtr != -1

        // Retrieve an index node in the current Page, at the specified position
        INTERNAL VIRTUAL PROPERTY SELF[ index AS WORD ] AS CdxPageNode
            GET
                RETURN CdxPageNode{ IIF(SELF:Tag != NULL, SELF:Tag:KeyLength,0), SELF, index }
            END GET
        END PROPERTY

        ABSTRACT INTERNAL PROPERTY LeftPtr		AS Int32 GET SET    // FoxPro stores empty pointers as -1, FoxBASE as 0
        ABSTRACT INTERNAL PROPERTY RightPtr		AS Int32 GET SET    // FoxPro stores empty pointers as -1, FoxBASE as 0
        ABSTRACT INTERNAL PROPERTY NumKeys      AS WORD  GET SET
        ABSTRACT INTERNAL PROPERTY LastNode     AS CdxPageNode GET
        INTERNAL PROPERTY NextFree              AS LONG ;
            GET IIF(LeftPtr >= 0, LeftPtr, 0);
            SET LeftPtr := IIF(VALUE >=0, VALUE, 0)// alias for LeftPtr
        // For debugging
        INTERNAL PROPERTY LeftPtrX  AS STRING GET LeftPtr:ToString("X8")
        INTERNAL PROPERTY RightPtrX AS STRING GET RightPtr:ToString("X8")
            
        INTERNAL PROPERTY FirstPageOnLevel AS CdxTreePage
            GET
                VAR oPage := SELF
                DO WHILE oPage:HasLeft
                    oPage := SELF:Tag:GetPage(oPage:LeftPtr)
                ENDDO
                RETURN oPage
            END GET
        END PROPERTY

        INTERNAL PROPERTY CurrentLevel AS IList<CdxTreePage>
        GET
            VAR oList := List<CdxTreePage>{}
            VAR oPage := SELF:FirstPageOnLevel
            VAR aPages := List<INT>{}
            oList:Add(oPage)
            DO WHILE oPage:HasRight
                VAR nRight := oPage:RightPtr
                IF !aPages:Contains(nRight)
                    oPage := SELF:Tag:GetPage(oPage:RightPtr)
                    oList:Add(oPage)
                    aPages:Add(nRight)
                ELSE
                    EXIT
                ENDIF
            ENDDO
            RETURN oList
        END GET
        END PROPERTY
#ifdef TESTCDX
        PRIVATE oPageLeft  AS CdxTreePage
        PRIVATE oPageRight AS CdxTreePage
        INTERNAL PROPERTY PageLeft AS CdxTreePage
            GET
                IF HasLeft
                    IF oPageLeft == NULL .OR. oPageLeft:PageNo != LeftPtr
                        oPageLeft := SELF:Tag:GetPage(SELF:LeftPtr)
                    ENDIF
                ELSE
                    oPageLeft := NULL
                ENDIF
                RETURN oPageLeft
            END GET
        END PROPERTY
        INTERNAL PROPERTY PageRight AS CdxTreePage
            GET
                IF HasRight
                    IF oPageRight == NULL .OR. oPageRight:PageNo != RightPtr
                        oPageRight := SELF:Tag:GetPage(SELF:RightPtr)
                    ENDIF
                ELSE
                    oPageRight := NULL
                ENDIF
                RETURN oPageRight
            END GET
        END PROPERTY
#endif
*/  
        #endregion
        
        ABSTRACT INTERNAL METHOD InitBlank(oTag AS CdxTag) AS VOID
        ABSTRACT INTERNAL METHOD GetRecno(nPos AS Int32) AS Int32
        ABSTRACT INTERNAL METHOD GetChildPage(nPos AS Int32) AS Int32
        ABSTRACT INTERNAL METHOD GetKey(nPos AS Int32) AS BYTE[]
        ABSTRACT INTERNAL METHOD GetChildren as IList<LONG>

         INTERNAL VIRTUAL METHOD Read() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Read()
            IF lOk
                SELF:_getValues()
            ENDIF
            RETURN lOk

        INTERNAL VIRTUAL METHOD Write() AS LOGIC
            IF SELF:LeftPtr == 0
                SELF:LeftPtr := -1
            ENDIF
            IF SELF:RightPtr == 0
                SELF:RightPtr := -1
            ENDIF            
            IF SELF:PageNo > 0
                System.Diagnostics.Debug.Assert(SELF:PageNo != SELF:RightPtr)
                System.Diagnostics.Debug.Assert(SELF:PageNo != SELF:LeftPtr)
            ENDIF
#ifdef TESTCDX
            SELF:Validate()
#endif
            RETURN SUPER:Write()


        ABSTRACT METHOD Validate AS VOID
        ABSTRACT METHOD ValidateKeys() AS LOGIC
        ABSTRACT METHOD ValidateLevel() AS LOGIC
        ABSTRACT METHOD ValidateSiblings() AS LOGIC 
            
        INTERNAL METHOD SetRoot() AS VOID
            SELF:PageType |= CdxPageType.Root
            RETURN

        INTERNAL METHOD ClearRoot() AS VOID
            SELF:PageType := _AND(SELF:PageType, _NOT(CdxPageType.Root))
            RETURN


        INTERNAL PROPERTY IsRoot AS LOGIC GET SELF:PageType:HasFlag(CdxPageType.Root)

       INTERNAL METHOD AddRightSibling(oNewRight AS CdxTreePage) AS VOID
            Debug.Assert(oNewRight != NULL_OBJECT)
            //DUMP( SELF:PageType:ToString(), oNewRight:PageNo:ToString("X") )
            IF oNewRight != NULL_OBJECT
                LOCAL  oOldRight := NULL AS CdxTreePage
                oNewRight:LeftPtr  := SELF:PageNo
                oNewRight:RightPtr := SELF:RightPtr
                IF SELF:HasRight
                    oOldRight := SELF:_tag:GetPage(SELF:RightPtr)
                    Debug.Assert(oOldRight:LeftPtr == SELF:PageNo)
                    oOldRight:LeftPtr := oNewRight:PageNo    
                ENDIF
                SELF:RightPtr     := oNewRight:PageNo
                IF oOldRight != NULL
                    oOldRight:Write()
                ENDIF
                IF oNewRight != NULL
                    oNewRight:Write()
                ENDIF
                SELF:Write()
            ELSE
                NOP
            ENDIF
            RETURN  
        INTERNAL ABSTRACT METHOD FindKey(key AS BYTE[], recno AS LONG, length AS LONG) AS WORD
        METHOD Debug(o PARAMS  OBJECT[] ) AS VOID
           LOCAL count := o:Length AS INT
           LOCAL x                 AS INT
           LOCAL cProc             AS STRING
           LOCAL sb as StringBuilder
            sb := StringBuilder{}
           cProc := ProcName(1):ToLower():PadRight(30)
           sb:Append(cProc+" ")
           sb:Append(SELF:PageNoX)
           sb:Append(" ")
           FOR x := 0 UPTO count-1
              sb:Append( o[x]:ToString())
              sb:Append( " ")
            NEXT
           _DebOut32(sb:ToString())
           RETURN
	END CLASS
END NAMESPACE 
