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
		INTERNAL CONST CDXPAGE_OFFSET_NUMKEYS		:= 2	AS WORD // 2 WORD
		INTERNAL CONST CDXPAGE_OFFSET_LEFTPTR		:= 4	AS WORD // 4 LONGINT
		INTERNAL CONST CDXPAGE_OFFSET_RIGHTPTR 	    := 8	AS WORD // 4 LONGINT

        PROTECTED _numKeys        AS WORD
        PROTECTED _leftPtr        AS DWORD
        PROTECTED _rightPtr       AS DWORD

        PROTECTED VIRTUAL METHOD _clear() AS VOID
            SELF:_numKeys        := 0
            SELF:_leftPtr        := MISSING_PAGE
            SELF:_rightPtr       := MISSING_PAGE


        INTERNAL CONSTRUCTOR( oBag AS CdxOrderBag, nPage AS DWORD, buffer AS BYTE[] )
            SUPER(oBag, nPage, buffer)
            SELF:_getValues()
            RETURN

        INTERNAL PROPERTY DebuggerDisplay AS STRING GET String.Format("{0} {1:X} Keys: {2}",PageType, PageNo, NumKeys)
        PRIVATE _pageType AS CdxPageType

        PRIVATE METHOD _getValues() AS VOID
            _pageType := (CdxPageType) SELF:_GetWord(CDXPAGE_TYPE)
            _numKeys        := SELF:_GetWord(CDXPAGE_OFFSET_NUMKEYS)
            _leftPtr        := SELF:_GetDWord(CDXPAGE_OFFSET_LEFTPTR)
            _rightPtr       := SELF:_GetDWord(CDXPAGE_OFFSET_RIGHTPTR)
            IF SELF:_leftPtr == 0
                SELF:_leftPtr  := MISSING_PAGE
            ENDIF
            IF SELF:_rightPtr == 0
            SELF:_rightPtr   := MISSING_PAGE
            ENDIF

        #region Properties
        // FoxPro stores empty pointers as -1, FoxBASE as 0
        INTERNAL PROPERTY HasLeft    AS LOGIC GET PageIsValid(LeftPtr)
        INTERNAL PROPERTY HasRight   AS LOGIC GET PageIsValid(RightPtr)

        INTERNAL OVERRIDE PROPERTY PageType AS CdxPageType ;
          GET _pageType ;
          SET SELF:_SetWord(CDXPAGE_TYPE, value), IsHot := TRUE, _pageType := value


        // Retrieve an index node in the current Page, at the specified position
        INTERNAL VIRTUAL PROPERTY SELF[ index AS WORD ] AS CdxPageNode
            GET
                RETURN CdxPageNode{ IIF(SELF:Tag != NULL, SELF:Tag:KeyLength,0), SELF, index }
            END GET
        END PROPERTY

		INTERNAL PROPERTY NumKeys  AS WORD	GET _numKeys;
			SET SELF:_SetWord(CDXPAGE_OFFSET_NUMKEYS, VALUE), _numKeys := VALUE

		INTERNAL PROPERTY LeftPtr AS DWORD  GET _leftPtr;
			SET SELF:_SetDWord(CDXPAGE_OFFSET_LEFTPTR, VALUE), _leftPtr:= VALUE

		INTERNAL PROPERTY RightPtr AS DWORD	GET _rightPtr;
			SET SELF:_SetDWord(CDXPAGE_OFFSET_RIGHTPTR, VALUE), _rightPtr := VALUE

        ABSTRACT INTERNAL PROPERTY LastNode     AS CdxPageNode GET
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
            VAR aPages := List<DWORD>{}
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

        #endregion

        ABSTRACT INTERNAL METHOD InitBlank(oTag AS CdxTag) AS VOID
        ABSTRACT INTERNAL METHOD GetRecno(nPos AS Int32) AS DWORD
        ABSTRACT INTERNAL METHOD GetChildPage(nPos AS Int32) AS DWORD
        ABSTRACT INTERNAL METHOD GetKey(nPos AS Int32) AS BYTE[]
        ABSTRACT INTERNAL METHOD GetChildren as IList<DWORD>

         INTERNAL OVERRIDE METHOD Read() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Read()
            IF lOk
                SELF:_getValues()
            ENDIF
            RETURN lOk

        INTERNAL OVERRIDE METHOD Write() AS LOGIC
            IF PageIsValid(SELF:PageNo)
                System.Diagnostics.Debug.Assert(SELF:PageNo != SELF:RightPtr)
                System.Diagnostics.Debug.Assert(SELF:PageNo != SELF:LeftPtr)
            ENDIF
#ifdef TESTCDX
            SELF:Validate()
#endif
            RETURN SUPER:Write()

#ifdef TESTCDX
        ABSTRACT METHOD Validate AS VOID
        ABSTRACT METHOD ValidateKeys() AS LOGIC
        ABSTRACT METHOD ValidateLevel() AS LOGIC
        ABSTRACT METHOD ValidateSiblings() AS LOGIC
#endif
        INTERNAL METHOD SetRoot() AS VOID
            SELF:PageType |= CdxPageType.Root
            SELF:LeftPtr  := MISSING_PAGE
            SELF:RightPtr := MISSING_PAGE
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
        INTERNAL ABSTRACT METHOD FindKey(key AS BYTE[], recno AS DWORD, length AS LONG) AS WORD
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
