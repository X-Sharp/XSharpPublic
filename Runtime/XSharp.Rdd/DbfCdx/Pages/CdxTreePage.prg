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
    [DebuggerDisplay(e"{DebuggerDisplay,nq}")];
	INTERNAL ABSTRACT CLASS CdxTreePage INHERIT CdxPage 

        PROTECTED CONST CDXPAGE_TYPE	:= 0	AS WORD // WORD

	    PROTECTED INTERNAL CONSTRUCTOR( oBag AS CdxOrderBag, nPage AS Int32, buffer AS BYTE[] )
            SUPER(oBag, nPage, buffer)
            SELF:_getValues()
            RETURN
        INTERNAL PROPERTY DebuggerDisplay AS STRING GET String.Format("{0} {1:X} Keys: {2}",PageType, PageNo, NumKeys)
        PRIVATE _pageType AS CdxPageType
        PRIVATE METHOD _getValues() AS VOID
            _pageType := (CdxPageType) _GetWord(CDXPAGE_TYPE)

        #region Properties
        INTERNAL OVERRIDE PROPERTY PageType AS CdxPageType ;
          GET _pageType ;
          SET _SetWord(CDXPAGE_TYPE, VALUE), isHot := TRUE, _pageType := VALUE

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
        INTERNAL PROPERTY NextFree              AS LONG GET LeftPtr SET LeftPtr := VALUE // alias for LeftPtr
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
            oList:Add(oPage)
            DO WHILE oPage:HasRight 
                oPage := SELF:Tag:GetPage(oPage:RightPtr)
                oList:Add(oPage)
            ENDDO
            RETURN oList
        END GET
        END PROPERTY

#ifdef DEBUG
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
                IF HasRIght
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
        ABSTRACT PUBLIC METHOD GetRecno(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetChildPage(nPos AS Int32) AS Int32
        ABSTRACT PUBLIC METHOD GetKey(nPos AS Int32) AS BYTE[]
        ABSTRACT PUBLIC METHOD GetChildren as IList<LONG>

         PROTECTED INTERNAL VIRTUAL METHOD Read() AS LOGIC
            LOCAL lOk AS LOGIC
            lOk := SUPER:Read()
            IF lOk
                SELF:_getValues()
            ENDIF
            RETURN lOk

        PROTECTED INTERNAL VIRTUAL METHOD Write() AS LOGIC
            IF SELF:LeftPtr == 0
                SELF:LeftPtr := -1
            ENDIF
            IF SELF:RightPtr == 0
                SELF:RightPtr := -1
            ENDIF            
           IF SELF:PageNo != -1
                System.Diagnostics.Debug.Assert(SELF:PageNo != SELF:RightPtr)
                System.Diagnostics.Debug.Assert(SELF:PageNo != SELF:LeftPtr)
            ENDIF
#ifdef TESTCDX
            SELF:Validate()
#endif
            RETURN SUPER:Write()

#ifdef TESTCDX
        ABSTRACT METHOD Validate AS VOID
#endif
        METHOD SetRoot() AS VOID
            SELF:PageType |= CdxPageType.Root
            RETURN

        METHOD ClearRoot() AS VOID
            SELF:PageType := _AND(SELF:PageType, _NOT(CdxPageType.Root))
            RETURN


        PROPERTY IsRoot AS LOGIC GET SELF:PageType:HasFlag(CdxPageType.Root)


       INTERNAL METHOD AddRightSibling(oSibling AS CdxTreePage) AS VOID
            System.Diagnostics.Debug.Assert(oSibling != NULL_OBJECT)
            IF oSibling != NULL_OBJECT
                IF SELF:HasRight
                    //Debug(SELF:PageType, oSibling:PageNo:ToString("X8"),"between", SELF:PageNo:ToString("X8"),"and",SELF:RightPtr:ToString("X8"))
                    VAR oOldRight := SELF:Tag:GetPage(SELF:RightPtr)
                    oOldRight:LeftPtr := oSibling:PageNo
                ELSE
                    //Debug(SELF:PageType, oSibling:PageNo:ToString("X8"),"after  ", SELF:PageNo:ToString("X8"))
                ENDIF
                
                oSibling:RightPtr := SELF:RightPtr
                SELF:RightPtr     := oSibling:PageNo
                oSibling:LeftPtr  := SELF:PageNo
            ENDIF
            RETURN 
        INTERNAL ABSTRACT METHOD FindKey(key AS BYTE[], recno AS LONG, length AS LONG) AS WORD

        METHOD Debug(o PARAMS  OBJECT[] ) AS VOID
           LOCAL count := o:Length AS INT
           LOCAL x                 AS INT
           LOCAL cProc             AS STRING
           
           cProc := Procname(1):ToLower():PadRight(30) 
           Console.Write(cProc+" ")
           Console.Write(SELF:PageNo:ToString("X8"))
           Console.Write(" ")
           FOR x := 0 UPTO count-1
              Console.Write( o[x] )
              IF x < count
                 Console.Write( " " )
              ENDIF
           NEXT
           Console.WriteLine()
           AltD()
           RETURN
	END CLASS
END NAMESPACE 
