USING System
USING System.Collections.Generic
USING System.Text
// make sure this is compiled early bound !
#pragma options ("lb", off)
BEGIN NAMESPACE XSharp.VFP
    //INTERNAL DELEGATE __IsInDelimiterArray(tc2Check AS Char) AS LOGIC
    //INTERNAL DELEGATE __GetWordCountActive(tcString AS STRING) AS LONG

    INTERNAL DEFINE Asc_A_Low := 097 // Asc_A + 32
    INTERNAL DEFINE Asc_Z_Low := 122 // Asc_Z + 32

    INTERNAL INTERFACE IGetWord
        METHOD GetWordCount(tcString AS STRING) AS LONG
        METHOD GetWordNum(tcString AS STRING, tnWordNum AS INT) AS STRING
        METHOD IsDelimiter(tc2Check AS Char) AS LOGIC
        METHOD SetStru() AS VOID
    END INTERFACE

    INTERNAL CLASS GetVfpDefault IMPLEMENTS IGetWord
        INTERNAL CONST _cSpace := c' '    AS Char // _Chr(ASC_BLANK)  Chr(032)[0]
        INTERNAL CONST _c_LinF := c'\n'   AS Char // _Chr(ASC_LF)     Chr(010)[0]
        INTERNAL CONST _c__Tab := c'\t'   AS Char // _Chr(ASC_Tab)    Chr(009)[0]
        * public const _c_CRet := c'\r'   as Char // _Chr(ASC_CR)     Chr(013)[0]

        INTERNAL oParent AS GetWordHandler

        VIRTUAL METHOD GetWordCount(tcString AS STRING) AS LONG
            // Start with a Word?
            LOCAL lnCount  := iif(SELF:IsDelimiter(tcString[0]), 0, 1) AS INT
            LOCAL lnSLast := tcString:Length-1 AS INT
            FOR LOCAL lnI := 0 AS INT TO lnSLast
                IF SELF:IsDelimiter(tcString[lnI])
                    //remove any delimiters following in row, like Space(2)
			        DO WHILE lnI < lnSLast
				        lnI := lnI + 1
	    		        IF !SELF:IsDelimiter(tcString[lnI])
                            *-- we have next normal chars considered "in word"
		    		        lnCount := lnCount+1
					        EXIT
				        ENDIF
			        ENDDO
		        ENDIF
	        NEXT
	        RETURN lnCount

        VIRTUAL METHOD GetWordNum(tcString AS STRING, tnWordNum AS INT) AS STRING
            *-- Checked: throws on .Null.
            *-- undocumented and differing vom vfp: if tnIndex is 0, return "" (as in vfp9)
            *-- but set WordCount as out Parameter to use in GetWordNum
            *-- pure stringdancing, stay with those methods
            * ? tcRefString, tnRefIndex, tcDelimiters:Length, tcDelimiters
            // Start with a Word?
            LOCAL lnCount := iif(SELF:IsDelimiter(tcString[0]), 0, 1) AS INT
            LOCAL lnSLast := tcString:Length-1 AS INT
            LOCAL lnStart := 0 AS INT, lnStop AS INT
            FOR LOCAL lnI := 0 AS INT TO lnSLast
                * ? tcRefString:Substring(lnI,1), tcDelimiters:Contains( tcRefString:Substring(lnI,1))
                IF SELF:IsDelimiter(tcString[lnI])
                    // Skip all other delimiters until next Word is found
                    DO WHILE lnI < lnSLast
                        lnI := lnI+1
                        IF !SELF:IsDelimiter(tcString[lnI])
                            *-- gets interesting... found a word
                            IF tnWordNum=lnCount
                                *-- if we start at a word and RefIndex is 1 we reach this here
                                lnStop := lnI-1
                            ELSE
                                lnCount := lnCount + 1
                                lnStart = lnI
                                FOR lnStop := lnI+1 TO lnSLast
                                    IF SELF:IsDelimiter(tcString[lnStop])
                                        EXIT
                                    ENDIF
                                NEXT
                            ENDIF
                            IF tnWordNum=lnCount
                                *-- here we HAVE correct word on both starts if tnIndex>0 and will RETURN
                                RETURN tcString:Substring(lnStart, lnStop-lnStart)
                            ELSE
                                *-- Add already found non-White
                                lnI := lnStop - 1
                                EXIT
                            ENDIF
                        ENDIF
                    ENDDO
                ENDIF
            NEXT
            *-- return empty as in vfp, as real payload takes inner exit
            RETURN ""

        VIRTUAL METHOD IsDelimiter(tc2Check AS Char) AS LOGIC
            /// direct switch checking against constants
            /// ToDo: flagged version, flags set on SetDict? Measure setup time!
            SWITCH  tc2Check
                CASE _cSpace
                CASE _c_LinF
                CASE _c__Tab
                    RETURN .t.
                OTHERWISE
                    RETURN .f.
            END SWITCH

        VIRTUAL METHOD SetStru() AS VOID
            RETURN

        CONSTRUCTOR(toParent AS GetWordHandler) AS VOID
             SELF:oParent := toParent
             RETURN
        END Constructor
    END CLASS

    INTERNAL CLASS GetDotNetWhite INHERIT GetVfpDefault

        OVERRIDE METHOD IsDelimiter(tc2Check AS Char) AS LOGIC
            RETURN System.Char.IsWhiteSpace(tc2Check)

        CONSTRUCTOR(toParent AS GetWordHandler) AS VOID
            SUPER(toParent)
    END CLASS

    INTERNAL CLASS GetSingle INHERIT GetVfpDefault
        *-- Single delimiter Comparison options
        PUBLIC cCmp AS Char

        OVERRIDE METHOD IsDelimiter(tc2Check AS Char) AS LOGIC
            RETURN SELF:cCmp:Equals(tc2Check)

        OVERRIDE METHOD SetStru() AS VOID
            SELF:cCmp := SELF:oParent:cRawStr[0]
            RETURN

        CONSTRUCTOR(toParent AS GetWordHandler) AS VOID
            SUPER(toParent)
    END CLASS

    INTERNAL CLASS GetSingleOpt INHERIT GetSingle
        // Direct call to Char:Equals without calling IsDelimiter gives sizable boost
        OVERRIDE METHOD GetWordCount(tcString AS STRING) AS LONG
            // Start with a Word?
            LOCAL lnCount  := iif(SELF:cCmp:Equals(tcString[0]), 0, 1) AS INT
            LOCAL lnSLast := tcString:Length-1 AS INT
            FOR LOCAL lnI := 0 AS INT TO lnSLast
                IF SELF:cCmp:Equals(tcString[lnI])
                    //remove any delimiters following in row, like Space(2)
			        DO WHILE lnI < lnSLast
				        lnI := lnI + 1
	    		        IF !SELF:cCmp:Equals(tcString[lnI])
                            *-- we have next normal chars considered "in word"
		    		        lnCount := lnCount+1
					        EXIT
				        ENDIF
			        ENDDO
		        ENDIF
	        NEXT
	        RETURN lnCount

        OVERRIDE METHOD GetWordNum(tcString AS STRING, tnWordNum AS INT) AS STRING
            *-- Checked: throws on .Null.
            *-- undocumented and differing vom vfp: if tnIndex is 0, return "" (as in vfp9)
            *-- but set WordCount as out Parameter to use in GetWordNum
            *-- pure stringdancing, stay with those methods
            * ? tcRefString, tnRefIndex, tcDelimiters:Length, tcDelimiters
            // Start with a Word?
            LOCAL lnCount := iif(SELF:cCmp:Equals(tcString[0]), 0, 1) AS INT
            LOCAL lnSLast := tcString:Length-1 AS INT
            LOCAL lnStart := 0 AS INT, lnStop AS INT
            FOR LOCAL lnI := 0 AS INT TO lnSLast
                * ? tcRefString:Substring(lnI,1), tcDelimiters:Contains( tcRefString:Substring(lnI,1))
                IF SELF:cCmp:Equals(tcString[lnI])
                    // Skip all other delimiters until next Word is found
                    DO WHILE lnI < lnSLast
                        lnI := lnI+1
                        IF !SELF:cCmp:Equals(tcString[lnI])
                            *-- gets interesting... found a word
                            IF tnWordNum=lnCount
                                *-- if we start at a word and RefIndex is 1 we reach this here
                                lnStop := lnI-1
                            ELSE
                                lnCount := lnCount + 1
                                lnStart = lnI
                                FOR lnStop := lnI+1 TO lnSLast
                                    IF SELF:IsDelimiter(tcString[lnStop])
                                        EXIT
                                    ENDIF
                                NEXT
                            ENDIF
                            IF tnWordNum=lnCount
                                *-- here we HAVE correct word on both starts if tnIndex>0 and will RETURN
                                RETURN tcString:Substring(lnStart, lnStop-lnStart)
                            ELSE
                                *-- Add already found non-White
                                lnI := lnStop - 1
                                EXIT
                            ENDIF
                        ENDIF
                    ENDDO
                ENDIF
            NEXT
            *-- return empty as in vfp, as real payload takes inner exit
            RETURN ""

        CONSTRUCTOR(toParent AS GetWordHandler) AS VOID
            SUPER(toParent)

    END CLASS

    INTERNAL CLASS GetMultiple INHERIT GetVfpDefault
        *-- if not single char, DotNetWhite or VfpWhite:
        PRIVATE CONST _c_A126 := (Char) 126 AS Char // _Chr(ASC_Z_Low)  Chr(126)[0], cut off ~ TO be safe on 1 off
        PRIVATE CONST _i_ArSz := 135 AS INT
        PRIVATE CONST _i_ArOf := _i_ArSz - 2 AS INT

        INTERNAL nInDict AS INT
	    INTERNAL alFlg AS LOGIC[]
	    PUBLIC hcCmp AS Dictionary <Char, BYTE>
        OVERRIDE METHOD IsDelimiter(tc2Check AS Char) AS LOGIC
            /// this way I don't have to care if lowest bits are filled via different encoding
            /// as comparison is made to one of the ASCII char identical to Unicode
            /// Array plus offest calculated with some spare room so no combination of bad luck
            /// can create index error
            LOCAL lnI := tc2Check:CompareTo(_c_A126) AS INT
            IF lnI<0
                *-- it is GUARANTEED OBJECT below Ascii 126
                *-- make certain, no index Error can slip in
                RETURN SELF:alFlg[_i_ArOf + lnI]
            ELSEIF SELF:nInDict>0
                *-- guard for other alphabets
                RETURN  SELF:hcCmp:ContainsKey(tc2Check)
            ENDIF
            RETURN .f.

        OVERRIDE METHOD SetStru() AS VOID
            *-- chk different version of setdict in 2. run of timings
            *-- when often calling routine for short takes, many rows in table
            SELF:nInDict := 0
            SELF:hcCmp := Dictionary<Char,BYTE>{}
            SELF:alFlg := LOGIC[]{_i_ArSz}
            *-- ? instead try timing
            * Self:hcCmp:Clear()
            * self:alFlg:Clear()

            LOCAL lcAdd AS Char
            LOCAL lnI AS INT
            FOR LOCAL lnRun := 0 AS INT TO SELF:oParent:cRawStr:Length-1  // Att: ARRAY base 1, STRING Base 0!!!
                lcAdd := SELF:oParent:cRawStr[lnRun]
                *-- Alternative coding with Try/Catch faster?? usually the Catch part incurs runtime hit?
                *-- but feels less clean, as source of problem (duplicate keys in string) known
                lnI := lcAdd:CompareTo(_c_A126)
                IF lnI<0
                    *-- it is GUARANTEED OBJECT below Ascii 126
                    *-- make certain, no index Error can slip in
                    SELF:alFlg[_i_ArOf + lnI] := .t.
                ELSEIF !SELF:hcCmp:ContainsKey(lcAdd)
                    *-- Do NOT add lcAdds already in earlier structure(s)
                    *-- to enable total block of dict access if zero delimiters outside checked range given
                    *-- but checked text consists mostly of unicode chars outside Latin:
                    *-- Greek, Cyrillic, Hirigana, Katagana, whatever
                    SELF:hcCmp:Add(lcAdd,0)
                    Self.nInDict := Self.nInDict + 1
                ENDIF
            NEXT

        CONSTRUCTOR(toParent AS GetWordHandler) AS VOID
            SUPER(toParent)
    END CLASS

//    INTERNAL CLASS GetMoreLanguage INHERIT GetMultiple
//        *-- exchange with Range for Greek or Cyrillic alphabet
//        PRIVATE CONST _c_Up_A := c'A'    AS Char // _Chr(ASC_A)      Chr(065)[0]
//        PRIVATE CONST _c_Up_Z := c'Z'    AS Char // _Chr(ASC_Z)      Chr(090)[0]
//        PRIVATE CONST _c_Lw_a := c'a'    AS Char // _Chr(ASC_A_Low)  Chr(097)[0]
//        PRIVATE CONST _c_Lw_z := c'z'    AS Char // _Chr(ASC_Z_Low)  Chr(122)[0]
//
//        OVERRIDE METHOD IsDelimiter(tc2Check AS Char) AS LOGIC
//            *-- even better would be 1 Compare, then checking against known numeric range,
//            *-- or checking several ranges, perhaps even adding DotNet.IsWhiteSpace into the fray
//            *-- generating+compiling "specific" IsDelimiter() on the fly
//            *-- either generating whole class dynamic only predicate delegate IsDelimiter
//            * local liCompared2BigAlpha :=
//           IF  tc2Check:CompareTo(_c_Lw_a)>=0 .and. tc2Check:CompareTo(_c_Lw_z)<=0    // lower letters first, AS ocurring more often
//                RETURN .f.
//            ELSEIF  tc2Check:CompareTo(_c_Up_A)>=0 .and. tc2Check:CompareTo(_c_Up_Z)<=0
//                RETURN .f.
//            ELSE //IF  .t. // SELF:isVfpWhiteSwitchChar(tc2Check)
//                /// Flag Check Version???
//                RETURN .t.
//            ENDIF
//            //RETURN  SELF:hcCmp:ContainsKey(tc2Check)
//
//        CONSTRUCTOR(toParent AS GetWordHandler) AS VOID
//            SUPER(toParent)
//    END CLASS
//
	INTERNAL CLASS GetWordHandler
	    /// Single
        /// lMany:
        /// todo: Check new dict+array vs. :clear
        *-- gets faster runtime if declared immutable? Not really...
        PUBLIC cRawStr AS STRING
        PUBLIC iMethod := 0 AS INT
        PUBLIC lAuto := .t. AS LOGIC
        PRIVATE CONST Delimiters := e" \n\t" AS STRING

        ///implemented as lazy loading in :SetActive, perhaps more DotNetStyle
        ///would be a property or access method creating an instance in member location slot
        ///when accessed and still .Null.
        PRIVATE STATIC oVfpDefault := NULL AS GetVfpDefault
        PRIVATE STATIC oDotWhiteSp := NULL AS GetDotNetWhite
        PRIVATE STATIC oSingleChar := NULL AS GetSingle
        PRIVATE STATIC oSingle_Opt := NULL AS GetSingleOpt
        PRIVATE STATIC oMultipleCs := NULL AS GetMultiple
        INTERNAL oActiveObjc := NULL AS IGetWord // IGetWord AS common INTERFACE would be cleaner

        *-- plus a few special methods to set different delimiters,
        *-- causing the object to "reoptimize" the code used to analyze/extract the string (
        PUBLIC METHOD SetDelimiter(tcDelimiter AS STRING) AS VOID
            *-- Somewhat ugly: here no option to set DotNetWhite or VfpDefault via "best" mode
            *-- of course setting :iMethod directly works, and "special" values
            *-- like "" for vfpFeault and .Null. for DotNetWhite have their own messy code smell
            *-- what is missing for alines() ? Perhaps implement alines(taArr, tcString, tnFlags, tcCHAR_List as Char[]) overload?
            *-- List of STRING Parameters, Flags: in own code Chr(13)+Chr(10) most often used "String"
            SELF:cRawStr := tcDelimiter
            IF tcDelimiter:Length=1
                IF SELF:iMethod <0
                    *-- guard for manual sets
                    SELF:iMethod := -SELF:iMethod
                ELSE
                    SELF:iMethod := 1
                ENDIF
            ELSEIF  tcDelimiter:Length=3 and tcDelimiter:IndexOf(GetVfpDefault._cSpace)>=0 ;
                and tcDelimiter:IndexOf(GetVfpDefault._c_LinF)>=0 ;
                and tcDelimiter:IndexOf(GetVfpDefault._c__Tab)>=0
                *-- ToDo: find most efficient check for Permutations of Tab, LF, Space(1)
                *-- Will switcg over to correct subobject
                *-- min() taking usuals not an option
                IF SELF:iMethod <0
                    *-- guard for manual sets
                    SELF:iMethod := -SELF:iMethod
                ELSE
                    SELF:iMethod := 22
                ENDIF
            ELSE
                SELF:iMethod := 37
            ENDIF
            IF SELF:lAuto
                SELF:SetActive()
            ENDIF


        PUBLIC METHOD SetActive() AS VOID
            // more elegant NullCheck+Create via property ?
            // Speed difference caused by property vs. field access ?
            // Different ways to check for .Null.?
            SWITCH SELF:iMethod
                CASE  -4
                    IF oDotWhiteSp == NULL
                        oDotWhiteSp := GetDotNetWhite{SELF}
                    ENDIF
                    SELF:oActiveObjc :=  oDotWhiteSp
                CASE  1
                    *if !Self:oSingle_Opt != null
                    *if Self:oSingle_Opt is null
                    IF oSingle_Opt == NULL
                        oSingle_Opt := GetSingleOpt{SELF}
                    ENDIF
                    SELF:oActiveObjc :=  oSingle_Opt
                CASE  3
                    IF oSingleChar == NULL
                        oSingleChar := GetSingle{SELF}
                    ENDIF
                    SELF:oActiveObjc :=  oSingleChar
                CASE 22
                    SELF:cRawStr := Delimiters
                    IF oVfpDefault == NULL
                        oVfpDefault := GetVfpDefault{SELF}
                    ENDIF
                    SELF:oActiveObjc := oVfpDefault //
                * case 34
                    * Hook for additional Char sets like Greek, Cyrillic...
                    * self:IsDelimiter := Self:isViaDictGuarded
                OTHERWISE
                    IF oMultipleCs == NULL
                        oMultipleCs := GetMultiple{SELF}
                    ENDIF
                    SELF:oActiveObjc := oMultipleCs
                END SWITCH
                IF SELF:lAuto
                    *-- better imp via additional interface for 3(1)&37, 22/4 don't need empty method
                    *-- no additional code as 1 inherits 3, but
                    *-- then have to guard via call "as" plus check for returned null
                    SELF:SetStru()
                ENDIF
            RETURN

        INTERNAL METHOD SetStru() AS VOID
            SELF:oActiveObjc:SetStru()

        INTERNAL METHOD isViaStr(tc2Check AS Char) AS LOGIC
            RETURN .t. // SELF:cRawStr:Contains((STRING) tc2Check)


    CONSTRUCTOR()
         *self:GetWordCount := self:GetWordCountMeth
         *self:GetWordNum := self:GetWordNumMeth
         *self:IsDelimiter := Self:isVia1Char
         RETURN

	END CLASS


END NAMESPACE // XSharp.VFP


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/getwordcount/*" />
INTERNAL FUNCTION GetDefaultWordHandler()  AS GetWordHandler
    LOCAL loSrch := GetWordHandler{} AS GetWordHandler
    loSrch:iMethod := 22    // Sidestepping check on cRawStr
    loSrch:SetActive()
    RETURN loSrch

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/getwordcount/*" />
FUNCTION GetWordCount( cString AS STRING) AS LONG
    LOCAL loSrch := GetDefaultWordHandler() AS GetWordHandler
    RETURN loSrch:oActiveObjc:GetWordCount( cString)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/getwordcount/*" />
FUNCTION GetWordCount( cString AS STRING, cDelimiters AS STRING) AS LONG
    LOCAL lnRefSwitch := 0 AS INT
    LOCAL lcTrack := "" AS STRING
    RETURN GetWordCount( cString , cDelimiters, REF lnRefSwitch, REF lcTrack)

INTERNAL FUNCTION GetWordCount( cString AS STRING, cDelimiters AS STRING, tnSwitch REF INT, tcTrack REF STRING) AS LONG
    *-- Checked: throws on .Null.
    *-- when moving class based optimized, presearch optimization times are avoided
    *-- so no systematic error to do up front when checking efficiency
    LOCAL lnReturn AS INT
    LOCAL loSrch := GetWordHandler{} AS GetWordHandler
    IF tnSwitch<0
            loSrch:iMethod := tnSwitch
    ENDIF
    = loSrch:SetDelimiter(cDelimiters)
    tnSwitch := loSrch:iMethod
    tnSwitch := loSrch:iMethod
    tcTrack := loSrch:oActiveObjc:GetType():Name
    lnReturn := loSrch:oActiveObjc:GetWordCount(cString)
    RETURN lnReturn

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/getwordnum/*" />
FUNCTION GetWordNum( cString AS STRING, nIndex AS INT) AS STRING
    LOCAL loSrch := GetDefaultWordHandler() AS GetWordHandler
    RETURN loSrch:oActiveObjc:GetWordNum( cString, nIndex)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/getwordnum/*" />
FUNCTION GetWordNum( cString AS STRING, nIndex AS INT, cDelimiters AS STRING) AS STRING
    *-- Checked: throws on .Null.
    LOCAL lnRefSwitch := 0 AS INT
    LOCAL lcTrack := "" AS STRING
    RETURN  GetWordNum( cString, nIndex, cDelimiters, REF lnRefSwitch, REF lcTrack)

INTERNAL FUNCTION GetWordNum( cString AS STRING, nIndex AS INT, cDelimiters AS STRING, tnSwitch REF INT, tcTrack REF STRING) AS STRING
    *-- Checked: throws on .Null., lcReturn still here to debug
    LOCAL lcReturn AS STRING
    LOCAL loSrch AS GetWordHandler
    loSrch := GetWordHandler {}
    = loSrch:SetDelimiter(cDelimiters)
    tnSwitch := loSrch:iMethod
    tcTrack := loSrch:oActiveObjc:GetType():Name
    lcReturn := loSrch:oActiveObjc:GetWordNum(cString, nIndex)
    RETURN lcReturn

    //STATIC FUNCTION __GetDelimiters() AS STRING
    //RETURN e" \n\t"
    //RETURN Space(1) + Chr(10) + Chr(9) // + Chr(13) CarriageReturn not included IN vfp9!
