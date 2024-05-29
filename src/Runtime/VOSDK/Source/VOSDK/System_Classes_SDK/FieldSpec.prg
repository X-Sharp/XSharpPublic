#pragma options ("enforceself", on)
/// <include file="System.xml" path="doc/FieldSpec/*" />
CLASS FieldSpec
    // Class that contains a number of properties of database fields and form fields ( controls )
    // HyperLabel   describes the FieldSpec
    // Status           describes the current status of the field or control associated with the FieldSpec,
    //                      after a validation ( NIL if the validation passed )
    // Type             the underlying storage type ( number, string, logic, date, memo )
    // Length           the field length
    // Decimals     the number of decimals
    // MinLength        the minimum number of characters ( for example, a STATE field may be defined with
    //                      Length and MinLength of 2, while a PASSWORD may have a Length of 10 and a MinLength of 4
    // Min, Max     the data range
    // Validation       codeblock. Also note that a FieldSpec subclass may be given a Validation method, which overrides
    //                      the codeblock.
    // Picture          formatting string
    // Diagnostics  HyperLabels for each of the validation rules. Note that a HyperLabel contains not only
    //                      a diagnostic message, but also a help context to provide for context sensitive help
    //                      after a validation failure.
    PROTECT oHyperLabel AS HyperLabel
    PROTECT oHLStatus 	AS HyperLabel
    PROTECT wType 			AS DWORD        //RvdH 070122 Changed from WORD to DWORD
    PROTECT cType 			AS STRING
    PROTECT lNumeric 		AS LOGIC
    PROTECT oHLType 		AS HyperLabel
    PROTECT wLength 		AS DWORD				//RvdH 070122 Changed from WORD to DWORD
    PROTECT oHLLength 	AS HyperLabel
    PROTECT wDecimals 	AS DWORD         //RvdH 070122 Changed from WORD to DWORD
    PROTECT lRequired 	AS LOGIC
    PROTECT oHLRequired AS HyperLabel
    PROTECT wMinLength 	AS DWORD
    PROTECT oHLMinLength AS HyperLabel
    PROTECT uMin, uMax  AS USUAL
    PROTECT oHLRange 		AS HyperLabel
    PROTECT cbValidation AS USUAL // AS CODEBLOCK
    PROTECT oHLValidation AS HyperLabel
    PROTECT cPicture 		AS STRING
        // UH 01/30/1997
    PROTECT lNullable AS LOGIC


        //RvdH-030916 Strong typing
    /// <exclude />
    method __GetHLRange  as void strict
        //RvdH-030916 Strong typing
        IF oHLRange == NULL_OBJECT
            IF IsNil(uMin) .AND. IsNil(uMax)
                RETURN
            ENDIF
            IF IsNil(uMin) .AND. !IsNil(uMax)
                oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
                    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMAX,oHyperLabel:Name,AsString( uMax ) ) }
            ENDIF
            IF !IsNil(uMin) .AND. IsNil(uMax)
                oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
                    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMIN,oHyperLabel:Name,AsString( uMin ) ) }
            ENDIF
            IF !IsNil(uMin) .AND. !IsNil(uMax)
                oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
                    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDRANGE,oHyperLabel:Name,AsString(uMin),AsString( uMax )) }
            ENDIF
        ENDIF
        RETURN


    /// <include file="System.xml" path="doc/FieldSpec.AsString/*" />
    method AsString( )
        RETURN oHyperLabel:Caption


    /// <include file="System.xml" path="doc/FieldSpec.Decimals/*" />
    access Decimals
        // Returns the number of decimals
        RETURN wDecimals




    /// <include file="System.xml" path="doc/FieldSpec.Decimals/*" />
    assign Decimals (uDecimals)


        IF IsNil(uDecimals )
            wDecimals := 0
        ELSEIF IsNumeric( uDecimals )
            wDecimals := uDecimals
        ELSE
            DbError{ SELF, #Decimals, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADDECIMALS), uDecimals, "uDecimals" }:Throw()
        ENDIF
        return


    /// <exclude />
    ACCESS __HyperLabel as HyperLabel
        RETURN oHyperLabel
    /// <include file="System.xml" path="doc/FieldSpec.HyperLabel/*" />
    access HyperLabel
        // Returns the HyperLabel object
        RETURN oHyperLabel


    /// <include file="System.xml" path="doc/FieldSpec.ctor/*" />
    constructor( oHLName, uType, uLength, uDecimals )
        // Instantiation parameters for FieldSpec
        // oHLName      ( required ) HyperLabel
        // uType            ( required ) the type, either as one of the data type keywords (STRING, INT, LOGIC, etc.)
        //                      or as a 1-char string ("N","C","D","L","M")
        // uLength          ( required for some data types, not for LOGIC for example )
        // uDecimals        ( optional ) defaults to 0
        if oHLName is HyperLabel
            oHyperLabel := oHLName
        ELSEIF IsSymbol( oHLName ) .OR. IsString( oHLName )
            oHyperLabel := HyperLabel{ oHLName }
        ELSE
            DbError{SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADNAME), oHLName, "oHLName" }:Throw()
        ENDIF
        SELF:_SetType(uType, #Init)
        if uType is STRING var strType .and. Left(Upper(strType),1)="I"
            uLength  := 10
            uDecimals := 0
        endif

        IF IsNumeric( uLength )
            wLength := uLength
        ELSE
            IF SELF:lNumeric .OR. SELF:cType="C"
                //  UH 12/16/1999
                DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADLENGTH), uLength, "uLength" }:Throw()
            ENDIF
        ENDIF


        IF IsNil(uDecimals )
            wDecimals := 0
        ELSEIF IsNumeric( uDecimals )
            wDecimals := uDecimals
        ELSE
            DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADDECIMALS), uDecimals, "uDecimals" }:Throw()
        ENDIF
        return




    /// <include file="System.xml" path="doc/FieldSpec.Length/*" />
    ACCESS Length
        // Returns the length of the field
        RETURN wLength


    /// <include file="System.xml" path="doc/FieldSpec.Maximum/*" />
    ACCESS Maximum
        RETURN uMax


    /// <include file="System.xml" path="doc/FieldSpec.Minimum/*" />
    ACCESS Minimum
        RETURN uMin


    /// <include file="System.xml" path="doc/FieldSpec.MinLength/*" />
    ACCESS MinLength
        RETURN wMinLength


    /// <include file="System.xml" path="doc/FieldSpec.Nullable/*" />
    ACCESS Nullable
        RETURN SELF:lNullable


    /// <include file="System.xml" path="doc/FieldSpec.Nullable/*" />
    ASSIGN Nullable( lNew )
        RETURN SELF:lNullable := lNew
    /// <include file="System.xml" path="doc/FieldSpec.PerformValidations/*" />
    method PerformValidations(uValue, arg)
        // Performs all the validations on the specified value: required field, data type compliance, range, etc.
        // Returns a LOGIC indicating whether the validation succeeded.
        // Also sets the STATUS to the appropriate HyperLabel for the validation rule that failed,
        // sets it to NIL if it succeeded
        //
        LOCAL cValue      AS STRING
        LOCAL wLen, wDecLen:=0, i  AS DWORD
        LOCAL cDecSep, cTmp   AS STRING


        oHLStatus := NULL_OBJECT


        //  UH 01/31/1997
        IF SELF:lNullable .AND. IsNil(uValue)
            return .t.
        ENDIF


        if (IsString(uValue) .and. Empty(AllTrim(uValue))) .or. ;
                (IsDate(uValue) .and. uValue == null_date)
            // Check required
            IF lRequired
                IF IsNil(oHLRequired)
                    oHLRequired := HyperLabel{ #FieldSpecRequired, , VO_Sprintf(__CAVOSTR_DBFCLASS_REQUIRED,oHyperLabel:Name) }
                ENDIF


                oHLStatus := oHLRequired
                RETURN FALSE
            ENDIF
        ELSE
            //  UH 01/06/2000
            IF IsNil(uValue) .AND. SELF:wType == OBJECT
                return .t.
            ENDIF


            // Check data type (no conversions here!)
            IF !(UsualType(uValue) == wType .OR. (SELF:lNumeric .AND. IsNumeric(uValue)) .OR. ((wType == TYPE_MULTIMEDIA) .AND. IsString(uValue)))
                IF oHLType == NULL_OBJECT
                    oHLType := HyperLabel{ #FieldSpecType, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDTYPE,oHyperLabel:Name,TypeAsString(wType)) }
                ENDIF
                oHLStatus := oHLType


                RETURN FALSE
            ENDIF


            // Check max and min length
            IF SELF:lNumeric
                IF IsNil(uValue)
                    cValue := NULL_STRING
                ELSE
                    cValue := AllTrim(AsString(uValue))
                ENDIF


                IF !Empty(SELF:cPicture)
                    cDecSep := Chr(SetDecimalSep())


                    // NOTE: picture templates need to be in U.S. format for output
                    // to be correct when set from control panel.
                    IF At2(".", SELF:cPicture) > 0
                        // get decimal locations according to picture
                        cTmp := SubStr2(SELF:cPicture, At2(".", SELF:cPicture) + 1)


                        wLen := SLen(cTmp)
#ifdef __VULCAN__
                        i := 0
                        DO WHILE i < wLen
                            if Char.IsDigit( cTmp, (int) i )
                                wDecLen++
                            ENDIF
                            i++
                        enddo
#else
                        FOR i := 1 UPTO wLen
                            IF IsDigit(PSZ(_CAST, SubStr3(cTmp, i, 1)))
                                wDecLen++
                            ENDIF
                        NEXT
#endif


                        // substring cValue based on control panel's decimal sep.
                        cValue := SubStr3(cValue, 1, At2(cDecSep, cValue) + wDecLen)


                    ENDIF
                ENDIF
            ELSEIF wType == STRING
                cValue := uValue
            ENDIF


            wLen := SLen(cValue)


            IF wLen > wLength .AND. ;
                    !(SELF:cType == "M" /*.AND. wLength == 10 .AND. !wLength > 65536*/ )


                IF oHLLength == NULL_OBJECT
                    oHLLength := HyperLabel{ #FieldSpecLength, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDLENGTH,oHyperLabel:Name,Str( wLength ) ) }
                ENDIF


                oHLStatus := oHLLength
                RETURN FALSE


            ELSEIF wType == STRING .AND. wLen < wMinLength
                IF  oHLMinLength = NULL_OBJECT
                    oHLMinLength := HyperLabel{ #FieldSpecMinLength, ,  ;
                        VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMINLENGTH,oHyperLabel:Name,Str( wMinLength ) ) }
                ENDIF
                oHLStatus := oHLMinLength
                RETURN FALSE
            ENDIF


            // Check range
            IF !IsNil(uMin) .AND. uValue < uMin
                IF IsNil(oHLRange)
                    SELF:__GetHLRange( )
                ENDIF
                oHLStatus := oHLRange
                RETURN FALSE
            ENDIF


            IF !IsNil(uMax) .AND. uValue > uMax
                IF IsNil(oHLRange)
                    SELF:__GetHLRange( )
                ENDIF
                oHLStatus := oHLRange


                RETURN FALSE
            ENDIF
        ENDIF


        // Check validation method or codeblock
        IF !SELF:Validate(uValue, arg)
            IF oHLStatus == NULL_OBJECT
                IF oHLValidation == NULL_OBJECT
                    oHLValidation := HyperLabel{ #FieldSpecValidate, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDVALUE,oHyperLabel:Name) }
                ENDIF
                oHLStatus := oHLValidation      // Fill in status if not done by client code
            ENDIF


            RETURN FALSE
        ENDIF


        RETURN TRUE


    /// <include file="System.xml" path="doc/FieldSpec.Picture/*" />
    ACCESS Picture
        RETURN cPicture


    /// <include file="System.xml" path="doc/FieldSpec.Picture/*" />
    assign Picture( cNewPicture )
        //ASSERT _DYNCHECKERRORBOX( )
        RETURN cPicture := cNewPicture


    /// <include file="System.xml" path="doc/FieldSpec.Required/*" />
    ACCESS Required
        RETURN lRequired


    /// <include file="System.xml" path="doc/FieldSpec.SetLength/*" />
    METHOD SetLength( w, oHL )
        // The length is set through the instantiation parameter, and is not normally changed later
        // This method does allow changing the length and, more usefully,
        // the HyperLabel diagnostic for the length check
        // Both parameters are optional, if one is not provided the corresponding value is not changed
        IF !IsNil( w )
            IF IsNumeric( w )
                wLength := w
            ELSE
                DbError{ SELF, #SetLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADSI), w, "w" }:Throw()
            ENDIF
        ENDIF
        IF oHL # NIL
            IF oHL IS HyperLabel
                oHLLength := oHL
            ELSE
                DbError{ SELF, #SetLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
            ENDIF
        ENDIF
        RETURN NIL


    /// <include file="System.xml" path="doc/FieldSpec.SetMinLength/*" />
    method SetMinLength( w, oHL )
        // This method is used to set the minimum length,
        // and the HyperLabel diagnostic for the minlength check (applies to string data only)
        // Both parameters are optional, if one is not provided the corresponding value is not changed
        IF !IsNil( w )
            IF IsNumeric( w )
                wMinLength := w
            ELSE
                DbError{ SELF, #SetMinLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADSI), w, "w" }:Throw()
            ENDIF
        ENDIF
        IF !IsNil(oHL)
            if oHL is HyperLabel
                oHLMinLength := oHL
            ELSE
                DbError{ SELF, #SetMinLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
            ENDIF
        ENDIF
        RETURN NIL


    /// <include file="System.xml" path="doc/FieldSpec.SetRange/*" />
    method SetRange( uMinimum, uMaximum, oHL )
        // Sets the range and the HyperLabel for the range check error message
        // All parameters are optional, if one is not provided the corresponding value is not changed
        IF !IsNil(uMinimum)
            uMin := uMinimum
        ENDIF
        IF !IsNil(uMaximum)
            uMax := uMaximum
        ENDIF
        IF !IsNil(oHL)
            if IsObject(oHL) .and. __Usual.ToObject(oHL) is HyperLabel
                oHLRange := oHL
            ELSE
                DbError{ SELF, #SetRange, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
            ENDIF
        ENDIF
        RETURN NIL


    /// <include file="System.xml" path="doc/FieldSpec.SetRequired/*" />
    method SetRequired( lReq, oHL )
        // This method is used to specify whether this is a required field,
        // and the HyperLabel diagnostic for the required check
        // Both parameters are optional; if lReq is omitted TRUE is assumed;
        // if the HyperLabel is not provided the current value is not changed
        IF IsNil(lReq)
            lRequired := TRUE
        ELSEIF IsLogic( lReq )
            lRequired := lReq
        ELSE
            DbError{ SELF, #SetRequired, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADLREQ), lReq, "lReq" }:Throw()
        ENDIF
        IF !IsNil(oHL)
            if oHL is HyperLabel
                oHLRequired := oHL
            ELSE
                DbError{ SELF, #SetRequired, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
            ENDIF
        ENDIF
        RETURN NIL

    method _SetType(uType as USUAL, symMethod as SYMBOL) as void
        IF !IsNil(uType)
            if  uType  is string var strType
                self:lNullable := strType:IndexOf(":0") > 0
                SELF:cType := Left(Upper(strType),1)
                SWITCH SELF:cType
                CASE "C"            // Char
                CASE "M"            // Memo
                CASE "V"            // VarChar
                    SELF:wType := STRING
                CASE "N"            // Numeric
                CASE "F"            // Float
                CASE "B"            // Double
                CASE "I"            // Integer
                CASE "Y"            // Currency
                    SELF:wType := FLOAT
                    SELF:lNumeric:=TRUE
                    SELF:cType := "N"
                case "L"
                    wType := LOGIC
                CASE "D"                // Date
                CASE "T"                // DateTime
                    SELF:wType := DATE


                CASE "O"                // Object
                    SELF:wType := object
                    SELF:cType := "O"
                CASE "G"                // General
                CASE "P"                // Picture
                CASE "Q"                // VarBinary
                    SELF:wType := object
                    SELF:cType := "M"

                CASE "X"
                    wType := TYPE_MULTIMEDIA

                case "0"
                    SELF:cType := ""
                OTHERWISE
                    SELF:cType := ""
                    DbError{ SELF, symMethod, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
                END SWITCH
            ELSE
                SELF:wType := uType
                IF SELF:wType = STRING
                    SELF:cType := "C"
                ELSEIF SELF:wType = LOGIC
                    SELF:cType := "L"
                ELSEIF wType = DATE
                    SELF:cType := "D"
                elseif SELF:wType=int   .or.; // also Long
                       SELF:wType=float .or.;
                       SELF:wType=byte  .or.;
                       SELF:wType=shortint .or.;
                       SELF:wType=word  .or.;
                       SELF:wType=dword .or.;
                       SELF:wType=real4 .or.;
                       SELF:wType=real8
                    SELF:lNumeric := TRUE
                    SELF:cType := "N"
                ELSEIF (SELF:wType == TYPE_MULTIMEDIA)
                    SELF:cType := "X"
                ELSE
                    SELF:wType := 0
                    DbError{ SELF, symMethod, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
                ENDIF
            ENDIF
        ENDIF
        RETURN


    /// <include file="System.xml" path="doc/FieldSpec.SetType/*" />
    method SetType( uType, oHL )
        // The storage type is normally set as an instantiation parameter and is not changed later.
        // This method does allow the storage type to be changed and, more usefully,
        // the HyperLabel diagnostic for the storage type check
        // Both parameters are optional, if one is not provided the corresponding value is not changed
        SELF:_SetType(uType, #SetType)
        IF !IsNil(oHL)
            if oHL is HyperLabel
                oHLType := oHL
            ELSEIF IsSymbol( oHL ) .OR. IsString( oHL )
                oHLType := HyperLabel{ oHL }
            ELSE
                DbError{ SELF, #SetType, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
            ENDIF
        ENDIF
        RETURN NIL




    /// <include file="System.xml" path="doc/FieldSpec.SetValidation/*" />
    method SetValidation( cb, oHL )
        // Used to set the validation codeblock and its corresponding HyperLabel diagnostic
        // The validation rule may be specified as a codeblock or a string
        // Both parameters are optional, if one is not provided the corresponding value is not changed
        IF !IsNil(cb)


            //  UH 04/10/2000
            //  IF IsCodeBlock( cb )
            IF __CanEval( cb )
                cbValidation := cb
            ELSEIF IsString( cb )
                cbValidation := &( "{ | |" + cb + " }" )
            ELSE
                DbError{ SELF, #SetValidation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADCB), cb, "cb" }:Throw()
            ENDIF
        ENDIF
        IF !IsNil(oHL)
            IF oHL IS HyperLabel
                oHLValidation := oHL
            ELSE
                DbError{ SELF, #SetValidation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
            ENDIF
        ENDIF
        RETURN NIL




    /// <include file="System.xml" path="doc/FieldSpec.Status/*" />
    access Status
        // Returns the Status HyperLabel object; NIL if status is OK. Status reflects the
        // most recently made validation ( see METHOD PerformValidations ).
        RETURN oHLStatus


    /// <include file="System.xml" path="doc/FieldSpec.Status/*" />
    ASSIGN Status (oHL)
        IF oHL IS HyperLabel
            oHLStatus := oHL
        ELSE
            DbError{ SELF, #Status, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
        ENDIF


        return


    /// <include file="System.xml" path="doc/FieldSpec.Transform/*" />
    method Transform( uValue )
        // Format the value into a string according to the picture clause
        // should default to windows formats


        LOCAL cResult   AS STRING
        local cTemp     as string
        local lScience  as logic
        local lZero :=false   as logic


        IF cPicture == NULL_STRING
            if lNumeric
                IF wDecimals=0
                    cResult := Transform(uValue,Replicate("9",wLength))
                ELSE
                    cResult := Transform(uValue,Replicate("9",wLength-wDecimals-1)+"."+Replicate("9",wDecimals))
                ENDIF


                IF SubStr3(cResult,1,1) == "*"
                    lScience := SetScience(TRUE)
                    cTemp := AsString(uValue)
                    SetScience(lScience)
                    IF SLen(cTemp)>wLength //if<overall length, trim it.
                        cTemp:=AllTrim(StrTran(cTemp,Chr(0)))
                        IF SLen(cTemp)>wLength
                            IF lZero //Still too long
                                cResult:=StrTran(cResult,"0","*")
                            ENDIF
                        ELSE
                            // PadL() not available yet
                            //            cResult:=PadL(cTemp,wLength)
                            IF SLen(cTemp) <= wLength
                                cResult := Left(cTemp, wLength)
                            ELSE
                                cResult := Space(wLength - SLen(cTemp))+cTemp
                            ENDIF
                        ENDIF
                    ELSE
                        cResult:=cTemp
                    ENDIF
                ENDIF
            elseif uValue==nil .and. wType==string
                cResult := null_string
            ELSE
                cResult := AsString(uValue)
            ENDIF
        ELSE
            cResult := Transform(uValue,cPicture)
        ENDIF


        RETURN cResult




    /// <include file="System.xml" path="doc/FieldSpec.UsualType/*" />
    access UsualType
        // Returns the storage type as a keyword (INT, STRING, etc.)
        RETURN wType


    /// <include file="System.xml" path="doc/FieldSpec.Val/*" />
    method Val( cString )
        // Converts a string to the appropriate data type


        LOCAL xRet  AS USUAL
        LOCAL cType1 AS STRING


        IF !IsString( cString )     // This is a generic converter
            DbError{ SELF, #Val, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADSTRING), cString, "cString" }:Throw()
        ENDIF


        IF lNumeric
            IF SELF:lNullable
                cType1 := "N0"
            ELSE
                cType1 := "N"
            ENDIF
            xRet := Unformat( cString, cPicture, cType1)


        ELSEIF wType = DATE
            IF SELF:lNullable
                cType1 := "D0"
            ELSE
                cType1 := "D"
            ENDIF
            xRet := Unformat( cString, cPicture, cType1)


        ELSEIF wType = LOGIC
            IF SELF:lNullable
                cType1 := "L0"
            ELSE
                cType1 := "L"
            ENDIF
            xRet := Unformat( cString, cPicture, cType1)


        ELSEIF wType = STRING
            IF SELF:lNullable
                cType1 := "C0"
            ELSE
                cType1 := "C"
            ENDIF
            xRet := Unformat( cString, cPicture, cType1)
        ENDIF


        RETURN xRet


    /// <include file="System.xml" path="doc/FieldSpec.Validate/*" />
    method Validate( uValue, arg )


        RETURN cbValidation = NIL .OR. Eval( cbValidation, uValue, arg )


    /// <include file="System.xml" path="doc/FieldSpec.Validation/*" />
    access Validation
        RETURN cbValidation


    /// <include file="System.xml" path="doc/FieldSpec.ValType/*" />
    access ValType
        // Returns the storage type as a keyword (INT, STRING, etc.)
        RETURN cType




        //RvdH 2010-12-03: Some extra accesses
    /// <include file="System.xml" path="doc/FieldSpec.MinLengthHL/*" />
    access MinLengthHL
        RETURN oHLMinLength


    /// <include file="System.xml" path="doc/FieldSpec.RangeHL/*" />
    access RangeHL
        RETURN oHLRange


    /// <include file="System.xml" path="doc/FieldSpec.RequiredHL/*" />
    access RequiredHL
        RETURN oHLRequired


    /// <include file="System.xml" path="doc/FieldSpec.ValidationHL/*" />
    access ValidationHL
        RETURN oHLValidation




END CLASS


/// <include file="System.xml" path="doc/DateFS/*" />
PARTIAL CLASS DateFS INHERIT FieldSpec


    /// <include file="System.xml" path="doc/DateFS.ctor/*" />
    constructor( oHLName )
        IF IsNil(oHLName)
            oHLName := "__DateFS"
        ENDIF
        SUPER(oHLName,"D",8,0)
        return
END CLASS


/// <include file="System.xml" path="doc/IntegerFS/*" />
PARTIAL CLASS IntegerFS INHERIT FieldSpec


    /// <include file="System.xml" path="doc/IntegerFS.ctor/*" />
    constructor( oHLName, uLength )
        IF IsNil(oHLName)
            oHLName := "__IntegerFS"
        ENDIF


        IF IsNil(uLength)
            uLength := 10
        ENDIF


        SUPER(oHLName,"N",uLength,0)
        SELF:Picture := " 9,999"
        return
END CLASS


/// <include file="System.xml" path="doc/LogicFS/*" />
PARTIAL CLASS LogicFS INHERIT FieldSpec


    /// <include file="System.xml" path="doc/LogicFS.ctor/*" />
    constructor(oHLName)


        IF IsNil(oHLName)
            oHLName := "__LogicFS"
        ENDIF
        SUPER(oHLName,"L",1,0)
        return
END CLASS


/// <include file="System.xml" path="doc/MoneyFS/*" />
PARTIAL CLASS MoneyFS INHERIT NumberFS


    /// <include file="System.xml" path="doc/MoneyFS.ctor/*" />
    constructor( oHLName, uLength, uDecimals)
        IF IsNil(oHLName)
            oHLName := "__MoneyFS"
        ENDIF


        SUPER(oHLName,uLength,uDecimals)
        return
END CLASS


/// <include file="System.xml" path="doc/NumberFS/*" />
PARTIAL CLASS NumberFS INHERIT FieldSpec


    /// <include file="System.xml" path="doc/NumberFS.ctor/*" />
    constructor(oHLName, uLength, uDecimals)
        IF IsNil(oHLName)
            oHLName := "__NumberFS"
        ENDIF
        IF IsNil(uLength)
            uLength := 12
        ENDIF
        IF IsNil(uDecimals)
            uDecimals := 2
        ENDIF
        SUPER(oHLName,"N",uLength,uDecimals)


        IF  uDecimals > 0
            SELF:Picture := Replicate("9",uLength-uDecimals-1) + "." +;
                Replicate("9",uDecimals)
        ELSE
            SELF:Picture := Replicate("9",uLength)
        ENDIF
        return


END CLASS


/// <include file="System.xml" path="doc/StringFS/*" />
PARTIAL CLASS StringFS INHERIT FieldSpec


    /// <include file="System.xml" path="doc/StringFS.ctor/*" />
    constructor( oHLName, uLength)
        IF IsNil(oHLName)
            oHLName := "__StringFS"
        ENDIF
        IF IsNil(uLength)
            uLength := 10
        ENDIF
        SUPER(oHLName,"C",uLength,0)
        return


END CLASS


STATIC FUNCTION TypeAsString(wType AS DWORD) AS STRING
    IF wType = STRING
        RETURN "string"
    ELSEIF wType=DATE
        RETURN "date"
    ELSEIF wType=LOGIC
        RETURN "logic"
    ELSEIF wType=FLOAT
        RETURN "number"
    ELSE
        RETURN "integer"
    ENDIF







