
/// <include file="System.xml" path="doc/FieldSpec/*" />
#pragma options ("enforceself", on)
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
        IF SELF:oHLRange == NULL_OBJECT
            IF IsNil(SELF:uMin) .AND. IsNil(SELF:uMax)
                RETURN
            ENDIF
            IF IsNil(SELF:uMin) .AND. !IsNil(SELF:uMax)
                SELF:oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
                    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMAX,SELF:oHyperLabel:Name,AsString( SELF:uMax ) ) }
            ENDIF
            IF !IsNil(SELF:uMin) .AND. IsNil(SELF:uMax)
                SELF:oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
                    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMIN,SELF:oHyperLabel:Name,AsString( SELF:uMin ) ) }
            ENDIF
            IF !IsNil(SELF:uMin) .AND. !IsNil(SELF:uMax)
                SELF:oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
                    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDRANGE,SELF:oHyperLabel:Name,AsString(SELF:uMin),AsString(SELF:uMax )) }
            ENDIF
        ENDIF
        RETURN


    /// <include file="System.xml" path="doc/FieldSpec.AsString/*" />
    method AsString( )
        RETURN SELF:oHyperLabel:Caption


    /// <include file="System.xml" path="doc/FieldSpec.Decimals/*" />
    access Decimals
        // Returns the number of decimals
        RETURN SELF:wDecimals




    /// <include file="System.xml" path="doc/FieldSpec.Decimals/*" />
    assign Decimals (uDecimals)


        IF IsNil(uDecimals )
            SELF:wDecimals := 0
        ELSEIF IsNumeric( uDecimals )
            SELF:wDecimals := uDecimals
        ELSE
            DbError{ SELF, #Decimals, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADDECIMALS), uDecimals, "uDecimals" }:Throw()
        ENDIF
        return


    /// <exclude />
    ACCESS __HyperLabel as HyperLabel
        RETURN SELF:oHyperLabel
    /// <include file="System.xml" path="doc/FieldSpec.HyperLabel/*" />
    access HyperLabel
        // Returns the HyperLabel object
        RETURN SELF:oHyperLabel


    /// <include file="System.xml" path="doc/FieldSpec.ctor/*" />
    constructor( oHLName, uType, uLength, uDecimals )
        // Instantiation parameters for FieldSpec
        // oHLName      ( required ) HyperLabel
        // uType            ( required ) the type, either as one of the data type keywords (STRING, INT, LOGIC, etc.)
        //                      or as a 1-char string ("N","C","D","L","M")
        // uLength          ( required for some data types, not for LOGIC for example )
        // uDecimals        ( optional ) defaults to 0
        if oHLName is HyperLabel
            SELF:oHyperLabel := oHLName
        ELSEIF IsSymbol( oHLName ) .OR. IsString( oHLName )
            SELF:oHyperLabel := HyperLabel{ oHLName }
        ELSE
            DbError{SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADNAME), oHLName, "oHLName" }:Throw()
        ENDIF
        SELF:_SetType(uType, #Init)
        if uType is STRING var strType .and. Left(Upper(strType),1)="I"
            uLength  := 10
            uDecimals := 0
        endif

        IF IsNumeric( uLength )
            SELF:wLength := uLength
        ELSE
            IF SELF:lNumeric .OR. SELF:cType="C"
                //  UH 12/16/1999
                DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADLENGTH), uLength, "uLength" }:Throw()
            ENDIF
        ENDIF


        IF IsNil(uDecimals )
            SELF:wDecimals := 0
        ELSEIF IsNumeric( uDecimals )
            SELF:wDecimals := uDecimals
        ELSE
            DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADDECIMALS), uDecimals, "uDecimals" }:Throw()
        ENDIF
        return




    /// <include file="System.xml" path="doc/FieldSpec.Length/*" />
    ACCESS Length
        // Returns the length of the field
        RETURN SELF:wLength


    /// <include file="System.xml" path="doc/FieldSpec.Maximum/*" />
    ACCESS Maximum
        RETURN SELF:uMax


    /// <include file="System.xml" path="doc/FieldSpec.Minimum/*" />
    ACCESS Minimum
        RETURN SELF:uMin


    /// <include file="System.xml" path="doc/FieldSpec.MinLength/*" />
    ACCESS MinLength
        RETURN SELF:wMinLength


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


        SELF:oHLStatus := NULL_OBJECT


        //  UH 01/31/1997
        IF SELF:lNullable .AND. IsNil(uValue)
            return .t.
        ENDIF


        if (IsString(uValue) .and. Empty(AllTrim(uValue))) .or. ;
                (IsDate(uValue) .and. uValue == null_date)
            // Check required
            IF SELF:lRequired
                IF IsNil(SELF:oHLRequired)
                    SELF:oHLRequired := HyperLabel{ #FieldSpecRequired, , VO_Sprintf(__CAVOSTR_DBFCLASS_REQUIRED,SELF:oHyperLabel:Name) }
                ENDIF


                SELF:oHLStatus := SELF:oHLRequired
                RETURN FALSE
            ENDIF
        ELSE
            //  UH 01/06/2000
            IF IsNil(uValue) .AND. SELF:wType == OBJECT
                return .t.
            ENDIF


            // Check data type (no conversions here!)
            IF !(UsualType(uValue) == SELF:wType .OR. (SELF:lNumeric .AND. IsNumeric(uValue)) .OR. ((SELF:wType == TYPE_MULTIMEDIA) .AND. IsString(uValue)))
                IF SELF:oHLType == NULL_OBJECT
                    SELF:oHLType := HyperLabel{ #FieldSpecType, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDTYPE,SELF:oHyperLabel:Name,TypeAsString(SELF:wType)) }
                ENDIF
                SELF:oHLStatus := SELF:oHLType


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
            ELSEIF SELF:wType == STRING
                cValue := uValue
            ENDIF


            wLen := SLen(cValue)


            IF wLen > SELF:wLength .AND. ;
                    !(SELF:cType == "M" /*.AND. wLength == 10 .AND. !wLength > 65536*/ )


                IF SELF:oHLLength == NULL_OBJECT
                    SELF:oHLLength := HyperLabel{ #FieldSpecLength, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDLENGTH,SELF:oHyperLabel:Name,Str( SELF:wLength ) ) }
                ENDIF


                SELF:oHLStatus := SELF:oHLLength
                RETURN FALSE


            ELSEIF SELF:wType == STRING .AND. wLen < SELF:wMinLength
                IF SELF:oHLMinLength = NULL_OBJECT
                    SELF:oHLMinLength := HyperLabel{ #FieldSpecMinLength, ,  ;
                        VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMINLENGTH,SELF:oHyperLabel:Name,Str( SELF:wMinLength ) ) }
                ENDIF
                SELF:oHLStatus := SELF:oHLMinLength
                RETURN FALSE
            ENDIF


            // Check range
            IF !IsNil(SELF:uMin) .AND. uValue < SELF:uMin
                IF IsNil(SELF:oHLRange)
                    SELF:__GetHLRange( )
                ENDIF
                SELF:oHLStatus := SELF:oHLRange
                RETURN FALSE
            ENDIF


            IF !IsNil(SELF:uMax) .AND. uValue > SELF:uMax
                IF IsNil(SELF:oHLRange)
                    SELF:__GetHLRange( )
                ENDIF
                SELF:oHLStatus := SELF:oHLRange


                RETURN FALSE
            ENDIF
        ENDIF


        // Check validation method or codeblock
        IF !SELF:Validate(uValue, arg)
            IF SELF:oHLStatus == NULL_OBJECT
                IF SELF:oHLValidation == NULL_OBJECT
                    SELF:oHLValidation := HyperLabel{ #FieldSpecValidate, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDVALUE,SELF:oHyperLabel:Name) }
                ENDIF
                SELF:oHLStatus := SELF:oHLValidation      // Fill in status if not done by client code
            ENDIF


            RETURN FALSE
        ENDIF


        RETURN TRUE


    /// <include file="System.xml" path="doc/FieldSpec.Picture/*" />
    ACCESS Picture
        RETURN SELF:cPicture


    /// <include file="System.xml" path="doc/FieldSpec.Picture/*" />
    assign Picture( cNewPicture )
        //ASSERT _DYNCHECKERRORBOX( )
        RETURN SELF:cPicture := cNewPicture


    /// <include file="System.xml" path="doc/FieldSpec.Required/*" />
    ACCESS Required
        RETURN SELF:lRequired


    /// <include file="System.xml" path="doc/FieldSpec.SetLength/*" />
    METHOD SetLength( w, oHL )
        // The length is set through the instantiation parameter, and is not normally changed later
        // This method does allow changing the length and, more usefully,
        // the HyperLabel diagnostic for the length check
        // Both parameters are optional, if one is not provided the corresponding value is not changed
        IF !IsNil( w )
            IF IsNumeric( w )
                SELF:wLength := w
            ELSE
                DbError{ SELF, #SetLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADSI), w, "w" }:Throw()
            ENDIF
        ENDIF
        IF oHL # NIL
            IF oHL IS HyperLabel
                SELF:oHLLength := oHL
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
                SELF:wMinLength := w
            ELSE
                DbError{ SELF, #SetMinLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADSI), w, "w" }:Throw()
            ENDIF
        ENDIF
        IF !IsNil(oHL)
            if oHL is HyperLabel
                SELF:oHLMinLength := oHL
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
            SELF:uMin := uMinimum
        ENDIF
        IF !IsNil(uMaximum)
            SELF:uMax := uMaximum
        ENDIF
        IF !IsNil(oHL)
            if IsObject(oHL) .and. __Usual.ToObject(oHL) is HyperLabel
                SELF:oHLRange := oHL
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
            SELF:lRequired := TRUE
        ELSEIF IsLogic( lReq )
            SELF:lRequired := lReq
        ELSE
            DbError{ SELF, #SetRequired, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADLREQ), lReq, "lReq" }:Throw()
        ENDIF
        IF !IsNil(oHL)
            if oHL is HyperLabel
                SELF:oHLRequired := oHL
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
                    SELF:wType := LOGIC
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
                    SELF:wType := TYPE_MULTIMEDIA

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
                ELSEIF SELF:wType = DATE
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
                SELF:oHLType := oHL
            ELSEIF IsSymbol( oHL ) .OR. IsString( oHL )
                SELF:oHLType := HyperLabel{ oHL }
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
                SELF:cbValidation := cb
            ELSEIF IsString( cb )
                SELF:cbValidation := &( "{ | |" + cb + " }" )
            ELSE
                DbError{ SELF, #SetValidation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADCB), cb, "cb" }:Throw()
            ENDIF
        ENDIF
        IF !IsNil(oHL)
            IF oHL IS HyperLabel
                SELF:oHLValidation := oHL
            ELSE
                DbError{ SELF, #SetValidation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
            ENDIF
        ENDIF
        RETURN NIL




    /// <include file="System.xml" path="doc/FieldSpec.Status/*" />
    access Status
        // Returns the Status HyperLabel object; NIL if status is OK. Status reflects the
        // most recently made validation ( see METHOD PerformValidations ).
        RETURN SELF:oHLStatus


    /// <include file="System.xml" path="doc/FieldSpec.Status/*" />
    ASSIGN Status (oHL)
        IF oHL IS HyperLabel
            SELF:oHLStatus := oHL
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


        IF SELF:cPicture == NULL_STRING
            if SELF:lNumeric
                IF SELF:wDecimals=0
                    cResult := Transform(uValue,Replicate("9",SELF:wLength))
                ELSE
                    cResult := Transform(uValue,Replicate("9",SELF:wLength-SELF:wDecimals-1)+"."+Replicate("9",SELF:wDecimals))
                ENDIF


                IF SubStr3(cResult,1,1) == "*"
                    lScience := SetScience(TRUE)
                    cTemp := AsString(uValue)
                    SetScience(lScience)
                    IF SLen(cTemp)>SELF:wLength //if<overall length, trim it.
                        cTemp:=AllTrim(StrTran(cTemp,Chr(0)))
                        IF SLen(cTemp)>SELF:wLength
                            IF lZero //Still too long
                                cResult:=StrTran(cResult,"0","*")
                            ENDIF
                        ELSE
                            // PadL() not available yet
                            //            cResult:=PadL(cTemp,wLength)
                            IF SLen(cTemp) <= SELF:wLength
                                cResult := Left(cTemp, SELF:wLength)
                            ELSE
                                cResult := Space(SELF:wLength - SLen(cTemp))+cTemp
                            ENDIF
                        ENDIF
                    ELSE
                        cResult:=cTemp
                    ENDIF
                ENDIF
            elseif uValue==nil .and. SELF:wType==string
                cResult := null_string
            ELSE
                cResult := AsString(uValue)
            ENDIF
        ELSE
            cResult := Functions.Transform(uValue,SELF:cPicture)
        ENDIF


        RETURN cResult




    /// <include file="System.xml" path="doc/FieldSpec.UsualType/*" />
    access UsualType
        // Returns the storage type as a keyword (INT, STRING, etc.)
        RETURN SELF:wType


    /// <include file="System.xml" path="doc/FieldSpec.Val/*" />
    method Val( cString )
        // Converts a string to the appropriate data type


        LOCAL xRet  AS USUAL
        LOCAL cType1 AS STRING


        IF !IsString( cString )     // This is a generic converter
            DbError{ SELF, #Val, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADSTRING), cString, "cString" }:Throw()
        ENDIF


        IF SELF:lNumeric
            IF SELF:lNullable
                cType1 := "N0"
            ELSE
                cType1 := "N"
            ENDIF
            xRet := Unformat( cString, SELF:cPicture, cType1)


        ELSEIF SELF:wType = DATE
            IF SELF:lNullable
                cType1 := "D0"
            ELSE
                cType1 := "D"
            ENDIF
            xRet := Unformat( cString, SELF:cPicture, cType1)


        ELSEIF SELF:wType = LOGIC
            IF SELF:lNullable
                cType1 := "L0"
            ELSE
                cType1 := "L"
            ENDIF
            xRet := Unformat( cString, SELF:cPicture, cType1)


        ELSEIF SELF:wType = STRING
            IF SELF:lNullable
                cType1 := "C0"
            ELSE
                cType1 := "C"
            ENDIF
            xRet := Unformat( cString, SELF:cPicture, cType1)
        ENDIF


        RETURN xRet


    /// <include file="System.xml" path="doc/FieldSpec.Validate/*" />
    method Validate( uValue, arg )


        RETURN SELF:cbValidation = NIL .OR. Eval( SELF:cbValidation, uValue, arg )


    /// <include file="System.xml" path="doc/FieldSpec.Validation/*" />
    access Validation
        RETURN SELF:cbValidation


    /// <include file="System.xml" path="doc/FieldSpec.ValType/*" />
    access ValType
        // Returns the storage type as a keyword (INT, STRING, etc.)
        RETURN SELF:cType




        //RvdH 2010-12-03: Some extra accesses
    /// <include file="System.xml" path="doc/FieldSpec.MinLengthHL/*" />
    access MinLengthHL
        RETURN SELF:oHLMinLength


    /// <include file="System.xml" path="doc/FieldSpec.RangeHL/*" />
    access RangeHL
        RETURN SELF:oHLRange


    /// <include file="System.xml" path="doc/FieldSpec.RequiredHL/*" />
    access RequiredHL
        RETURN SELF:oHLRequired


    /// <include file="System.xml" path="doc/FieldSpec.ValidationHL/*" />
    access ValidationHL
        RETURN SELF:oHLValidation




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







