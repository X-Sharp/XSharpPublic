//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options ("enforceself", on)
/// <include file="System.xml" path="doc/FieldSpec/*" />
[XSharp.Internal.TypesChanged];
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
	PROTECT oHyperLabel     AS HyperLabel
	PROTECT oHLStatus 	    AS HyperLabel
	protect wType 			as dword
	PROTECT cType 			AS STRING
	PROTECT lNumeric 		AS LOGIC
	PROTECT oHLType 		AS HyperLabel
	protect wLength 		as dword
	PROTECT oHLLength 	    AS HyperLabel
	protect wDecimals 	    as dword
	PROTECT lRequired 	    AS LOGIC
	PROTECT oHLRequired     AS HyperLabel
	PROTECT wMinLength 	    AS DWORD
	PROTECT oHLMinLength    AS HyperLabel
	PROTECT uMin, uMax      AS USUAL
	PROTECT oHLRange 		AS HyperLabel
	PROTECT cbValidation    AS USUAL // AS CODEBLOCK
	PROTECT oHLValidation   AS HyperLabel
	PROTECT cPicture 		AS STRING
	PROTECT lNullable AS LOGIC


 /// <exclude />
	method __GetHLRange  as void strict
	    IF SELF:oHLRange == NULL_OBJECT
		    IF IsNil(SELF:uMin) .AND. IsNil(SELF:uMax)
			    RETURN
		    ENDIF
		    IF IsNil(SELF:uMin) .AND. !IsNil(SELF:uMax)
			    SELF:oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
				    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMAX,oHyperLabel:Name,AsString( SELF:uMax ) ) }
		    ENDIF
		    IF !IsNil(SELF:uMin) .AND. IsNil(SELF:uMax)
			    SELF:oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
				    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMIN,oHyperLabel:Name,AsString( SELF:uMin ) ) }
		    ENDIF
		    IF !IsNil(SELF:uMin) .AND. !IsNil(SELF:uMax)
			    SELF:oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
				    VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDRANGE,oHyperLabel:Name,AsString(self:uMin),AsString( self:uMax )) }
		    ENDIF
	    ENDIF
	    RETURN


/// <include file="System.xml" path="doc/FieldSpec.AsString/*" />
method AsString( ) as string strict
	RETURN oHyperLabel:Caption


/// <include file="System.xml" path="doc/FieldSpec.Decimals/*" />
access Decimals   as dword
	// Returns the number of decimals
	RETURN SELF:wDecimals




/// <include file="System.xml" path="doc/FieldSpec.Decimals/*" />
assign Decimals (uDecimals as dword )
	SELF:wDecimals := uDecimals
	return


/// <include file="System.xml" path="doc/FieldSpec.HyperLabel/*" />
access HyperLabel as HyperLabel
	// Returns the HyperLabel object
	RETURN oHyperLabel


/// <include file="System.xml" path="doc/FieldSpec.ctor/*" />
constructor( oHLName as string, uType:= nil as usual, uLength := 0 as dword, uDecimals := 0 as dword)
    SELF(HyperLabel{oHLName}, uType, uLength, uDecimals)


/// <include file="System.xml" path="doc/FieldSpec.ctor/*" />
constructor( oHLName as HyperLabel, uType:= nil as usual, uLength := 0 as dword, uDecimals := 0 as dword)
	// Instantiation parameters for FieldSpec
	// oHLName      ( required ) HyperLabel
	// uType            ( required ) the type, either as one of the data type keywords (STRING, INT, LOGIC, etc.)
	//                      or as a 1-char string ("N","C","D","L","M")
	// uLength          ( required for some data types, not for LOGIC for example )
	// uDecimals        ( optional ) defaults to 0
	oHyperLabel := oHLName
    SELF:_SetType(uType, #Init)

	SELF:wLength := uLength
	SELF:wDecimals := uDecimals
	RETURN




/// <include file="System.xml" path="doc/FieldSpec.Length/*" />
ACCESS Length AS DWORD
	// Returns the length of the field
	RETURN SELF:wLength


/// <include file="System.xml" path="doc/FieldSpec.Maximum/*" />
access Maximum   as usual
	RETURN SELF:uMax


/// <include file="System.xml" path="doc/FieldSpec.Minimum/*" />
ACCESS Minimum   AS USUAL
	RETURN SELF:uMin


/// <include file="System.xml" path="doc/FieldSpec.MinLength/*" />
ACCESS MinLength AS DWORD
	RETURN wMinLength


/// <include file="System.xml" path="doc/FieldSpec.Nullable/*" />
access Nullable  as logic
	RETURN SELF:lNullable


/// <include file="System.xml" path="doc/FieldSpec.Nullable/*" />
ASSIGN Nullable( lNew AS LOGIC)
	SELF:lNullable := lNew


/// <include file="System.xml" path="doc/FieldSpec.PerformValidations/*" />
METHOD PerformValidations(uValue AS USUAL)  AS LOGIC
	// Performs all the validations on the specified value: required field, data type compliance, range, etc.
	// Returns a LOGIC indicating whether the validation succeeded.
	// Also sets the STATUS to the appropriate HyperLabel for the validation rule that failed,
	// sets it to NIL if it succeeded
	//
	LOCAL cValue      AS STRING
	LOCAL wLen, wDecLen:=0, i  AS DWORD
	LOCAL cDecSep, cTmp   AS STRING


	SELF:oHLStatus := NULL_OBJECT


	IF SELF:lNullable .AND. IsNil(uValue)
		RETURN .T.
	endif


	IF (IsString(uValue) .AND. Empty(AllTrim(uValue))) .OR. ;
		(IsDate(uValue) .AND. uValue == NULL_DATE)
		// Check required
		IF lRequired
			IF SELF:oHLRequired == NULL_OBJECT
				SELF:oHLRequired := HyperLabel{ #FieldSpecRequired, , VO_Sprintf(__CAVOSTR_DBFCLASS_REQUIRED,oHyperLabel:Name) }
			ENDIF


			SELF:oHLStatus := SELF:oHLRequired
			RETURN FALSE
		endif
	ELSE
		IF IsNil(uValue) .AND. SELF:wType == OBJECT
			RETURN .T.
		ENDIF


		// Check data type (no conversions here!)
		IF !(UsualType(uValue) == wType .OR. (lNumeric .AND. IsNumeric(uValue)) .OR. ((wType == TYPE_MULTIMEDIA) .AND. IsString(uValue)))
			IF SELF:oHLType == NULL_OBJECT
				SELF:oHLType := HyperLabel{ #FieldSpecType, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDTYPE,oHyperLabel:Name,TypeAsString(wType)) }
			ENDIF
			SELF:oHLStatus := SELF:oHLType


			RETURN FALSE
		ENDIF


		// Check max and min length
		IF lNumeric
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
                    i := 0
                    DO WHILE i < wLen
						IF Char.IsDigit( cTmp, (INT) i )
							wDecLen++
						ENDIF
						i++
					ENDDO


					// substring cValue based on control panel's decimal sep.
					cValue := SubStr3(cValue, 1, At2(cDecSep, cValue) + wDecLen)


				ENDIF
			ENDIF
		ELSEIF wType == STRING
			cValue := uValue
		ENDIF


		wLen := SLen(cValue)


		IF wLen > SELF:wLength .AND. !(cType == "M" )


			IF SELF:oHLLength == NULL_OBJECT
				SELF:oHLLength := HyperLabel{ #FieldSpecLength, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDLENGTH,oHyperLabel:Name,Str( SELF:wLength ) ) }
			ENDIF


			SELF:oHLStatus := SELF:oHLLength
			RETURN FALSE


		ELSEIF wType == STRING .AND. wLen < wMinLength
			IF  SELF:oHLMinLength = NULL_OBJECT
				SELF:oHLMinLength := HyperLabel{ #FieldSpecMinLength, ,  ;
					VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMINLENGTH,oHyperLabel:Name,Str( wMinLength ) ) }
			ENDIF
			SELF:oHLStatus := SELF:oHLMinLength
			RETURN FALSE
		ENDIF


		// Check range
		IF !IsNil(SELF:uMin) .AND. uValue < SELF:uMin
			IF SELF:oHLRange == NULL_OBJECT
				SELF:__GetHLRange( )
			ENDIF
			SELF:oHLStatus := SELF:oHLRange
			RETURN FALSE
		ENDIF


		IF !IsNil(SELF:uMax) .AND. uValue > SELF:uMax
			IF SELF:oHLRange == NULL_OBJECT
				SELF:__GetHLRange( )
			ENDIF
			SELF:oHLStatus := SELF:oHLRange


			RETURN FALSE
		ENDIF
	ENDIF


	// Check validation method or codeblock
	IF !SELF:Validate(uValue)
		IF SELF:oHLStatus == NULL_OBJECT
			IF SELF:oHLValidation == NULL_OBJECT
				SELF:oHLValidation := HyperLabel{ #FieldSpecValidate, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDVALUE,oHyperLabel:Name) }
			ENDIF
			SELF:oHLStatus := SELF:oHLValidation      // Fill in status if not done by client code
		ENDIF


		RETURN FALSE
	ENDIF


	RETURN TRUE


/// <include file="System.xml" path="doc/FieldSpec.Picture/*" />
ACCESS Picture  AS STRING
	RETURN cPicture


/// <include file="System.xml" path="doc/FieldSpec.Picture/*" />
assign Picture( cNewPicture as string)
	cPicture := cNewPicture


/// <include file="System.xml" path="doc/FieldSpec.Required/*" />
ACCESS Required  AS LOGIC
	RETURN lRequired


/// <include file="System.xml" path="doc/FieldSpec.SetLength/*" />
METHOD SetLength( w  AS DWORD, oHL := NULL AS HyperLabel )  AS VOID
	// The length is set through the instantiation parameter, and is not normally changed later
	// This method does allow changing the length and, more usefully,
	// the HyperLabel diagnostic for the length check
	// Both parameters are optional, if one is not provided the corresponding value is not changed
    SELF:wLength := w
	if oHL # null_object
		SELF:oHLLength := oHL
	ENDIF
	RETURN


/// <include file="System.xml" path="doc/FieldSpec.SetMinLength/*" />
METHOD SetMinLength( w  AS DWORD, oHL := NULL AS HyperLabel) AS VOID
	// This method is used to set the minimum length,
	// and the HyperLabel diagnostic for the minlength check (applies to string data only)
	// Both parameters are optional, if one is not provided the corresponding value is not changed
	wMinLength := w
	if oHL != null_object
		SELF:oHLMinLength := oHL
	ENDIF
	RETURN


/// <include file="System.xml" path="doc/FieldSpec.SetRange/*" />
METHOD SetRange( uMinimum AS USUAL, uMaximum AS USUAL, oHL := NULL AS HyperLabel ) AS VOID
	// Sets the range and the HyperLabel for the range check error message
	// All parameters are optional, if one is not provided the corresponding value is not changed
	IF !IsNil(uMinimum)
		SELF:uMin := uMinimum
	ENDIF
	IF !IsNil(uMaximum)
		SELF:uMax := uMaximum
	ENDIF
	if oHL != null_object
		SELF:oHLRange := oHL
	ENDIF
	RETURN


/// <include file="System.xml" path="doc/FieldSpec.SetRequired/*" />
METHOD SetRequired( lReq := TRUE AS LOGIC, oHL := NULL AS HyperLabel) AS VOID
	// This method is used to specify whether this is a required field,
	// and the HyperLabel diagnostic for the required check
	// Both parameters are optional; if lReq is omitted TRUE is assumed;
	// if the HyperLabel is not provided the current value is not changed
	lRequired := lReq
	if oHL != null_object
		SELF:oHLRequired := oHL
	ENDIF
	RETURN


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
METHOD SetType( uType AS USUAL, oHL := NULL AS HyperLabel) AS VOID
	// The storage type is normally set as an instantiation parameter and is not changed later.
	// This method does allow the storage type to be changed and, more usefully,
	// the HyperLabel diagnostic for the storage type check
	// Both parameters are optional, if one is not provided the corresponding value is not changed
        SELF:_SetType(uType, #SetType)

	if oHL != null_object
		SELF:oHLType := oHL
	ENDIF
	RETURN




/// <include file="System.xml" path="doc/FieldSpec.SetValidation/*" />
METHOD SetValidation( cb AS USUAL, oHL := NULL_OBJECT AS HyperLabel) AS VOID
	// Used to set the validation codeblock and its corresponding HyperLabel diagnostic
	// The validation rule may be specified as a codeblock or a string
	// Both parameters are optional, if one is not provided the corresponding value is not changed
	IF !IsNil(cb)
		IF __CanEval( cb )
			cbValidation := cb
		ELSEIF IsString( cb )
			cbValidation := &( "{ | |" + cb + " }" )
		ELSE
			DbError{ SELF, #SetValidation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADCB), cb, "cb" }:Throw()
		ENDIF
	ENDIF
	if oHL != null_object
		SELF:oHLValidation := oHL
	ENDIF
	RETURN




/// <include file="System.xml" path="doc/FieldSpec.Status/*" />
ACCESS Status  AS HyperLabel
	// Returns the Status HyperLabel object; NIL if status is OK. Status reflects the
	// most recently made validation ( see METHOD PerformValidations ).
	RETURN SELF:oHLStatus


/// <include file="System.xml" path="doc/FieldSpec.Status/*" />
ASSIGN Status (oHL AS HyperLabel)
    SELF:oHLStatus := oHL
	RETURN


/// <include file="System.xml" path="doc/FieldSpec.Transform/*" />
METHOD Transform( uValue AS USUAL) AS STRING
	// Format the value into a string according to the picture clause
	// should default to windows formats


	local cResult   as string
	LOCAL cTemp     AS STRING
	LOCAL lScience  AS LOGIC
	LOCAL lZero :=FALSE   AS LOGIC


	IF cPicture == NULL_STRING
		IF lNumeric
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
				endif
			ENDIF
		ELSEIF uValue==NIL .AND. wType==STRING
			cResult := NULL_STRING
		ELSE
			cResult := AsString(uValue)
		ENDIF
	ELSE
		cResult := Transform(uValue,cPicture)
	ENDIF


	RETURN cResult




/// <include file="System.xml" path="doc/FieldSpec.UsualType/*" />
ACCESS UsualType  AS DWORD
	// Returns the storage type as a keyword (INT, STRING, etc.)
	RETURN wType


/// <include file="System.xml" path="doc/FieldSpec.Val/*" />
METHOD Val( cString AS STRING) AS USUAL
	// Converts a string to the appropriate data type


	LOCAL xRet  := NIL AS USUAL
	LOCAL cType AS STRING


	IF lNumeric
		IF SELF:lNullable
			cType := "N0"
		ELSE
			cType := "N"
		ENDIF
		xRet := Unformat( cString, cPicture, cType)


	ELSEIF wType = DATE
		IF SELF:lNullable
			cType := "D0"
		ELSE
			cType := "D"
		ENDIF
		xRet := Unformat( cString, cPicture, cType)


	ELSEIF wType = LOGIC
		IF SELF:lNullable
			cType := "L0"
		ELSE
			cType := "L"
		ENDIF
		xRet := Unformat( cString, cPicture, cType)


	ELSEIF wType = STRING
		IF SELF:lNullable
			cType := "C0"
		ELSE
			cType := "C"
		ENDIF
		xRet := Unformat( cString, cPicture, cType)
	ENDIF


	RETURN xRet


/// <include file="System.xml" path="doc/FieldSpec.Validate/*" />
METHOD Validate( uValue AS USUAL, arg := NIL AS USUAL) AS LOGIC


	RETURN cbValidation = NIL .OR. Eval( cbValidation, uValue, arg )


/// <include file="System.xml" path="doc/FieldSpec.Validation/*" />
ACCESS Validation  AS USUAL
	RETURN cbValidation


/// <include file="System.xml" path="doc/FieldSpec.ValType/*" />
ACCESS ValType AS STRING
	// Returns the storage type as a keyword (INT, STRING, etc.)
	RETURN cType




/// <include file="System.xml" path="doc/FieldSpec.MinLengthHL/*" />
ACCESS MinLengthHL AS HyperLabel
	RETURN SELF:oHLMinLength


/// <include file="System.xml" path="doc/FieldSpec.RangeHL/*" />
ACCESS RangeHL AS HyperLabel
	RETURN SELF:oHLRange


/// <include file="System.xml" path="doc/FieldSpec.RequiredHL/*" />
ACCESS RequiredHL AS HyperLabel
	RETURN SELF:oHLRequired


/// <include file="System.xml" path="doc/FieldSpec.ValidationHL/*" />
ACCESS ValidationHL AS HyperLabel
	RETURN SELF:oHLValidation


END CLASS


/// <include file="System.xml" path="doc/DateFS/*" />
PARTIAL CLASS DateFS INHERIT FieldSpec


/// <include file="System.xml" path="doc/DateFS.ctor/*" />
constructor( oHLName := "__DateFS" as string)
    SELF(HyperLabel{oHLName})


/// <include file="System.xml" path="doc/DateFS.ctor/*" />
CONSTRUCTOR( oHLName AS HyperLabel)
	SUPER(oHLName,"D",8,0)
	RETURN
END CLASS


/// <include file="System.xml" path="doc/IntegerFS/*" />
PARTIAL CLASS IntegerFS INHERIT FieldSpec
/// <include file="System.xml" path="doc/IntegerFS.ctor/*" />
CONSTRUCTOR( oHLName := "__MoneyFS" AS STRING, uLength := 10 AS DWORD)
    SELF(HyperLabel{oHLName}, uLength)


/// <include file="System.xml" path="doc/IntegerFS.ctor/*" />
constructor( oHLName as HyperLabel, uLength := 10 as dword)
	SUPER(oHLName,"N",uLength,0)
	SELF:Picture := " 9,999"
	RETURN
END CLASS


/// <include file="System.xml" path="doc/LogicFS/*" />
PARTIAL CLASS LogicFS INHERIT FieldSpec


/// <include file="System.xml" path="doc/LogicFS.ctor/*" />
constructor(oHLName := "__LogicFS" as string)
    SELF(HyperLabel{oHLName})


/// <include file="System.xml" path="doc/LogicFS.ctor/*" />
CONSTRUCTOR(oHLName AS HyperLabel)
	SUPER(oHLName,"L",1,0)
	RETURN
END CLASS


/// <include file="System.xml" path="doc/MoneyFS/*" />
PARTIAL CLASS MoneyFS INHERIT NumberFS


/// <include file="System.xml" path="doc/MoneyFS.ctor/*" />
CONSTRUCTOR( oHLName := "__MoneyFS" AS STRING, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)
	SUPER(oHLName,uLength,uDecimals)




/// <include file="System.xml" path="doc/MoneyFS.ctor/*" />
CONSTRUCTOR( oHLName AS HyperLabel, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)
	SUPER(oHLName,uLength,uDecimals)




END CLASS


/// <include file="System.xml" path="doc/NumberFS/*" />
PARTIAL CLASS NumberFS INHERIT FieldSpec


/// <include file="System.xml" path="doc/NumberFS.ctor/*" />
CONSTRUCTOR(oHLName := "__NumberFS" AS STRING, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)
    SELF( HyperLabel{oHLName}, uLength, uDecimals)


/// <include file="System.xml" path="doc/NumberFS.ctor/*" />
CONSTRUCTOR(oHLName AS HyperLabel, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)
	SUPER(oHLName,"N",uLength,uDecimals)


	IF  uDecimals > 0
		SELF:Picture := Replicate("9",uLength-uDecimals-1) + "." +;
			Replicate("9",uDecimals)
	else
		SELF:Picture := Replicate("9",uLength)
	ENDIF
	RETURN


END CLASS


/// <include file="System.xml" path="doc/StringFS/*" />
PARTIAL CLASS StringFS INHERIT FieldSpec


/// <include file="System.xml" path="doc/StringFS.ctor/*" />
CONSTRUCTOR( oHLName := "__StringFS" AS STRING, uLength := 10 AS DWORD)
	SELF(HyperLabel{oHLName},uLength)
	RETURN


/// <include file="System.xml" path="doc/StringFS.ctor/*" />
CONSTRUCTOR( oHLName AS HyperLabel, uLength := 10 AS DWORD)
	SUPER(oHLName,"C",uLength,0)
	RETURN


END CLASS


INTERNAL FUNCTION TypeAsString(wType AS DWORD) AS STRING
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


