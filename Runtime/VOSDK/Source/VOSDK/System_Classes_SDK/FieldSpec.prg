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
	METHOD __GetHLRange  AS VOID STRICT 
	//RvdH-030916 Strong typing
	IF IsNil(oHLRange)
		IF IsNil(uMin) .AND. IsNil(uMax)
			RETURN
		ENDIF
		IF IsNil(uMin) .AND. !IsNil(uMax)
			oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
				/*418@JFS001 changed FS_MSG_INVALIDMAX --> __CavoStr(__CAVOSTR_DBFCLASS_INVALIDMAX) */;
				VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMAX,oHyperLabel:Name,AsString( uMax ) ) }
		ENDIF
		IF !IsNil(uMin) .AND. IsNil(uMax)
			oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
				/* 418@JFS001 changed FS_MSG_INVALIDMIN --> __CavoStr(__CAVOSTR_DBFCLASS_INVALIDMIN)*/;
				VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDMIN,oHyperLabel:Name,AsString( uMin ) ) }
		ENDIF
		IF !IsNil(uMin) .AND. !IsNil(uMax)
			oHLRange := HyperLabel{ #FieldSpecRange, ,  ;
				/* 418@JFS001 changed FS_MSG_INVALIDRANGE1 --> __CavoStr(__CAVOSTR_DBFCLASS_INVALIDRANGE1)*/;
				VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDRANGE,oHyperLabel:Name,AsString(uMin),AsString( uMax )) } 
		ENDIF
	ENDIF
	RETURN

METHOD AsString( )                              
	RETURN oHyperLabel:Caption

ACCESS Decimals                                 
	// Returns the number of decimals
	RETURN wDecimals


ASSIGN Decimals (uDecimals)                     

	IF IsNil(uDecimals )
		wDecimals := 0
	ELSEIF IsNumeric( uDecimals )
		wDecimals := uDecimals
	ELSE
		//  UH 12/16/1999
		//  BREAK DbError{ SELF, #Decimals, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADDECIMALS), uDecimals, "uDecimals" }
		DbError{ SELF, #Decimals, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADDECIMALS), uDecimals, "uDecimals" }:Throw()
	ENDIF
	RETURN 

ACCESS __HyperLabel as HyperLabel
    RETURN oHyperLabel
ACCESS HyperLabel                               
	// Returns the HyperLabel object
	RETURN oHyperLabel

CONSTRUCTOR( oHLName, uType, uLength, uDecimals ) 
	// Instantiation parameters for FieldSpec
	// oHLName      ( required ) HyperLabel
	// uType            ( required ) the type, either as one of the data type keywords (STRING, INT, LOGIC, etc.)
	//                      or as a 1-char string ("N","C","D","L","M")
	// uLength          ( required for some data types, not for LOGIC for example )
	// uDecimals        ( optional ) defaults to 0
	IF IsObject(oHLName) .and. __Usual.ToObject(oHLName) IS HyperLabel 
		oHyperLabel := oHLName
	ELSEIF IsSymbol( oHLName ) .OR. IsString( oHLName )
		oHyperLabel := HyperLabel{ oHLName }
	ELSE
		//  UH 12/16/1999
		//  BREAK DbError{SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADNAME), oHLName, "oHLName" }
		DbError{SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADNAME), oHLName, "oHLName" }:Throw()
	ENDIF

	IF IsString( uType )
		cType := Upper(uType)
		IF cType == "C" .OR. cType == "M"
			wType := STRING
		ELSEIF ctype == "N" .OR. cType == "F"
			wType := FLOAT
			lNumeric:=TRUE
			cType := "N"
		ELSEIF cType == "L"
			wType := LOGIC
		ELSEIF cType == "D"
			wType := DATE

		ELSEIF cType == "O"
			wType := OBJECT
			// Ansgar 7/9/97 added Bitmap
		ELSEIF cType == "X"
			wType := TYPE_MULTIMEDIA
		ELSE
			cType := ""
			DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
		ENDIF
	ELSE
		wType := uType
		IF wType = STRING
			cType := "C"
		ELSEIF wType = LOGIC
			cType := "L"
		ELSEIF wType = DATE
			cType := "D"
		ELSEIF lNumeric:=   wType=INT   .OR.; // also Long 
			wType=FLOAT .OR.;              
			wType=BYTE  .OR.;              
			wType=SHORTINT .OR.;           
			wtype=WORD  .OR.;              
			wType=DWORD .OR.;              
			wType=REAL4 .OR.;              
			wtype=REAL8                    
			cType := "N"                   
		ELSEIF (wType == TYPE_MULTIMEDIA)
			cType := "X"
		ELSE
			wType := 0
			//  UH 12/16/1999
			DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
		ENDIF
	ENDIF

	IF IsNumeric( uLength )
		wLength := uLength
	ELSE
		IF lNumeric .OR. cType="C"
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
	RETURN 


ACCESS Length                                   
	// Returns the length of the field
	RETURN wLength

ACCESS Maximum                                  
	RETURN uMax

ACCESS Minimum                                  
	RETURN uMin

ACCESS MinLength                                
	RETURN wMinLength

ACCESS Nullable                                 
	RETURN SELF:lNullable

ASSIGN Nullable( lNew )                         
	RETURN SELF:lNullable := lNew

METHOD PerformValidations(uValue, arg)        
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
		RETURN .T. 
	ENDIF

	IF (IsString(uValue) .AND. Empty(AllTrim(uValue))) .OR. ;  
		(IsDate(uValue) .AND. uValue == NULL_DATE)         
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
			RETURN .T. 
		ENDIF

		// Check data type (no conversions here!)
		IF !(UsualType(uValue) == wType .OR. (lNumeric .AND. IsNumeric(uValue)) .OR. ((wType == TYPE_MULTIMEDIA) .AND. IsString(uValue)))
			IF oHLType == NULL_OBJECT
				oHLType := HyperLabel{ #FieldSpecType, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDTYPE,oHyperLabel:Name,TypeAsString(wType)) }
			ENDIF
			oHLStatus := oHLType

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
#ifdef __VULCAN__
               i := 0
               DO WHILE i < wLen
						IF Char.IsDigit( cTmp, (INT) i ) 
							wDecLen++
						ENDIF
						i++
					ENDDO	
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
			!(cType == "M" /*.AND. wLength == 10 .AND. !wLength > 65536*/ ) 

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
		IF IsNil(oHLStatus)
			IF IsNil(oHLValidation)
				oHLValidation := HyperLabel{ #FieldSpecValidate, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDVALUE,oHyperLabel:Name) }
			ENDIF
			oHLStatus := oHLValidation      // Fill in status if not done by client code
		ENDIF

		RETURN FALSE
	ENDIF

	RETURN TRUE

ACCESS Picture                                  
	RETURN cPicture

ASSIGN Picture( cNewPicture )                   
	//ASSERT _DYNCHECKERRORBOX( )
	RETURN cPicture := cNewPicture

ACCESS Required                                 
	RETURN lRequired

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
		IF IsObject(oHL) .and. __Usual.ToObject(oHL) IS HyperLabel
			oHLLength := oHL
		ELSE
			DbError{ SELF, #SetLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
		ENDIF
	ENDIF
	RETURN NIL

METHOD SetMinLength( w, oHL )                   
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
		IF IsObject(oHL) .and. __Usual.ToObject(oHL) IS HyperLabel 
			oHLMinLength := oHL
		ELSE
			DbError{ SELF, #SetMinLength, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
		ENDIF
	ENDIF
	RETURN NIL

METHOD SetRange( uMinimum, uMaximum, oHL )      
	// Sets the range and the HyperLabel for the range check error message
	// All parameters are optional, if one is not provided the corresponding value is not changed
	IF !IsNil(uMinimum)
		uMin := uMinimum
	ENDIF
	IF !IsNil(uMaximum)
		uMax := uMaximum
	ENDIF
	IF !IsNil(oHL)
		IF IsObject(oHL) .and. __Usual.ToObject(oHL) IS HyperLabel 
			oHLRange := oHL
		ELSE
			DbError{ SELF, #SetRange, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
		ENDIF
	ENDIF
	RETURN NIL

METHOD SetRequired( lReq, oHL )                 
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
		IF IsObject(oHL) .and. __Usual.ToObject(oHL) IS HyperLabel 
			oHLRequired := oHL
		ELSE
			DbError{ SELF, #SetRequired, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
		ENDIF
	ENDIF
	RETURN NIL

METHOD SetType( uType, oHL )                    
	// The storage type is normally set as an instantiation parameter and is not changed later.
	// This method does allow the storage type to be changed and, more usefully,
	// the HyperLabel diagnostic for the storage type check
	// Both parameters are optional, if one is not provided the corresponding value is not changed
	IF !IsNil(uType)
		IF IsString( uType )
			cType := Upper(uType)
			IF cType == "C" .OR. cType == "M"
				wType := STRING
			ELSEIF cType == "N" .OR. cType == "F"
				wType := FLOAT
				lNumeric:=TRUE
				cType := "N"
			ELSEIF cType == "L"
				wType := LOGIC
			ELSEIF cType == "D"
				wType := DATE

				// SABR01 12/28/95
				// O is an OLE object
			ELSEIF cType == "O"
				wType := OBJECT

				// Ansgar 7/9/97 added Bitmap
			ELSEIF cType == "B"
				wType := TYPE_MULTIMEDIA

			ELSE
				cType := ""
				DbError{ SELF, #SetType, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
			ENDIF
		ELSE
			wType := uType
			IF wType = STRING
				cType := "C"
			ELSEIF wType = LOGIC
				cType := "L"
			ELSEIF wType = DATE
				cType := "D"
			ELSEIF lNumeric:=   wType=INT   .OR.; // also Long
				wType=FLOAT .OR.;
				wType=BYTE  .OR.;
				wType=SHORTINT .OR.;
				wtype=WORD  .OR.;
				wType=DWORD .OR.;
				wType=REAL4 .OR.;
				wtype=REAL8
				cType := "N"
			ELSEIF (wType == TYPE_MULTIMEDIA)
				cType := "B"

			ELSE
				wType := 0
				DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
			ENDIF
		ENDIF
	ENDIF
	IF !IsNil(oHL)
		IF IsObject(oHL) .and. __Usual.ToObject(oHL) IS HyperLabel
			oHLType := oHL
		ELSE
			DbError{ SELF, #SetType, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
		ENDIF
	ENDIF
	RETURN NIL


METHOD SetValidation( cb, oHL )                 
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
		IF IsObject(oHL) .and. __Usual.ToObject(oHL) IS HyperLabel
			oHLValidation := oHL
		ELSE
			DbError{ SELF, #SetValidation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
		ENDIF
	ENDIF
	RETURN NIL


ACCESS Status                                   
	// Returns the Status HyperLabel object; NIL if status is OK. Status reflects the
	// most recently made validation ( see METHOD PerformValidations ).
	RETURN oHLStatus

ASSIGN Status (oHL)
    IF IsObject(oHL) .and. __Usual.ToObject(oHL) IS HyperLabel
        oHLStatus := oHL
    ELSE
		DbError{ SELF, #Status, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADHL), oHL, "oHL" }:Throw()
	ENDIF
	
	RETURN 

METHOD Transform( uValue )                      
	// Format the value into a string according to the picture clause
	// should default to windows formats

	LOCAL cResult   AS STRING
	LOCAL cTemp     AS STRING   
	LOCAL lScience  AS LOGIC    
	LOCAL lZero :=FALSE   AS LOGIC    

	IF cPicture == NULL_STRING
		IF lNumeric
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
		ELSEIF uValue==NIL .AND. wType==STRING 
			cResult := NULL_STRING             
		ELSE
			cResult := AsString(uValue)
		ENDIF
	ELSE
		cResult := Transform(uValue,cPicture)
	ENDIF

	RETURN cResult


ACCESS UsualType                                
	// Returns the storage type as a keyword (INT, STRING, etc.)
	RETURN wType

METHOD Val( cString )                           
	// Converts a string to the appropriate data type

	LOCAL xRet  AS USUAL
	LOCAL cType AS STRING

	IF !IsString( cString )     // This is a generic converter
		DbError{ SELF, #Val, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADSTRING), cString, "cString" }:Throw()
	ENDIF

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

METHOD Validate( uValue, arg )                       

	RETURN cbValidation = NIL .OR. Eval( cbValidation, uValue, arg )

ACCESS Validation                               
	RETURN cbValidation

ACCESS ValType                                  
	// Returns the storage type as a keyword (INT, STRING, etc.)
	RETURN cType


//RvdH 2010-12-03: Some extra accesses
ACCESS MinLengthHL 
	RETURN oHLMinLength

ACCESS RangeHL 
	RETURN oHLRange

ACCESS RequiredHL 
	RETURN oHLRequired

ACCESS ValidationHL 
	RETURN oHLValidation


END CLASS

PARTIAL CLASS DateFS INHERIT FieldSpec

CONSTRUCTOR( oHLName )                      
	IF IsNil(oHLName)
		oHLName := "__DateFS"
	ENDIF
	SUPER(oHLName,"D",8,0)
	RETURN 
END CLASS

PARTIAL CLASS IntegerFS INHERIT FieldSpec

CONSTRUCTOR( oHLName, uLength )             
	IF IsNil(oHLName)
		oHLName := "__IntegerFS"
	ENDIF

	IF IsNil(uLength)
		uLength := 10
	ENDIF

	SUPER(oHLName,"N",uLength,0)
	SELF:Picture := " 9,999"
	RETURN 
END CLASS

PARTIAL CLASS LogicFS INHERIT FieldSpec

CONSTRUCTOR(oHLName)   //457@AJP001

	IF IsNil(oHLName)
		oHLName := "__LogicFS"
	ENDIF
	SUPER(oHLName,"L",1,0)
	RETURN 
END CLASS

PARTIAL CLASS MoneyFS INHERIT NumberFS

CONSTRUCTOR( oHLName, uLength, uDecimals)   
	IF IsNil(oHLName)
		oHLName := "__MoneyFS"
	ENDIF

	SUPER(oHLName,uLength,uDecimals)
	RETURN 
END CLASS

PARTIAL CLASS NumberFS INHERIT FieldSpec

CONSTRUCTOR(oHLName, uLength, uDecimals)    
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
	RETURN 

END CLASS

PARTIAL CLASS StringFS INHERIT FieldSpec

CONSTRUCTOR( oHLName, uLength)              
	IF IsNil(oHLName)
		oHLName := "__StringFS"
	ENDIF
	IF IsNil(uLength)
		uLength := 10
	ENDIF
	SUPER(oHLName,"C",uLength,0)
	RETURN 

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



