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
	PROTECT wType 			AS DWORD        
	PROTECT cType 			AS STRING
	PROTECT lNumeric 		AS LOGIC
	PROTECT oHLType 		AS HyperLabel
	PROTECT wLength 		AS DWORD		
	PROTECT oHLLength 	AS HyperLabel
	PROTECT wDecimals 	AS DWORD         
	PROTECT lRequired 	AS LOGIC
	PROTECT oHLRequired AS HyperLabel
	PROTECT wMinLength 	AS DWORD
	PROTECT oHLMinLength AS HyperLabel
	PROTECT uMin, uMax  AS USUAL
	PROTECT oHLRange 		AS HyperLabel
	PROTECT cbValidation AS USUAL // AS CODEBLOCK
	PROTECT oHLValidation AS HyperLabel
	PROTECT cPicture 		AS STRING
	PROTECT lNullable AS LOGIC

	METHOD __GetHLRange  AS VOID STRICT 
	IF IsNil(oHLRange)
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

METHOD AsString( ) AS STRING STRICT                             
	RETURN oHyperLabel:Caption

ACCESS Decimals   AS DWORD                              
	// Returns the number of decimals
	RETURN wDecimals


ASSIGN Decimals (uDecimals AS DWORD )                    

	wDecimals := uDecimals
	RETURN 

ACCESS HyperLabel AS HyperLabel                            
	// Returns the HyperLabel object
	RETURN oHyperLabel

CONSTRUCTOR( oHLName AS STRING, uType:= NIL AS USUAL, uLength := 0 AS DWORD, uDecimals := 0 AS DWORD) 
    SELF(HyperLabel{oHLName}, uType, uLength, uDecimals)
    
CONSTRUCTOR( oHLName AS HyperLabel, uType:= NIL AS USUAL, uLength := 0 AS DWORD, uDecimals := 0 AS DWORD) 
	// Instantiation parameters for FieldSpec
	// oHLName      ( required ) HyperLabel
	// uType            ( required ) the type, either as one of the data type keywords (STRING, INT, LOGIC, etc.)
	//                      or as a 1-char string ("N","C","D","L","M")
	// uLength          ( required for some data types, not for LOGIC for example )
	// uDecimals        ( optional ) defaults to 0
	oHyperLabel := oHLName

	IF IsString( uType )
		cType := Upper(uType)
        SWITCH cType
		CASE "C"
        CASE "M"
			wType := __UsualType.String
		CASE "N"
        CASE "F"
			wType := __UsualType.Float
			lNumeric:=TRUE
			cType := "N"
		CASE "L"
			wType := __UsualType.Logic
		CASE "D"
			wType := __UsualType.Date
		CASE "O"
			wType := __UsualType.Object
		CASE "X"
			wType := TYPE_MULTIMEDIA
		CASE "Y"
			wType := __UsualType.Currency
		CASE "I"
			wType := __UsualType.Long
		CASE "T"
			wType := __UsualType.DateTime
		OTHERWISE
			cType := ""
			DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
		END SWITCH
	ELSE
		wType := uType
        SWITCH wType
        CASE __UsualType.String
			cType := "C"
		CASE __UsualType.Logic
			cType := "L"
		CASE __UsualType.Date
			cType := "D"
        CASE __UsualType.Long
		CASE __UsualType.Float
		CASE __UsualType.Byte
		CASE __UsualType.ShortInt
		CASE __UsualType.Word
		CASE __UsualType.DWord
		CASE __UsualType.Real4
		CASE __UsualType.Real8        
			cType := "N"
            lNumeric := TRUE
		CASE __UsualType.Currency
			cType := "Y"
            lNumeric := TRUE
        CASE __UsualType.DateTime
            cType := "T"
		CASE TYPE_MULTIMEDIA
			cType := "X"
		OTHERWISE
			wType := 0
			//  UH 12/16/1999
			DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
		END SWITCH
	ENDIF

	wLength := uLength
	wDecimals := uDecimals
	RETURN 


ACCESS Length AS DWORD                                  
	// Returns the length of the field
	RETURN wLength

ACCESS Maximum   AS USUAL                               
	RETURN uMax

ACCESS Minimum   AS USUAL                               
	RETURN uMin

ACCESS MinLength AS DWORD                               
	RETURN wMinLength

ACCESS Nullable  AS LOGIC
	RETURN SELF:lNullable

ASSIGN Nullable( lNew AS LOGIC)                         
	SELF:lNullable := lNew

METHOD PerformValidations(uValue AS USUAL)  AS LOGIC
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

		IF wLen > wLength .AND. !(cType == "M" ) 

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
	IF !SELF:Validate(uValue)
		IF IsNil(oHLStatus)
			IF IsNil(oHLValidation)
				oHLValidation := HyperLabel{ #FieldSpecValidate, , VO_Sprintf(__CAVOSTR_DBFCLASS_INVALIDVALUE,oHyperLabel:Name) }
			ENDIF
			oHLStatus := oHLValidation      // Fill in status if not done by client code
		ENDIF

		RETURN FALSE
	ENDIF

	RETURN TRUE

ACCESS Picture  AS STRING                                
	RETURN cPicture

ASSIGN Picture( cNewPicture AS STRING)                   
	//ASSERT _DYNCHECKERRORBOX( )
	cPicture := cNewPicture

ACCESS Required  AS LOGIC                               
	RETURN lRequired

METHOD SetLength( w  AS DWORD, oHL := NULL AS HyperLabel )  AS VOID                      
	// The length is set through the instantiation parameter, and is not normally changed later
	// This method does allow changing the length and, more usefully,
	// the HyperLabel diagnostic for the length check
	// Both parameters are optional, if one is not provided the corresponding value is not changed
    wLength := w
	IF oHL # NULL_OBJECT
		oHLLength := oHL
	ENDIF
	RETURN 

METHOD SetMinLength( w  AS DWORD, oHL := NULL AS HyperLabel) AS VOID                  
	// This method is used to set the minimum length,
	// and the HyperLabel diagnostic for the minlength check (applies to string data only)
	// Both parameters are optional, if one is not provided the corresponding value is not changed
	wMinLength := w
	IF oHL != NULL_OBJECT
		oHLMinLength := oHL
	ENDIF
	RETURN 

METHOD SetRange( uMinimum AS USUAL, uMaximum AS USUAL, oHL := NULL AS HyperLabel ) AS VOID   
	// Sets the range and the HyperLabel for the range check error message
	// All parameters are optional, if one is not provided the corresponding value is not changed
	IF !IsNil(uMinimum)
		uMin := uMinimum
	ENDIF
	IF !IsNil(uMaximum)
		uMax := uMaximum
	ENDIF
	IF oHL != NULL_OBJECT
		oHLRange := oHL
	ENDIF
	RETURN 

METHOD SetRequired( lReq := TRUE AS LOGIC, oHL := NULL AS HyperLabel) AS VOID                   
	// This method is used to specify whether this is a required field,
	// and the HyperLabel diagnostic for the required check
	// Both parameters are optional; if lReq is omitted TRUE is assumed;
	// if the HyperLabel is not provided the current value is not changed
	lRequired := lReq
	IF oHL != NULL_OBJECT
		oHLRequired := oHL
	ENDIF
	RETURN 

METHOD SetType( uType AS USUAL, oHL := NULL AS HyperLabel) AS VOID                  
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
				wType=WORD  .OR.;
				wType=DWORD .OR.;
				wType=REAL4 .OR.;
				wType=REAL8
				cType := "N"
			ELSEIF (wType == TYPE_MULTIMEDIA)
				cType := "B"

			ELSE
				wType := 0
				DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADTYPE), uType, "uType" }:Throw()
			ENDIF
		ENDIF
	ENDIF
	IF oHL != NULL_OBJECT
		oHLType := oHL
	ENDIF
	RETURN 


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
	IF oHL != NULL_OBJECT
		oHLValidation := oHL
	ENDIF
	RETURN 


ACCESS Status  AS HyperLabel                                 
	// Returns the Status HyperLabel object; NIL if status is OK. Status reflects the
	// most recently made validation ( see METHOD PerformValidations ).
	RETURN oHLStatus

ASSIGN Status (oHL AS HyperLabel)
    oHLStatus := oHL
	RETURN 

METHOD Transform( uValue AS USUAL) AS STRING                     
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


ACCESS UsualType  AS DWORD                              
	// Returns the storage type as a keyword (INT, STRING, etc.)
	RETURN wType

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

METHOD Validate( uValue AS USUAL, arg := NIL AS USUAL) AS LOGIC                       

	RETURN cbValidation = NIL .OR. Eval( cbValidation, uValue, arg )

ACCESS Validation  AS USUAL                             
	RETURN cbValidation

ACCESS ValType AS STRING                                 
	// Returns the storage type as a keyword (INT, STRING, etc.)
	RETURN cType


ACCESS MinLengthHL AS HyperLabel
	RETURN oHLMinLength

ACCESS RangeHL AS HyperLabel
	RETURN oHLRange

ACCESS RequiredHL AS HyperLabel
	RETURN oHLRequired

ACCESS ValidationHL AS HyperLabel
	RETURN oHLValidation

END CLASS

PARTIAL CLASS DateFS INHERIT FieldSpec

CONSTRUCTOR( oHLName := "__DateFS" AS STRING)                      
    SELF(HyperLabel{oHLName})
    
CONSTRUCTOR( oHLName AS HyperLabel)                      
	SUPER(oHLName,"D",8,0)
	RETURN 
END CLASS

PARTIAL CLASS IntegerFS INHERIT FieldSpec
CONSTRUCTOR( oHLName := "__MoneyFS" AS STRING, uLength := 10 AS DWORD)   
    SELF(HyperLabel{oHLName}, uLength)

CONSTRUCTOR( oHLName AS HyperLabel, uLength := 10 AS DWORD)   
	SUPER(oHLName,"N",uLength,0)
	SELF:Picture := " 9,999"
	RETURN 
END CLASS

PARTIAL CLASS LogicFS INHERIT FieldSpec

CONSTRUCTOR(oHLName := "__LogicFS" AS STRING)   
    SELF(HyperLabel{oHLName})
    
CONSTRUCTOR(oHLName AS HyperLabel)   
	SUPER(oHLName,"L",1,0)
	RETURN 
END CLASS

PARTIAL CLASS MoneyFS INHERIT NumberFS

CONSTRUCTOR( oHLName := "__MoneyFS" AS STRING, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)   
	SUPER(oHLName,uLength,uDecimals)


CONSTRUCTOR( oHLName AS HyperLabel, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)   
	SUPER(oHLName,uLength,uDecimals)


END CLASS

PARTIAL CLASS NumberFS INHERIT FieldSpec

CONSTRUCTOR(oHLName := "__NumberFS" AS STRING, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)    
    SELF( HyperLabel{oHLName}, uLength, uDecimals)

CONSTRUCTOR(oHLName AS HyperLabel, uLength := 12 AS DWORD, uDecimals := 2 AS DWORD)    
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

CONSTRUCTOR( oHLName := "__StringFS" AS STRING, uLength := 10 AS DWORD)              
	SELF(HyperLabel{oHLName},uLength)
	RETURN 

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



