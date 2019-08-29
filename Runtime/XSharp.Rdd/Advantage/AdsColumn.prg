//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.IO
USING System.Collections.Generic
USING System.Linq
USING XSharp.RDD
USING XSharp.RDD.Support
USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.ADS
    /// <summary>Class for DBF Column reading / writing </summary>
    CLASS AdsColumn INHERIT RDDFieldInfo
        INTERNAL RDD        AS XSharp.ADS.AdsRDD
        INTERNAL AdsType    as AdsFieldType
        INTERNAL FieldPos   as DWORD
        PROPERTY _Table as IntPtr GET RDD:_Table

        STATIC METHOD Create(oInfo AS RDDFieldInfo, oRDD AS XSharp.ADS.AdsRDD,type as AdsFieldType, nPos as DWORD) AS AdsColumn
            // Commented out types are Harbour specific
            SWITCH type
            // char types
            CASE AdsFieldType.TIMESTAMP
            CASE AdsFieldType.MODTIME
            CASE AdsFieldType.TIME
            CASE AdsFieldType.MONEY
            CASE AdsFieldType.ROWVERSION
		    CASE AdsFieldType.STRING
		    CASE AdsFieldType.VARCHAR
		    CASE AdsFieldType.CISTRING
		    CASE AdsFieldType.VARCHAR_FOX
		    CASE AdsFieldType.NCHAR
		    CASE AdsFieldType.NVARCHAR
                return AdsCharacterColumn{oInfo, oRDD, type,nPos}

		    CASE AdsFieldType.MEMO
		    CASE AdsFieldType.BINARY
		    CASE AdsFieldType.IMAGE
		    CASE AdsFieldType.NMEMO
		    CASE AdsFieldType.RAW
		    CASE AdsFieldType.VARBINARY_FOX
                return AdsMemoColumn{oInfo, oRDD, type,nPos}

		    CASE AdsFieldType.SHORTINT
		    CASE AdsFieldType.AUTOINC
		    CASE AdsFieldType.INTEGER
		    CASE AdsFieldType.NUMERIC
		    CASE AdsFieldType.DOUBLE
		    CASE AdsFieldType.CURDOUBLE
                return AdsNumericColumn{oInfo, oRDD, type,nPos}

		    CASE AdsFieldType.LOGICAL
                return AdsLogicColumn{oInfo, oRDD, type,nPos}

		    CASE AdsFieldType.DATE
		    CASE AdsFieldType.COMPACTDATE
                return AdsDateColumn{oInfo, oRDD, type,nPos}
            END SWITCH
            RETURN NULL



        PROTECTED CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.ADS.AdsRDD, type as AdsFieldType, nPos as DWORD)
            SUPER(oInfo)
            SELF:RDD        := oRDD
            SELF:AdsType    := type
            SELF:FieldPos   := nPos
            RETURN

        VIRTUAL METHOD GetValue() AS OBJECT
            RETURN NIL

        VIRTUAL METHOD PutValue(oValue AS OBJECT) AS LOGIC
            RETURN FALSE

    END CLASS

    /// <summary>Class for reading / writing String Columns</summary>
    CLASS AdsCharacterColumn INHERIT AdsColumn

         CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.ADS.AdsRDD, type as AdsFieldType,nPos AS DWORD)
            SUPER(oInfo, oRDD,type,nPos)
            IF SELF:Decimals != 0
                SELF:Length         += SELF:Decimals * 256
			    SELF:Decimals       := 0
            ENDIF
            RETURN


       OVERRIDE METHOD PutValue(oValue AS OBJECT) AS LOGIC
	        LOCAL result := 0 AS DWORD
		    LOCAL slength AS DWORD
		    LOCAL strValue AS STRING
    		VAR tc := Type.GetTypeCode(oValue:GetType())
            SWITCH SELF:AdsType
		    CASE AdsFieldType.STRING
		    CASE AdsFieldType.VARCHAR
            CASE AdsFieldType.VARCHAR_FOX
		    CASE AdsFieldType.CISTRING
		    CASE AdsFieldType.NCHAR
		    CASE AdsFieldType.NVARCHAR
			    IF tc != TypeCode.String
				    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
			    ENDIF
			    strValue := (STRING) oValue
			    slength  := (DWORD) strValue:Length
                IF ! SELF:AdsType:IsMemo()
                    slength := Math.Min(slength, SELF:Length)
                ENDIF
			    IF slength == 0
				    result := ACE.AdsSetEmpty(SELF:_Table, SELF:FieldPos)
			    ELSE
                    IF SELF:AdsType:IsUnicode()
                        result := ACE.AdsSetStringW(SELF:_Table, SELF:FieldPos, strValue, slength)
                    ELSE
                        result := ACE.AdsSetString(SELF:_Table, SELF:FieldPos, strValue, slength)
                    ENDIF
			    ENDIF
		    CASE AdsFieldType.TIME
		    CASE AdsFieldType.TIMESTAMP
		    CASE AdsFieldType.MONEY
		    CASE AdsFieldType.ROWVERSION
		    CASE AdsFieldType.MODTIME
			    IF tc != TypeCode.String
				    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
			    ENDIF
			    strValue := (STRING) oValue
			    slength  := (DWORD) strValue:Length
			    result := ACE.AdsSetField(SELF:_Table, SELF:FieldPos, strValue, slength)
		    OTHERWISE
			    SELF:RDD:ADSError(ERDD_DATATYPE, EG_DataType, "PutValue")
		    END SWITCH
            IF result != 0 .AND. result != ACE.AE_DATA_TRUNCATED
			    SELF:RDD:ADSERROR(result, EG_WRITE, "PutValue")
		    ENDIF
            RETURN TRUE


        OVERRIDE METHOD Validate() AS LOGIC
            IF SELF:Length == 0  .OR. SELF:Decimals > 0  .OR. SELF:Length > System.UInt16.MaxValue 
                RETURN FALSE
            ENDIF
            RETURN TRUE

        OVERRIDE METHOD GetValue() AS OBJECT
            LOCAL result    := 0 as DWORD
            LOCAL slength    := SELF:Length+1 AS DWORD
			LOCAL chars := CHAR[] {slength} as CHAR[]
		    SWITCH SELF:AdsType
		    CASE AdsFieldType.TIME
		    CASE AdsFieldType.TIMESTAMP
		    CASE AdsFieldType.MONEY
		    CASE AdsFieldType.ROWVERSION
		    CASE AdsFieldType.MODTIME
                LOCAL isEmpty   := 0 AS WORD
			    RDD:_CheckError(ACE.AdsIsEmpty(SELF:_Table,  SELF:FieldPos , OUT isEmpty),EG_READ)
			    IF isEmpty == 1
				    RETURN NULL
			    ENDIF
			    chars := CHAR[] {slength}
			    result := ACE.AdsGetField(SELF:_Table, SELF:FieldPos, chars, REF slength ,0)
		    CASE AdsFieldType.STRING
		    CASE AdsFieldType.VARCHAR
		    CASE AdsFieldType.CISTRING
		    CASE AdsFieldType.VARCHAR_FOX
		    CASE AdsFieldType.NCHAR
		    CASE AdsFieldType.NVARCHAR
			    chars := CHAR[] {slength}
                IF SELF:AdsType:IsUnicode()
			        result := ACE.AdsGetStringW(SELF:_Table, SELF:FieldPos, chars, REF slength ,0)
                ELSE
			        result := ACE.AdsGetString(SELF:_Table, SELF:FieldPos, chars, REF slength ,0)
                ENDIF
		    OTHERWISE
			    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
		    END SWITCH
		    SWITCH result
		    CASE 0
			    IF SELF:AdsType:IsUnicode()
                    RETURN STRING{chars, 0, (INT) SELF:length}
			    ELSE
				    RETURN SELF:RDD:_Ansi2Unicode(chars, (INT) SELF:length)
			    ENDIF
		    CASE ACE.AE_NO_CURRENT_RECORD
			    RETURN STRING{' ', (INT) SELF:length}
		    OTHERWISE
			    SELF:RDD:_CheckError(result,EG_READ)
		    END SWITCH
            RETURN String.Empty

    END CLASS
    /// <summary>Class for reading / writing Date Columns</summary>
    CLASS AdsDateColumn INHERIT AdsColumn

         CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.ADS.AdsRDD, type as AdsFieldType,nPos AS DWORD)
            SUPER(oInfo, oRDD,type,nPos)
            RETURN

        OVERRIDE METHOD GetValue() AS OBJECT
		    LOCAL lJulian AS LONG
            VAR result := ACE.AdsGetJulian(SELF:_Table, SELF:FieldPos, OUT lJulian)
		    IF result == ACE.AE_NO_CURRENT_RECORD
			    RETURN DbDate{0,0,0}
		    ELSEIF result != 0
			    SELF:RDD:_CheckError(result,EG_READ)
		    ENDIF
		    TRY
			    IF lJulian == 0
				    RETURN DbDate{0,0,0}
			    ELSE
                    // there is a function to convert julian to string but it does not always work correctly
                    // this seems to always do the job
				    LOCAL wLength := ACE.ADS_MAX_DATEMASK+1 AS DWORD
                    LOCAL chars := CHAR[] {SELF:length} as CHAR[]
				    result := ACE.AdsGetString(SELF:_Table, SELF:FieldPos, chars, REF wLength, 0)
				    VAR cDate := STRING{chars, 0, 8}
				    IF String.IsNullOrWhiteSpace(cDate)
					    RETURN DbDate{0,0,0}
				    ELSE
					    VAR dt := DateTime.ParseExact(cDate, "yyyyMMdd",NULL)
					    RETURN DbDate{dt:Year, dt:Month, dt:Day}
				    ENDIF
			    ENDIF
		    CATCH
			
		    END TRY
		    RETURN DbDate{0,0,0}

        OVERRIDE METHOD PutValue(oValue AS OBJECT) AS LOGIC
            // Note that the XSharp.__Date type also returns a typecode of DateTime
            VAR tc := Type.GetTypeCode(oValue:GetType())
	        IF oValue IS IDate VAR oDate
                tc := TypeCode.DateTime
		        IF oDate:IsEmpty
			        oValue := DateTime.MinValue
		        ELSE
			        oValue := DateTime{oDate:Year, oDate:Month, oDate:Day}
		        ENDIF
            ELSEIF oValue IS DateTime
                tc := TypeCode.DateTime
	        ENDIF
		    IF tc != TypeCode.DateTime
			    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Date or DateTime value expected")
		    ENDIF

		    LOCAL dt := (DateTime) oValue AS DateTime
            if (dt == DateTime.MinValue)
                SELF:RDD:_CheckError(ACE.AdsSetEmpty(SELF:_Table, SELF:FieldPos),EG_WRITE)
            ELSE
		        LOCAL text := dt:ToString("yyyyMMdd") AS STRING
		        LOCAL r8Julian AS REAL8
		        SELF:RDD:_CheckError(AceUnPub.AdsConvertStringToJulian(text, (WORD) text:Length, OUT r8Julian),EG_WRITE)
		        SELF:RDD:_CheckError(ACE.AdsSetJulian(SELF:_Table, SELF:FieldPos, (LONG) r8Julian),EG_WRITE)
            ENDIF            
            RETURN TRUE


        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8  .AND.  SELF:Decimals == 0 


    END CLASS

    /// <summary>Class for reading / writing Logic Columns</summary>
    CLASS AdsLogicColumn INHERIT AdsColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.ADS.AdsRDD, type as AdsFieldType,nPos AS DWORD)
            SUPER(oInfo,oRDD,type,nPos)
            RETURN

        OVERRIDE METHOD GetValue() AS OBJECT
	        LOCAL wValue AS WORD
		    var result := ACE.AdsGetLogical(SELF:_Table, SELF:FieldPos, OUT wValue)
		    IF result ==  ACE.AE_NO_CURRENT_RECORD
			    wValue := 0
		    ELSEIF ! SELF:RDD:_CheckError(result,EG_READ)
              // Exception
		    ENDIF
		    RETURN wValue != 0

       OVERRIDE METHOD PutValue(oValue AS OBJECT) AS LOGIC
            VAR tc := Type.GetTypeCode(oValue:GetType())
		    IF tc != TypeCode.Boolean
			    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Logic value expected")
		    ENDIF
		    SELF:RDD:_CheckError(ACE.AdsSetLogical(SELF:_Table, SELF:FieldPos, (WORD)  IIF( (LOGIC) oValue, 1, 0)),EG_WRITE)
            RETURN TRUE

       OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN FALSE

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 1  .AND.  SELF:Decimals == 0 
    END CLASS

    /// <summary>Class for reading / writing Numeric Columns</summary>
    CLASS AdsNumericColumn INHERIT AdsColumn

    CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.ADS.AdsRDD, type as AdsFieldType,nPos AS DWORD)
            SUPER(oInfo,oRDD,type,nPos)
            RETURN

        OVERRIDE METHOD GetValue() AS OBJECT
            LOCAL result    := 0 as DWORD
		    SWITCH SELF:AdsType
		    CASE AdsFieldType.NUMERIC
		    CASE AdsFieldType.DOUBLE
		    CASE AdsFieldType.INTEGER
		    CASE AdsFieldType.SHORTINT
		    CASE AdsFieldType.AUTOINC
		    CASE AdsFieldType.CURDOUBLE
			    LOCAL r8 AS REAL8
			    result := ACE.AdsGetDouble(SELF:_Table, SELF:FieldPos, OUT r8)
			    IF result == ACE.AE_NO_CURRENT_RECORD
				    r8 := 0.0
			    ENDIF
			    IF SELF:AdsType:IsDouble()
				    VAR value := r8:ToString()
				    VAR pos := VALUE:IndexOf('.')
				    RETURN DbFloat{r8, SELF:Length, IIF(pos > 0, SELF:Length - pos -1, 0)}
			    ELSE
				    RETURN DbFloat{r8, SELF:Length, SELF:Decimals}
			    ENDIF
		    OTHERWISE
			    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
		    END SWITCH
            RETURN NULL


      OVERRIDE METHOD PutValue(oValue AS OBJECT) AS LOGIC
        VAR tc := Type.GetTypeCode(oValue:GetType())
      	IF oValue IS IFloat VAR floatValue
		    tc := TypeCode.Double
		    oValue := floatValue:Value
        ENDIF
        LOCAL wType AS WORD
        LOCAL result := 0 AS DWORD
        SELF:RDD:_CheckError(ACE.AdsGetFieldType(SELF:_Table, SELF:FieldPos, OUT wType),EG_READ)
        IF wType != ACE.ADS_AUTOINC
	        TRY
	            LOCAL r8 AS REAL8
		        r8 := Convert.ToDouble(oValue)
		        SELF:RDD:_CheckError(ACE.AdsSetDouble(SELF:_Table, SELF:FieldPos, r8),EG_WRITE)
        
            CATCH
		        SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Numeric value expected")
                RETURN FALSE
	        END TRY
        ELSE
	        NOP // Do not allow to write to AUTO INC field
        ENDIF
        RETURN TRUE

        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbFloat{0.0, SELF:Length, SELF:Decimals}

       OVERRIDE METHOD Validate() AS LOGIC
           IF SELF:Length >= 1  .AND.  SELF:Length <= 255 
                IF SELF:Decimals > 0 
                    // We must check that we have enough space for DOT and decimal
                    IF SELF:Length <= 2  .OR.  SELF:Decimals >= SELF:Length -1 
                        RETURN FALSE
                    ENDIF
                ENDIF
            ELSE
                RETURN FALSE
            ENDIF
            RETURN TRUE


    END CLASS

    /// <summary>Class for reading / writing Memo Columns. This class returns and writes the block numbers.</summary>
    CLASS AdsMemoColumn INHERIT AdsColumn
        // AdsFieldType.MEMO
        // AdsFieldType.NMEMO
        // AdsFieldType.BINARY
        // AdsFieldType.IMAGE
        // AdsFieldType.RAW
        // AdsFieldType.VARBINARY_FOX
        

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.ADS.AdsRDD, type as AdsFieldType,nPos AS DWORD)
            SUPER(oInfo,oRDD,type,nPos)
            RETURN


        OVERRIDE METHOD GetValue() AS OBJECT
            LOCAL isEmpty   := 0 AS WORD
            LOCAL result    := 0 as DWORD
            LOCAL length    := 0 AS DWORD
            LOCAL bytes     := NULL AS BYTE[]
			LOCAL chars := CHAR[] {length} as CHAR[]
            SWITCH SELF:AdsType
		    CASE AdsFieldType.MEMO
		    CASE AdsFieldType.NMEMO
		    CASE AdsFieldType.BINARY
		    CASE AdsFieldType.IMAGE
			    result := ACE.AdsGetMemoLength(SELF:_Table, SELF:FieldPos, OUT length )
			    SWITCH result
			    CASE 0
				    IF length == 0
					    RETURN string.Empty
				    ENDIF
			    CASE ACE.AE_NO_CURRENT_RECORD
				    IF AdsType:IsMemo()
					    RETURN string.Empty
				    ELSE    // image and binary
					    RETURN NULL
				    ENDIF
			    OTHERWISE
				    SELF:RDD:_CheckError(result,EG_READ)
			    END SWITCH
			    SWITCH SELF:AdsType
			    CASE AdsFieldType.MEMO
				    chars := CHAR[] {++length}
				    SELF:RDD:_CheckError(ACE.AdsGetString(SELF:_Table, SELF:FieldPos, chars, REF length ,0),EG_READ)
				    RETURN SELF:RDD:_Ansi2Unicode(chars, (INT) length)
			    CASE AdsFieldType.NMEMO
				    chars := CHAR[] {++length}
				    SELF:RDD:_CheckError(ACE.AdsGetStringW(SELF:_Table, SELF:FieldPos, chars, REF length ,0),EG_READ)
			    CASE AdsFieldType.BINARY
			    CASE AdsFieldType.IMAGE
				    bytes := BYTE[] {length}
				    SELF:RDD:_CheckError(ACE.AdsGetBinary(SELF:_Table, SELF:FieldPos, 0, bytes, REF length ),EG_READ)
				    RETURN bytes
			    END SWITCH
			
		    CASE AdsFieldType.RAW
		    CASE AdsFieldType.VARBINARY_FOX
			    result := ACE.AdsIsEmpty(SELF:_Table, SELF:FieldPos, OUT isEmpty)
			    IF result == 0
				    IF isEmpty == 1
					    RETURN NULL
				    ENDIF
			    ELSE
				    SELF:RDD:_CheckError(result,EG_READ)
			    ENDIF
			    length := SELF:Length
			    bytes := BYTE[] {length}
			    SELF:RDD:_CheckError(ACE.AdsGetBinary(SELF:_Table, SELF:FieldPos, 0, bytes, REF length ),EG_READ)
			    RETURN bytes
		    END SWITCH
            RETURN NULL


        OVERRIDE METHOD PutValue(oValue AS OBJECT) AS LOGIC
	        LOCAL result := 0 AS DWORD
		    LOCAL slength AS DWORD
		    LOCAL strValue AS STRING
    		VAR tc := Type.GetTypeCode(oValue:GetType())
            SWITCH SELF:AdsType
		    CASE AdsFieldType.MEMO
		    CASE AdsFieldType.NMEMO
			    IF tc != TypeCode.String
				    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
			    ENDIF
			    strValue := (STRING) oValue
			    slength  := (DWORD) strValue:Length
                IF ! SELF:AdsType:IsMemo()
                    slength := Math.Min(slength, SELF:Length)
                ENDIF
			    IF slength == 0
				    result := ACE.AdsSetEmpty(SELF:_Table, SELF:FieldPos)
			    ELSE
                    IF SELF:AdsType:IsUnicode()
                        result := ACE.AdsSetStringW(SELF:_Table, SELF:FieldPos, strValue, slength)
                    ELSE
                        result := ACE.AdsSetString(SELF:_Table, SELF:FieldPos, strValue, slength)
                    ENDIF
			    ENDIF
		    CASE AdsFieldType.BINARY
		    CASE AdsFieldType.IMAGE
		    CASE AdsFieldType.RAW
		    CASE AdsFieldType.VARBINARY_FOX
			    IF tc != TypeCode.String .AND. tc != TypeCode.Object
				    SELF:RDD:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String or Object expected")
			    ENDIF
			    IF tc != TypeCode.String
				    strValue := (STRING) oValue
				    slength := (DWORD) strValue:Length
				    result := ACE.AdsSetString(SELF:_Table, SELF:FieldPos, strValue, slength)
			    ELSE
				    LOCAL bytes AS BYTE[]
				    bytes   := (BYTE[]) oValue
				    slength  := (DWORD) bytes:Length
				    IF SELF:AdsType == AdsFieldType.RAW .OR. SELF:AdsType == AdsFieldType.VARBINARY_FOX
					    result := ACE.AdsSetField(SELF:_Table, SELF:FieldPos, bytes, slength)
				    ELSE
					    result := ACE.AdsSetBinary(SELF:_Table, SELF:FieldPos, SELF:AdsType, slength, 0, bytes, slength)
				    ENDIF
			    ENDIF
		    OTHERWISE
			    SELF:RDD:ADSError(ERDD_DATATYPE, EG_DataType, "PutValue")
		    END SWITCH
            IF result != 0 .AND. result != ACE.AE_DATA_TRUNCATED
			    SELF:RDD:ADSERROR(result, EG_WRITE, "PutValue")
		    ENDIF
            RETURN TRUE


        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN NULL

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 10 .AND.  SELF:Decimals == 0 


    END CLASS



     


END NAMESPACE
