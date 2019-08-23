//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.IO
USING XSharp.RDD.Enums
USING System.Collections.Generic
USING System.Linq
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
BEGIN NAMESPACE XSharp.RDD
    /// <summary>Class for DBF Column reading / writing </summary>
    CLASS DbfColumn INHERIT RDDFieldInfo
        INTERNAL NullBit        := -1 AS LONG
        INTERNAL LengthBit      := -1 AS LONG
        INTERNAL OffSetInHeader := -1 as LONG
        INTERNAL RDD        AS XSharp.RDD.DBF

        STATIC METHOD Create(oInfo AS RDDFieldInfo, oRDD AS XSharp.RDD.DBF) AS DbfColumn
            // Commented out types are Harbour specific
            SWITCH oInfo:FieldType
            CASE DBFieldType.Character   // C
            CASE DbFieldType.Varchar     // 'V'
            CASE DbFieldType.VarBinary   // 'Q'
                RETURN DbfCharacterColumn{oInfo,oRDD}
            CASE DBFieldType.Date        // D
                RETURN DbfDateColumn{oInfo,oRDD}
            CASE DBFieldType.Number      // N
            CASE DbFieldType.Float          // 'F'
                RETURN DbfNumericColumn{oInfo,oRDD}
            CASE DBFieldType.Memo        // M
            CASE DbFieldType.Picture        // 'P'
            CASE DbFieldType.General        // 'G'
            CASE DbFieldType.Blob           // 'W'
                RETURN DbfMemoColumn{oInfo,oRDD}
            CASE DBFieldType.Logic       // L
                RETURN DbfLogicColumn{oInfo,oRDD}
            CASE DBFieldType.Integer    // 'I' 
            //CASE DBFieldType.Integer2   // '2' 
            //CASE DBFieldType.Integer4   // '4'
                IF oInfo:IsAutoIncrement
                    RETURN DbfAutoIncrementColumn{oInfo,oRDD}
                ENDIF
                RETURN DbfIntegerColumn{oInfo,oRDD}
            //CASE DbFieldType.AutoIncrement
            //   RETURN DbfAutoIncrementColumn{oInfo}
            CASE DbFieldType.Double         // 'B'
            //CASE DbFieldType.Double8        // '8'
                RETURN DBFDoubleColumn{oInfo,oRDD}
            CASE DbFieldType.Currency		// 'Y'
            //CASE DbFieldType.CurrencyDouble	// 'Z'
                RETURN DbfCurrencyColumn{oInfo,oRDD}
            //CASE DbFieldType.TimeStamp      // '@'
            //CASE DbFieldType.ModTime        // '='
            //CASE DbFieldType.RowVer         // '^'
            //    RETURN DbfTimeStampColumn{oInfo}
            CASE DbFieldType.DateTime      // 'T'
                RETURN DbfDateTimeColumn{oInfo,oRDD}
            CASE DbFieldType.NullFlags
                RETURN DbfNullColumn{oInfo,oRDD}
            CASE DbFieldType.VOObject       // 'O'
            CASE DbFieldType.Unknown        // 'O'
            OTHERWISE
                RETURN DbfColumn{oInfo,oRDD}
            END SWITCH


        STATIC METHOD Create(oField REF DbfField, oRDD AS XSharp.RDD.DBF, nHeaderOffSet as LONG) AS DbfColumn
            LOCAL oInfo AS RDDFieldInfo
            LOCAL oColumn as DbfColumn
            oInfo        := RddFieldInfo{oField:Name, oField:Type, oField:Len, oField:Dec, oField:Offset, oField:Flags}
            oColumn      := DbfColumn.Create(oInfo, oRDD)
            oColumn:OffSetInHeader := nHeaderOffSet
            IF oColumn is DbfAutoIncrementColumn VAR dbfac
                dbfac:IncrStep  := oField:IncStep
                dbfac:Counter   := oField:Counter
            ENDIF
            return oColumn



        PROTECTED CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo)
            SELF:RDD        := oRDD
            RETURN


       VIRTUAL METHOD InitValue(buffer AS BYTE[]) AS VOID
            // this gets called to update the initialize the buffer for 'append blank'. Gets called once when a DBF is opened.
            RETURN

       VIRTUAL METHOD NewRecord(buffer AS BYTE[]) AS VOID
            // this gets called to update the appended record. Only used at this moment to set the AutoIncrement value
            // but can also be used to set a timestamp or rowversion later
            RETURN

        PROTECTED METHOD _GetString(buffer AS BYTE[]) AS STRING
            // The default implementation returns the part of the buffer as a string
            RETURN SELF:RDD:_Encoding:GetString(buffer, SELF:OffSet, SELF:Length)

        VIRTUAL METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            RETURN SELF:_GetString(buffer)

        VIRTUAL METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            RETURN FALSE


        METHOD IsNull() AS LOGIC
            IF SELF:IsNullable .AND. SELF:RDD:_NullColumn != NULL
                RETURN SELF:RDD:_NullColumn:GetBit(SELF:NullBit)
            ENDIF
            RETURN FALSE

        PROTECTED METHOD SetNullValue() AS LOGIC 
            IF SELF:IsNullable .AND. SELF:RDD:_NullColumn != NULL
                SELF:RDD:_NullColumn:SetBit(SELF:NullBit, TRUE)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        PROTECTED METHOD ClearNullValue() AS VOID
            IF SELF:IsNullable .AND. SELF:RDD:_NullColumn != NULL
                SELF:RDD:_NullColumn:SetBit(SELF:NullBit, FALSE)
            ENDIF
            RETURN 

        VIRTUAL METHOD EmptyValue() AS OBJECT
            RETURN String.Empty


        PROTECTED METHOD GetNumber<T>(oValue AS OBJECT, oResult OUT T) AS LOGIC WHERE T IS NEW()
            LOCAL tc AS TypeCode
            IF oValue != NULL
                tc := Type.GetTypeCode(oValue:GetType())
                SWITCH tc
                CASE TypeCode.SByte
                CASE TypeCode.Int16
                CASE TypeCode.Int32
                CASE TypeCode.Int64
                CASE TypeCode.Byte
                CASE TypeCode.UInt16
                CASE TypeCode.UInt32
                CASE TypeCode.UInt64
                CASE TypeCode.Decimal
                CASE TypeCode.Double
                CASE TypeCode.Single
                    oResult := (T) Convert.ChangeType(oValue, typeof(T) )
                    RETURN TRUE
                CASE TypeCode.Object
                    oResult := (T) Convert.ChangeType((((IFloat) oValue):Value), typeof(T) )
                    RETURN TRUE
                END SWITCH
            ENDIF
            oResult :=  T{}
            RETURN FALSE

        PROTECTED METHOD _PutString(buffer AS BYTE[], strValue AS STRING) AS VOID
            SELF:RDD:_Encoding:GetBytes(strValue, 0, SELF:Length, buffer, SELF:Offset)

    END CLASS

    /// <summary>Class for reading / writing String Columns</summary>
    CLASS DbfCharacterColumn INHERIT DbfColumn

         CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo, oRDD)
            IF SELF:Decimals != 0
                SELF:Length         += SELF:Decimals * 256
			    SELF:Decimals       := 0
            ENDIF
            RETURN

       OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            IF SELF:IsVarLength
                buffer[SELF:OffSet + SELF:Length-1] := 0
            ENDIF
            RETURN

       OVERRIDE METHOD EmptyValue() AS OBJECT
           RETURN STRING{ ' ', SELF:length }

       OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL str AS STRING
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS CHAR
                str := ((CHAR) oValue):ToString()
            ELSEIF oValue IS STRING
                str := (STRING) oValue
            ELSE
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            IF str:Length < SELF:length .AND. ! SELF:IsVarLength
                str := str:PadRight(SELF:length,' ')
            ENDIF
            IF SELF:IsBinary
                LOCAL nLen := Math.Min(SELF:Length, str:Length) AS LONG
                System.Text.Encoding.Default:GetBytes(str, 0, nLen, buffer, SELF:Offset)
            ELSE
                SELF:_PutString(buffer, str)
            ENDIF
            IF SELF:IsVarLength
                IF SELF:RDD:_NullColumn != NULL
                    IF str:Length >= SELF:Length
                        // we used all the bytes, so set the varlength bit to 0
                        SELF:RDD:_NullColumn:SetBit(SELF:LengthBit, FALSE)
                    ELSE
                        SELF:RDD:_NullColumn:SetBit(SELF:LengthBit, TRUE)
                        buffer[SELF:OffSet + SELF:Length-1] := (BYTE) str:Length
                    ENDIF
                ELSE
                    buffer[SELF:OffSet + SELF:Length-1] := (BYTE) Math.Max(str:Length, SELF:Length-1)
                ENDIF
            ENDIF
            RETURN TRUE


        OVERRIDE METHOD Validate() AS LOGIC
            IF SELF:Length == 0  .OR. SELF:Decimals > 0  .OR. SELF:Length > System.UInt16.MaxValue 
                RETURN FALSE
            ENDIF
            RETURN TRUE

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS STRING
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            IF SELF:IsBinary
                result := System.Text.Encoding.Default:GetString(buffer, SELF:OffSet, SELF:Length)
            ELSE
                result := SUPER:_GetString(buffer)
            ENDIF
            IF IsVarLength
                LOCAL len AS LONG
                len := SELF:Length
                IF SELF:RDD:_NullColumn != NULL
                    LOCAL lSet AS LOGIC
                    lSet := SELF:RDD:_NullColumn:GetBit(SELF:LengthBit)
                    IF lSet
                        len := buffer[SELF:OffSet + SELF:Length-1]
                    ENDIF
                ENDIF
                IF !String.IsNullOrEmpty(result)
                    result := result:SubString(0, len)
                ENDIF
            ELSE
                IF String.IsNullOrEmpty(result)
                    result := STRING{ ' ', SELF:length }
                ENDIF
            ENDIF
            RETURN result


    END CLASS
    /// <summary>Class for reading / writing Date Columns</summary>
    CLASS DbfDateColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL str AS STRING
            LOCAL result AS IDate
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            str :=  SUPER:_GetString(buffer)
            IF String.IsNullOrWhiteSpace(str)
                result := DbDate{0,0,0}
            ELSEIF str:Length == 8
                VAR year  := Int32.Parse(str:Substring(0,4))
                VAR month := Int32.Parse(str:Substring(4,2))
                VAR day   := Int32.Parse(str:Substring(6,2))
                result := DbDate{Year, Month, Day}
            ELSE
                result := DbDate{0,0,0}
            ENDIF
            RETURN result

        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS System.DateTime VAR dt
                IF dt == DateTime.MinValue
                    oValue := DbDate{0,0,0}
                ELSE
                    oValue := DbDate{dt:Year, dt:Month, dt:Day}
                ENDIF
            ENDIF
            IF oValue IS IDate VAR dValue
                LOCAL str AS STRING
                IF dValue:IsEmpty
                    str := Space(8)
                ELSE
                    VAR dt := DateTime{dValue:Year, dValue:Month, dValue:Day}
                    str := dt:ToString( "yyyyMMdd" )
                ENDIF
                SELF:_PutString(buffer, str)
                RETURN TRUE
            ENDIF
            SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
            RETURN FALSE


        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbDate{0,0,0}

        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8  .AND.  SELF:Decimals == 0 


    END CLASS

    /// <summary>Class for reading / writing Logic Columns</summary>
    CLASS DbfLogicColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL str AS STRING
            LOCAL result AS LOGIC
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            str := SUPER:_GetString(buffer)
            IF String.IsNullOrEmpty(str)
                result := FALSE
            ELSE
                SWITCH str[0]
                CASE 'T'
                CASE 't'
                CASE 'Y'
                CASE 'y'
                    result := TRUE
                OTHERWISE
                    result := FALSE
                END SWITCH
            ENDIF
            RETURN result

       OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS LOGIC VAR logicValue
                buffer[SELF:Offset] := IIF( logicValue, (BYTE)'T', (BYTE)'F' )
                RETURN TRUE
            ENDIF
            SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
            RETURN FALSE

       OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN FALSE

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 1  .AND.  SELF:Decimals == 0 
    END CLASS

    /// <summary>Class for reading / writing Numeric Columns</summary>
    CLASS DbfNumericColumn INHERIT DbfColumn

    CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL str AS STRING
            LOCAL result AS IFloat
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            str := SUPER:_GetString(buffer)
            LOCAL r8 AS REAL8
            IF !String.IsNullOrEmpty(str)
                r8 := Convert.ToDouble(_val(str))
            ELSE
                r8 := 0.0
            ENDIF
            result := DbFloat{r8, SELF:Length, SELF:Decimals}
            RETURN result

      OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL r8Value AS REAL8
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT r8Value)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            VAR numformat := SELF:RDD:_NumFormat
            numformat:NumberDecimalDigits := SELF:Decimals
            LOCAL str AS STRING
            str := R8Value:ToString("F", numformat)
            LOCAL lDataWidthError := FALSE AS LOGIC
            IF str:Length > length 
                str := STRING{'*', length}
                lDataWidthError := TRUE
            ELSE
                str := str:PadLeft(length,' ')
            ENDIF
            SELF:_PutString(buffer, str)
            IF lDataWidthError
                SELF:RDD:_dbfError(ERDD_DATAWIDTH, EG_DATAWIDTH)
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
    CLASS DbfMemoColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN


        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            // Read the Memo Block Number
            LOCAL str AS STRING
            LOCAL result AS LONG
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            IF SELF:Length == 10
                str     := SUPER:_GetString(buffer)
                result  := Convert.ToInt32(_Val(str))
            ELSE
                result := BuffToLong(buffer, SELF:OffSet)
            ENDIF
            RETURN result

        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes((LONG) 0)
                Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)
            ENDIF
            RETURN

        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            // oValue is the memo block number
            LOCAL intValue AS LONG
            IF ! SELF:GetNumber(oValue, OUT intValue)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes(intValue)
                Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)
            ELSE
                LOCAL strValue AS STRING
                IF intValue == 0
                    strValue := STRING{' ',10}
                ELSE
                    strValue := intValue:ToString():PadLeft(10,' ')
                ENDIF
                SELF:_PutString(buffer, strValue)
            ENDIF
            RETURN TRUE

        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN NULL

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN (SELF:Length == 10 .OR. SELF:Length == 4)  .AND.  SELF:Decimals == 0 


    END CLASS

    /// <summary>Class for reading / writing Integer Columns. </summary>
    CLASS DbfIntegerColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS LONG
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            IF SELF:Length == 4
                result := BuffToLong(buffer, SELF:OffSet)
            ELSEIF Length == 2
                result := BuffToShort(buffer, SELF:OffSet)
            ELSE
                result := 0
            ENDIF
            RETURN result

        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes((LONG) 0)
                Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)

            ELSEIF SELF:Length == 2
                VAR data := BitConverter.GetBytes((SHORT) 0)
                Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)

            ENDIF
            RETURN

        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL intValue AS LONG
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT intValue)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes(intValue)
                Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)
            ELSEIF SELF:Length == 2
                VAR data := BitConverter.GetBytes((SHORT) intValue)
                Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)
            ENDIF
            RETURN TRUE


        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN 0

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN (SELF:Length == 4 .OR. SELF:Length == 2)  .AND.  SELF:Decimals == 0 

    END CLASS

    /// <summary>Class for reading / writing AutoIncrement Columns. </summary>
    CLASS DbfAutoIncrementColumn INHERIT DbfIntegerColumn
        INTERNAL IncrStep as LONG
        INTERNAL Counter  AS LONG

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        VIRTUAL METHOD Read() AS LOGIC
            LOCAL oField  as DbfField
            oField := DbfField{}
            oField:initialize()
            SELF:RDD:_readField(SELF:OffSetInHeader, oField)
            SELF:Counter := oField:Counter
            SELF:IncrStep := oField:IncStep
            RETURN TRUE

        VIRTUAL METHOD Write() AS LOGIC
            LOCAL oField  as DbfField
            oField := DbfField{}
            oField:initialize()
            SELF:RDD:_readField(SELF:OffSetInHeader, oField)
            oField:Counter := SELF:Counter 
            oField:IncStep := (BYTE) SELF:IncrStep 
            SELF:RDD:_writeField(SELF:OffSetInHeader, oField)
            RETURN TRUE


        VIRTUAL METHOD NewRecord(buffer AS BYTE[]) AS VOID
            // when shared then read the header again to get the current values of the counter and incrstep
            // increment the counter and write to the header
            // then write the current value to the buffer
            LOCAL nCurrent as LONG
            nCurrent := SELF:Counter
            IF SELF:RDD:HeaderLock(DbLockMode.Lock)
            
                SELF:Read()
                SELF:Counter += SELF:IncrStep
                SELF:Write()
                SELF:RDD:HeaderLock( DbLockMode.UnLock )
            ENDIF
            SELF:PutValue(nCurrent, buffer)
            RETURN


    END CLASS

    /// <summary>Class for reading / writing Double Columns. </summary>
    CLASS DbfDoubleColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS REAL8
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            result := BitConverter.ToDouble(buffer, SELF:offset)
            RETURN DbFloat{result,-1,-1}

        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            VAR data := BitConverter.GetBytes((REAL8) 0)
            Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)
            RETURN

        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL r8Value AS REAL8
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT r8Value)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            VAR data := BitConverter.GetBytes(r8Value)
            Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)
            RETURN TRUE

       OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbFLoat{0,-1,-1}

        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8
    END CLASS

    /// <summary>Class for reading / writing Currency Columns. </summary>
    CLASS DbfCurrencyColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL tmp AS REAL8
            LOCAL result AS Decimal
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            tmp := BitConverter.ToInt64(buffer, SELF:offset)
            result := (decimal) (tmp/ (10 ^ SELF:Decimals))
            RETURN result


        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL currValue AS System.Decimal
            LOCAL i64Value AS System.Int64
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT currValue)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            i64Value := (INT64) (currValue * (System.Decimal) (10^ SELF:Decimals))
            VAR data := BitConverter.GetBytes(i64Value)
            Array.Copy(data, 0, buffer, SELF:OffSet, SELF:Length)
            RETURN TRUE


        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN (Decimal) 0
            
        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8

    END CLASS

    /// <summary>Class for reading / writing DateTime Columns. </summary>
    CLASS DbfDateTimeColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        INTERNAL METHOD _julianToDateTime(julian AS LONG) AS System.DateTime
            VAR jd := (REAL8) julian
            VAR z  := Math.Floor(jd + 0.5)
            VAR w  := Math.Floor( (z -1867216.25) / 36524.25)
            VAR x  := Math.Floor(w / 4)
            VAR aa := Math.Floor(z + 1 + w - x)
	        VAR bb := Math.Floor(aa + 1524)
	        VAR cc := Math.Floor((bb - 122.1) / 365.25)
	        VAR dd := Math.Floor(365.25 * cc)
	        VAR ee := Math.Floor((bb - dd) / 30.6001)
	        VAR ff := Math.Floor(30.6001 * ee)
            VAR day := bb - dd - ff
	        LOCAL month AS REAL8
            LOCAL year AS REAL8

	        IF ((ee - 13) <= 12 .AND.  (ee - 13) > 0)
		        month := ee - 13
	        ELSE
		        month := ee - 1
            ENDIF

	        IF (month == 1 || month == 2)
    	        year := cc - 4715
	        ELSE
		        year := cc - 4716
            ENDIF
            RETURN DateTime { (INT)year, (INT) month, (INT) day}

        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS DateTime 
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            result := DateTime.Minvalue
            VAR julian  := System.BitConverter.ToInt32(buffer, SELF:OffSet)
            VAR milsecs := System.BitConverter.ToInt32(buffer, SELF:OffSet+4)
            IF julian == 0 .AND. milsecs == 0
                RETURN result
            ENDIF
            // blank buffer ?
            IF julian == 0x20202020 .AND. milsecs == 0x20202020
                RETURN result
            ENDIF
            result := SELF:_julianToDateTime(julian)
            result := result.AddSeconds( (Int32) Math.Round ( (REAL8) milsecs / 1000.0))
            RETURN result

        // Convert a System.DateTime Date to a Julian Date
        PRIVATE CONST igreg := 15 + 31 * (10 + 12 * 1582) AS INT
        INTERNAL METHOD _dateTimeToJulian( dt AS DateTime ) AS LONG
            VAR y := dt:Year
		    VAR m := dt:Month
		    VAR day := dt:Day
		    IF y < 0
                y := y + 1
            ENDIF
		    IF m > 2
			    m := m + 1
		    ELSE
			    y := y - 1
			    m := m + 13
		    ENDIF

		    VAR ijulian := (INT)(365.25 * y) + (INT)(30.6001 * m) + day + 1720995

		    IF day + 31 * (m + 12 * y) >= igreg
			    // change for Gregorian calendar
			    VAR adj := y / 100
			    ijulian := ijulian + 2 - adj + adj / 4
		    ENDIF
            RETURN ijulian
            
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL empty AS LOGIC
            LOCAL dt AS DateTime
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS DateTime
                dt := (DateTime) oValue
                empty := dt == DateTime.MinValue
            ELSEIF oValue IS IDate VAR oDate
                dt := DateTime{oDate:Year, oDate:Month, oDate:Day}
                empty := oDate:IsEmpty
            ELSE
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            IF empty
                SELF:InitValue(buffer)
            ELSE
                VAR iJulian  := SELF:_dateTimeToJulian(dt)
                VAR milisecs := 1000 * dt:Second + dt:Minute * 1000 * 60 + dt:Hour * 1000 * 60 * 60
                VAR data := BitConverter.GetBytes(iJulian)
                Array.Copy(data, 0, buffer, SELF:OffSet, 4)
                data := BitConverter.GetBytes(milisecs)
                Array.Copy(data, 0, buffer, SELF:OffSet+4, 4)
            ENDIF

            RETURN TRUE


        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            VAR data := BitConverter.GetBytes((LONG) 0)
            Array.Copy(data, 0, buffer, SELF:OffSet, 4)
            Array.Copy(data, 0, buffer, SELF:OffSet+4, 4)
            RETURN

        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DateTime.MinValue

        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8  .AND.  SELF:Decimals == 0 

    END CLASS
    /// <summary>Class for reading / writing the Special Column for NULL values. </summary>
    CLASS DbfNullColumn INHERIT DbfColumn
        PRIVATE bitArray AS System.Collections.BitArray
        PRIVATE data     AS BYTE[]

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            data    := BYTE[]{SELF:Length}
            bitArray:= System.Collections.BitArray{data}
            RETURN

       OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            SELF:bitArray:SetAll(FALSE)
            SELF:PutValue(0, buffer)
            RETURN

       OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            // Read the bits from the buffer, dummy return value
            System.Array.Copy(buffer, SELF:Offset, data, 0, SELF:Length)
            bitArray:= System.Collections.BitArray{data}
            RETURN NULL

        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            // Write the bits to the buffer, ignore oValue
            SELF:BitArray:CopyTo(data, 0)
            System.Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            RETURN FALSE

        METHOD GetBit(nPos AS LONG) AS LOGIC
            RETURN SELF:bitArray:Get(nPos)

        METHOD SetBit(nPos AS LONG, lSet AS LOGIC) AS VOID
            SELF:bitArray:Set(nPos, lSet)
            RETURN

    END CLASS

END NAMESPACE
