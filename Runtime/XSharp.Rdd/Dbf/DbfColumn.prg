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
    CLASS DbfColumn inherit RDDFieldInfo
        INTERNAL NullBit   := -1 as LONG
        INTERNAL LengthBit := -1 as LONG
        INTERNAL RDD        as XSharp.RDD.DBF

        STATIC METHOD Create(oInfo as RDDFieldInfo, oRDD as XSharp.RDD.DBF) AS DbfColumn
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
            CASE DbFieldType.Picture        // 'P'
            CASE DbFieldType.General        // 'G'
            CASE DbFieldType.Blob           // 'W'
                RETURN DbfFoxMemoColumn{oInfo,oRDD}
            CASE DbFieldType.NullFlags
                RETURN DbfNullColumn{oInfo,oRDD}
            CASE DbFieldType.VOObject       // 'O'
            CASE DbFieldType.Unknown        // 'O'
            OTHERWISE
                RETURN DbfColumn{oInfo,oRDD}
            END SWITCH


        STATIC METHOD Create(oField REF DbfField, oRDD as XSharp.RDD.DBF) AS DbfColumn
            LOCAL oInfo as RDDFieldInfo
            oInfo        := RddFieldInfo{oField:Name, oField:Type, oField:Len, oField:Dec, oField:Offset, oField:Flags}
            RETURN DbfColumn.Create(oInfo, oRDD)


        PROTECTED CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo)
            SELF:RDD        := oRDD
            RETURN


        VIRTUAL METHOD InitValue(buffer as BYTE[]) AS VOID
            RETURN

        PROTECTED METHOD _GetValue(buffer as BYTE[]) AS STRING
            // The default implementation returns the part of the buffer as a string
            RETURN SELF:RDD:_Encoding:GetString(buffer, SELF:OffSet, SELF:Length)

        VIRTUAL METHOD GetValue(buffer as BYTE[]) AS OBJECT
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            RETURN SELF:_GetValue(buffer)

        VIRTUAL METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            RETURN FALSE


        METHOD IsNull() as LOGIC
            IF SELF:IsNullable .and. SELF:RDD:_NullColumn != NULL
                RETURN SELF:RDD:_NullColumn:GetBit(SELF:NullBit)
            ENDIF
            RETURN FALSE

        PROTECTED METHOD SetNullValue() AS LOGIC 
            IF SELF:IsNullable .and. SELF:RDD:_NullColumn != NULL
                SELF:RDD:_NullColumn:SetBit(SELF:NullBit, TRUE)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        PROTECTED METHOD ClearNullValue() AS VOID
            IF SELF:IsNullable .and. SELF:RDD:_NullColumn != NULL
                SELF:RDD:_NullColumn:SetBit(SELF:NullBit, FALSE)
            ENDIF
            RETURN 

        VIRTUAL METHOD EmptyValue() AS OBJECT
            RETURN String.Empty


        PROTECTED METHOD GetNumber<T>(oValue as OBJECT, oResult OUT T) AS LOGIC WHERE T IS NEW()
            LOCAL tc as TypeCode
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

    END CLASS


    CLASS DbfCharacterColumn INHERIT DbfColumn

         CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo, oRDD)
            IF SELF:Decimals != 0
                SELF:Length         += SELF:Decimals * 256
			    SELF:Decimals       := 0
            ENDIF
            RETURN

       OVERRIDE METHOD InitValue(buffer as BYTE[]) AS VOID
            IF SELF:IsVarLength
                buffer[SELF:OffSet + SELF:Length-1] := 0
            ENDIF
            RETURN

       OVERRIDE METHOD EmptyValue() AS OBJECT
           RETURN String{ ' ', SELF:length }

       OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            LOCAL str as STRING
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS Char
                str := ((Char) oValue):ToString()
            ELSEIF oValue IS String
                str := (STRING) oValue
            ELSE
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            IF str:Length < SELF:length .and. ! SELF:IsVarLength
                str := str:PadRight(SELF:length,' ')
            ENDIF
            IF SELF:IsBinary
                local nLen := Math.Min(SELF:Length, str:Length) AS LONG
                System.Text.Encoding.@@Default:GetBytes(str, 0, nLen, buffer, SELF:Offset)
            ELSE
                SELF:RDD:_Encoding:GetBytes(str, 0, SELF:Length, buffer, SELF:Offset)
            ENDIF
            IF SELF:IsVarLength
                IF SELF:RDD:_NullColumn != NULL
                    if str:Length >= SELF:Length
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
                return FALSE
            ENDIF
            RETURN TRUE

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local result as STRING
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            IF SELF:IsBinary
                result := System.Text.Encoding.@@Default:GetString(buffer, SELF:OffSet, SELF:Length)
            ELSE
                result := SUPER:_GetValue(buffer)
            ENDIF
            IF IsVarLength
                LOCAL len AS LONG
                len := SELF:Length
                IF SELF:RDD:_NullColumn != NULL
                    LOCAL lSet as LOGIC
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

    CLASS DbfDateColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local str as STRING
            local result as IDate
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            str :=  SUPER:_GetValue(buffer)
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

        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS System.DateTime VAR dt
                oValue := DbDate{dt:Year, dt:Month, dt:Day}
            ENDIF
            IF oValue IS IDate
                local dValue as IDate
                local str as String
                dValue := (IDate) oValue
                IF dValue:IsEmpty
                    str := Space(8)
                ELSE
                    VAR dt := DateTime{dValue:Year, dValue:Month, dValue:Day}
                    str := dt:ToString( "yyyyMMdd" )
                ENDIF
                SELF:RDD:_Encoding:GetBytes( str, 0, SELF:length, buffer, SELF:Offset )
                RETURN TRUE
            ENDIF
            SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
            RETURN FALSE


        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbDate{0,0,0}

        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8  .AND.  SELF:Decimals == 0 


    END CLASS

    CLASS DbfLogicColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local str as STRING
            local result as LOGIC
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            str := SUPER:_GetValue(buffer)
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

       OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS LOGIC
                buffer[SELF:Offset] := IIF( (LOGIC)oValue, (BYTE)'T', (BYTE)'F' )
                RETURN TRUE
            ENDIF
            SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
            RETURN FALSE

       OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN FALSE

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 1  .AND.  SELF:Decimals == 0 
    END CLASS

    CLASS DbfNumericColumn INHERIT DbfColumn

    CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local str as STRING
            local result as IFloat
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            str := SUPER:_GetValue(buffer)
            local r8 as Real8
            IF !String.IsNullOrEmpty(str)
                r8 := Convert.ToDouble(_val(str))
            ELSE
                r8 := 0.0
            ENDIF
            result := DbFloat{r8, SELF:Length, SELF:Decimals}
            RETURN result

      OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            LOCAL r8Value as REAL8
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber<REAL8>(oValue, OUT r8Value)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            var numformat := SELF:RDD:_NumFormat
            numformat:NumberDecimalDigits := SELF:Decimals
            local str as STRING
            str := R8Value:ToString("F", numformat)
            LOCAL lDataWidthError := FALSE AS LOGIC
            IF str:Length > length 
                str := STRING{'*', length}
                lDataWidthError := TRUE
            ELSE
                str := str:PadLeft(length)
            ENDIF
            SELF:RDD:_Encoding:GetBytes( str, 0, length, buffer, offset )
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

    CLASS DbfMemoColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN


        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local str as STRING
            local result as Long
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            if SELF:Length == 10
                str := SUPER:_GetValue(buffer)
                result := _Val(str)
            ELSE
                result := BuffToLong(buffer, SELF:OffSet)
            ENDIF
            RETURN result

        OVERRIDE METHOD InitValue(buffer as BYTE[]) AS VOID
            if SELF:Length == 4
                var data := BitConverter.GetBytes((LONG) 0)
                Array.Copy(data, 0, buffer, self:OffSet, self:Length)
            ENDIF
            RETURN

        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            // oValue is the memo block number
            RETURN FALSE

        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN 0

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN (SELF:Length == 10 .or. SELF:Length == 4)  .AND.  SELF:Decimals == 0 


    END CLASS

    CLASS DbfIntegerColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local result as Long
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            if SELF:Length == 4
                result := BuffToLong(buffer, SELF:OffSet)
            ELSEIF Length == 2
                result := BuffToShort(buffer, SELF:OffSet)
            ELSE
                result := 0
            ENDIF
            RETURN result

        OVERRIDE METHOD InitValue(buffer as BYTE[]) AS VOID
            if SELF:Length == 4
                var data := BitConverter.GetBytes((LONG) 0)
                Array.Copy(data, 0, buffer, self:OffSet, self:Length)

            ELSEif SELF:Length == 2
                var data := BitConverter.GetBytes((SHORT) 0)
                Array.Copy(data, 0, buffer, self:OffSet, self:Length)

            ENDIF
            RETURN

        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            LOCAL intValue as LONG
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber<LONG>(oValue, OUT intValue)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            IF SELF:Length == 4
                var data := BitConverter.GetBytes(intValue)
                Array.Copy(data, 0, buffer, self:OffSet, self:Length)
            ELSEIF SELF:Length == 2
                var data := BitConverter.GetBytes((SHORT) intValue)
                Array.Copy(data, 0, buffer, self:OffSet, self:Length)
            ENDIF
            RETURN TRUE


        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN 0

       OVERRIDE METHOD Validate() AS LOGIC
            RETURN (SELF:Length == 4 .or. SELF:Length == 2)  .AND.  SELF:Decimals == 0 

    END CLASS

    CLASS DbfAutoIncrementColumn INHERIT DbfIntegerColumn
        INTERNAL IncrStep as LONG
        INTERNAL Counter  AS LONG

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            //SELF:IncrStep := oField:IncStep
            //SELF:Counter  := oField:Counter

            RETURN
    END CLASS

    CLASS DbfDoubleColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local result as Real8
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            result := BitConverter.ToDouble(buffer, SELF:offset)
            RETURN DbFloat{result,-1,-1}

        OVERRIDE METHOD InitValue(buffer as BYTE[]) AS VOID
            var data := BitConverter.GetBytes((REAL8) 0)
            Array.Copy(data, 0, buffer, self:OffSet, self:Length)
            RETURN

        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            LOCAL r8Value as REAL8
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber<REAL8>(oValue, OUT r8Value)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            var data := BitConverter.GetBytes(r8Value)
            Array.Copy(data, 0, buffer, self:OffSet, self:Length)
            RETURN TRUE

       OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbFLoat{0,-1,-1}

        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8
    END CLASS

 
    CLASS DbfCurrencyColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local tmp as Real8
            local result as Decimal
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            tmp := BitConverter.ToInt64(buffer, SELF:offset)
            result := tmp/ (10 ^ self:Decimals)
            RETURN result


        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            LOCAL currValue as System.Decimal
            LOCAL i64Value as System.Int64
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber<System.Decimal>(oValue, OUT currValue)
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                RETURN FALSE
            ENDIF
            i64Value := currValue * (System.Decimal) (10^ self:Decimals)
            var data := BitConverter.GetBytes(i64Value)
            Array.Copy(data, 0, buffer, self:OffSet, self:Length)
            RETURN TRUE


        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN (Decimal) 0
            
        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8

    END CLASS

    CLASS DbfDateTimeColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        INTERNAL METHOD _julianToDateTime(julian AS LONG) AS System.DateTime
            var jd := (Real8) julian
            var z  := Math.Floor(jd + 0.5)
            var w  := Math.Floor( (z -1867216.25) / 36524.25)
            var x  := Math.Floor(w / 4)
            var aa := Math.Floor(z + 1 + w - x)
	        var bb := Math.Floor(aa + 1524)
	        var cc := Math.Floor((bb - 122.1) / 365.25)
	        var dd := Math.Floor(365.25 * cc)
	        var ee := Math.Floor((bb - dd) / 30.6001)
	        var ff := Math.Floor(30.6001 * ee)
            var day := bb - dd - ff
	        local month as real8
            local year as real8

	        if ((ee - 13) <= 12 .and.  (ee - 13) > 0)
		        month := ee - 13
	        else
		        month := ee - 1
            endif

	        if (month == 1 || month == 2)
    	        year := cc - 4715
	        else
		        year := cc - 4716
            endif
            return DateTime { (int)year, (int) month, (int) day}

        OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local result as DateTime 
            IF SELF:IsNull()
                RETURN NULL
            ENDIF
            result := DateTime.Minvalue
            var julian  := System.BitConverter.ToInt32(buffer, SELF:OffSet)
            var milsecs := System.BitConverter.ToInt32(buffer, SELF:OffSet+4)
            if julian == 0 .and. milsecs == 0
                return result
            endif
            // blank buffer ?
            if julian == 0x20202020 .and. milsecs == 0x20202020
                return result
            endif
            result := SELF:_julianToDateTime(julian)
            result := result.AddSeconds( (Int32) Math.Round ( (Real8) milsecs / 1000.0))
            RETURN result

        // Convert a System.DateTime Date to a Julian Date
        PRIVATE CONST igreg := 15 + 31 * (10 + 12 * 1582) AS INT
        INTERNAL METHOD _dateTimeToJulian( dt AS DateTime ) AS LONG
            var y := dt:Year
		    var m := dt:Month
		    var day := dt:Day
		    if y < 0
                y := y + 1
            endif
		    if m > 2
			    m := m + 1
		    else
			    y := y - 1
			    m := m + 13
		    ENDIF

		    var ijulian := (int)(365.25 * y) + (int)(30.6001 * m) + day + 1720995

		    if day + 31 * (m + 12 * y) >= igreg
			    // change for Gregorian calendar
			    var adj := y / 100
			    ijulian := ijulian + 2 - adj + adj / 4
		    endif
            return ijulian
            
        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            local dt as DateTime
            local empty as logic
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS DateTime
                dt    := (DateTime) oValue
                empty := dt == DateTime.MinValue
            ELSEIF oValue IS IDate
                local oDate as IDate
                oDate := (IDate) oValue
                dt := DateTime{oDate:Year, oDate:Month, oDate:Day}
                empty := oDate:IsEmpty
            ELSE
                SELF:RDD:_dbfError(SubCodes.ERDD_DATATYPE, EG_DATATYPE)
                return FALSE
            ENDIF
            if empty
                SELF:InitValue(buffer)
            ELSE
                var iJulian  := SELF:_dateTimeToJulian(dt)
                var milisecs := 1000 * dt:Second + dt:Minute * 1000 * 60 + dt:Hour * 1000 * 60 * 60
                var data := BitConverter.GetBytes(iJulian)
                Array.Copy(data, 0, buffer, self:OffSet, 4)
                data := BitConverter.GetBytes(milisecs)
                Array.Copy(data, 0, buffer, self:OffSet+4, 4)
            ENDIF

            RETURN TRUE


        OVERRIDE METHOD InitValue(buffer as BYTE[]) AS VOID
            var data := BitConverter.GetBytes((LONG) 0)
            Array.Copy(data, 0, buffer, self:OffSet, 4)
            Array.Copy(data, 0, buffer, self:OffSet+4, 4)
            RETURN

        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DateTime.MinValue

        OVERRIDE METHOD Validate() AS LOGIC
            RETURN SELF:Length == 8  .AND.  SELF:Decimals == 0 

    END CLASS

    CLASS DbfFoxMemoColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

       OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            local result as Long
            result := BuffToLong(buffer, SELF:OffSet)
            RETURN result

        OVERRIDE METHOD InitValue(buffer as BYTE[]) AS VOID
            var data := BitConverter.GetBytes((LONG) 0)
            Array.Copy(data, 0, buffer, self:OffSet, self:Length)
            RETURN

        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            // Todo oValue = the block number
            IF SELF:IsNullable
                IF oValue == NULL
                    RETURN SELF:SetNullValue()
                ENDIF
                SELF:ClearNullValue()
            ENDIF 
            RETURN FALSE

        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN NULL

    END CLASS

    CLASS DbfNullColumn INHERIT DbfColumn
        PRIVATE bitArray as System.Collections.BitArray
        PRIVATE data     as Byte[]

        CONSTRUCTOR(oInfo as RddFieldInfo,oRDD as XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            data    := byte[]{SELF:Length}
            bitArray:= System.Collections.BitArray{data}
            RETURN

       OVERRIDE METHOD InitValue(buffer as BYTE[]) AS VOID
            self:bitArray:SetAll(FALSE)
            SELF:PutValue(0, buffer)
            RETURN

       OVERRIDE METHOD GetValue(buffer as BYTE[]) AS OBJECT
            // Read the bits from the buffer, dummy return value
            System.Array.Copy(buffer, SELF:Offset, data, 0, SELF:Length)
            bitArray:= System.Collections.BitArray{data}
            RETURN NULL

        OVERRIDE METHOD PutValue(oValue as OBJECT, buffer as BYTE[]) AS LOGIC
            // Write the bits to the buffer, ignore oValue
            SELF:BitArray:CopyTo(data, 0)
            System.Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            RETURN FALSE

        METHOD GetBit(nPos as LONG) AS LOGIC
            return SELF:bitArray:Get(nPos)

        METHOD SetBit(nPos as LONG, lSet as LOGIC) AS VOID
            SELF:bitArray:Set(nPos, lSet)
            RETURN

    END CLASS

END NAMESPACE
