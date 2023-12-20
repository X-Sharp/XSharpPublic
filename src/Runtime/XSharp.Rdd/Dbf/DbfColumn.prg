//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.IO
USING XSharp.RDD.Enums
USING System.Collections.Generic
USING System.Linq
USING System.Runtime.CompilerServices
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING STATIC XSharp.Conversions
BEGIN NAMESPACE XSharp.RDD
    /// <summary>Class for DBF Column reading / writing </summary>
    CLASS DbfColumn INHERIT RddFieldInfo
        INTERNAL NullBit        := -1 AS LONG
        INTERNAL LengthBit      := -1 AS LONG
        INTERNAL OffSetInHeader := -1 as LONG
        INTERNAL RDD            AS XSharp.RDD.DBF
        /// <summary>Create a DbfColumn class, used when creating files.</summary>
        STATIC METHOD Create(oInfo AS RddFieldInfo, oRDD AS XSharp.RDD.DBF) AS DbfColumn
            // Commented out types are Harbour specific
            SWITCH oInfo:FieldType
            CASE DbFieldType.Character   // C
            CASE DbFieldType.VarChar     // 'V'
            CASE DbFieldType.VarBinary   // 'Q'
                RETURN DbfCharacterColumn{oInfo,oRDD}
            CASE DbFieldType.Date        // D
                RETURN DbfDateColumn{oInfo,oRDD}
            CASE DbFieldType.Number      // N
            CASE DbFieldType.Float          // 'F'
                RETURN DbfNumericColumn{oInfo,oRDD}
            CASE DbFieldType.Memo        // M
            CASE DbFieldType.Picture        // 'P'
            CASE DbFieldType.General        // 'G'
            CASE DbFieldType.Blob           // 'W'
                RETURN DbfMemoColumn{oInfo,oRDD}
            CASE DbFieldType.Logic       // L
                RETURN DbfLogicColumn{oInfo,oRDD}
            CASE DbFieldType.Integer    // 'I'
            //CASE DbFieldType.Integer2   // '2'
            //CASE DbFieldType.Integer4   // '4'
                IF oInfo:IsAutoIncrement
                    RETURN DbfAutoIncrementColumn{oInfo,oRDD}
                ENDIF
                RETURN DbfIntegerColumn{oInfo,oRDD}
            //CASE DbFieldType.AutoIncrement
            //   RETURN DbfAutoIncrementColumn{oInfo}
            CASE DbFieldType.Double         // 'B'
            //CASE DbFieldType.Double8        // '8'
                RETURN DbfDoubleColumn{oInfo,oRDD}
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

        /// <summary>Create a DbfColumn class, based on the definition in the DBF. Used when opening a file. </summary>
        STATIC METHOD Create(oField REF DbfField, oRDD AS XSharp.RDD.DBF, nHeaderOffSet AS LONG) AS DbfColumn
            LOCAL oInfo AS RddFieldInfo
            LOCAL oColumn as DbfColumn
            oInfo        := RddFieldInfo{oField:Name, oField:Type, oField:Len, oField:Dec, oField:Offset, oField:Flags}
            oColumn      := DbfColumn.Create(oInfo, oRDD)
            oColumn:OffSetInHeader := nHeaderOffSet
            IF oColumn is DbfAutoIncrementColumn VAR dbfac
                dbfac:IncrStep  := oField:IncStep
                dbfac:Counter   := oField:Counter
            ENDIF
            IF oInfo:HasProperties
                oColumn:Properties := oInfo:Properties
            ENDIF
            return oColumn

        INTERNAL METHOD TypeError(cType AS STRING, oValue AS OBJECT) AS STRING
            RETURN i"{cType} value expected for column '{ColumnName}' but received the value ({oValue}) of type {oValue.GetType()}"

        PROTECTED CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo)
            SELF:RDD        := oRDD
            RETURN

       /// <summary>Initialize the buffer used for appends. Gets called once when a DBF is opened.</summary>
       VIRTUAL METHOD InitValue(buffer AS BYTE[]) AS VOID
            // this gets called to update the initialize the buffer for 'append blank'. Gets called once when a DBF is opened.
            RETURN

       /// <summary>Update the buffer after appending. For example for AutoIncrement or TimeStamp columns.</summary>
       /// <param name="buffer">Record buffer for the current record</param>
       VIRTUAL METHOD NewRecord(buffer AS BYTE[]) AS VOID
            // this gets called to update the appended record. Only used at this moment to set the AutoIncrement value
            // but can also be used to set a timestamp or rowversion later
            RETURN

        PROTECTED METHOD _GetString(buffer AS BYTE[]) AS STRING
            // The default implementation returns the part of the buffer as a string
            RETURN SELF:RDD:_Encoding:GetString(buffer, SELF:Offset, SELF:Length)

        /// <summary>Get the value from the buffer</summary>
       /// <param name="buffer">Record buffer for the current record</param>
        VIRTUAL METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            RETURN SELF:_GetString(buffer)

        /// <summary>Handle NULL values.</summary>
       /// <param name="buffer">Record buffer for the current record</param>
        VIRTUAL METHOD HandleNullValue(buffer AS BYTE[]) AS LOGIC
            SELF:InitValue(buffer)
            IF SELF:IsNullable
                RETURN SELF:SetNullValue()
            ENDIF
            RETURN TRUE

       /// <summary>Write the value to the buffer</summary>
       /// <param name="buffer">Record buffer for the current record</param>
       /// <param name="oValue">Value to write.</param>
        VIRTUAL METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            RETURN FALSE

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
         INTERNAL METHOD IsNull() AS LOGIC
            IF SELF:IsNullable .AND. SELF:RDD:_NullColumn != NULL
                RETURN SELF:RDD:_NullColumn:GetBit(SELF:NullBit)
            ENDIF
            RETURN FALSE

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
        PROTECTED METHOD SetNullValue() AS LOGIC
            IF SELF:IsNullable .AND. SELF:RDD:_NullColumn != NULL
                SELF:RDD:_NullColumn:SetBit(SELF:NullBit, TRUE)
                RETURN TRUE
            ENDIF
            RETURN FALSE

         [MethodImpl(MethodImplOptions.AggressiveInlining)];
         PROTECTED METHOD ClearNullValue() AS VOID
            IF SELF:IsNullable .AND. SELF:RDD:_NullColumn != NULL
                SELF:RDD:_NullColumn:SetBit(SELF:NullBit, FALSE)
            ENDIF
            RETURN

        /// <summary>Get the default "empty" value, as you would get at EOF</summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)];
        VIRTUAL METHOD EmptyValue() AS OBJECT
            RETURN String.Empty

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
        PROTECTED METHOD _GetNumericValue(buffer AS BYTE[]) AS INT64
            LOCAL lValid := TRUE AS LOGIC
            LOCAL nValue AS INT64
            LOCAL nPos, nLen AS LONG
            nValue := 0
            nLen := SELF:Length
            nPos := 0
            IF SELF:Offset+nLen >= buffer:Length
                NOP
            ENDIF
            DO WHILE nPos < nLen .AND. buffer[SELF:Offset+nPos] == 32
                nPos++
            ENDDO
            FOR VAR i := nPos TO nLen -1
                LOCAL b AS BYTE
                b := buffer[SELF:Offset+i]
                SWITCH b
                CASE 48  // 0
                CASE 49  // 1
                CASE 50  // 2
                CASE 51  // 3
                CASE 52  // 4
                CASE 53  // 5
                CASE 54  // 6
                CASE 55  // 7
                CASE 56  // 8
                CASE 57  // 9
                    nValue := nValue * 10 + (b - 48)
                OTHERWISE
                    lValid := FALSE
                END SWITCH
                IF ! lValid
                    EXIT
                ENDIF
            NEXT
            IF lValid
                RETURN nValue
            ENDIF
            RETURN 0

         [MethodImpl(MethodImplOptions.AggressiveInlining)];
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
                    TRY
                        IF oValue IS IFloat VAR fl
                            oValue := fl:Value
                        ELSEIF oValue IS ICurrency VAR cur
                            oValue := cur:Value
                        ENDIF
                        oResult := (T) Convert.ChangeType(oValue, TYPEOF(T) )
                        RETURN TRUE
                    CATCH
                        oResult := T{}
                        RETURN FALSE
                    END TRY
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
        /// <inheritdoc/>
       OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            IF SELF:IsVarLength
                buffer[SELF:Offset + SELF:Length-1] := 0
            ELSEIF SELF:IsUnicode
                VAR str := STRING{' ',SELF:Length /2}
                System.Buffer.BlockCopy(str:ToCharArray(),0, buffer, SELF:Offset, SELF:Length)
            ENDIF
            RETURN
       /// <inheritdoc/>
       OVERRIDE METHOD EmptyValue() AS OBJECT
           IF SELF:IsUnicode
                RETURN STRING{ ' ', SELF:Length /2 }
           ENDIF
           RETURN STRING{ ' ', SELF:Length }
        /// <inheritdoc/>
       OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL str AS STRING
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS CHAR
                str := ((CHAR) oValue):ToString()
            ELSEIF oValue IS STRING
                str := (STRING) oValue
            ELSE
                SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("String",oValue))
                RETURN FALSE
            ENDIF
            IF str:Length < SELF:Length .AND. ! SELF:IsVarLength
                IF SELF:IsBinary
                    str := str:PadRight(SELF:Length,'\0')
                ELSEIF SELF:IsUnicode
                    str := str:PadRight(SELF:Length/2,' ')
                ELSE
                    str := str:PadRight(SELF:Length,' ')
                ENDIF
            ENDIF
            IF SELF:IsBinary
                LOCAL nLen := Math.Min(SELF:Length, str:Length) AS LONG
                System.Text.Encoding.Default:GetBytes(str, 0, nLen, buffer, SELF:Offset)
            ELSEIF SELF:IsUnicode
                System.Buffer.BlockCopy(str:ToCharArray(),0, buffer, SELF:Offset, SELF:Length)
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
                        buffer[SELF:Offset + SELF:Length-1] := (BYTE) str:Length
                    ENDIF
                ELSE
                    buffer[SELF:Offset + SELF:Length-1] := (BYTE) Math.Max(str:Length, SELF:Length-1)
                ENDIF
            ENDIF
            RETURN TRUE

        /// <inheritdoc/>
        OVERRIDE METHOD Validate() AS LOGIC
            IF SELF:Length == 0  .OR. SELF:Decimals > 0  .OR. SELF:Length > System.UInt16.MaxValue
                RETURN FALSE
            ENDIF
            RETURN TRUE
        /// <inheritdoc/>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS STRING
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            IF SELF:IsBinary
                result := System.Text.Encoding.Default:GetString(buffer, SELF:Offset, SELF:Length)
            ELSEIF SELF:IsUnicode
                VAR chars := CHAR[]{SELF:Length /2}
                System.Buffer.BlockCopy(buffer, SELF:Offset, chars, 0, SELF:Length)
                result := STRING{chars}
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
                        len := buffer[SELF:Offset + SELF:Length-1]
                    ENDIF
                ENDIF
                IF !String.IsNullOrEmpty(result)
                    result := result:Substring(0, len)
                ENDIF
            ELSE
                IF String.IsNullOrEmpty(result)
                    result := STRING{ ' ', SELF:Length }
                ENDIF
            ENDIF
            RETURN result


    END CLASS
    /// <summary>Class for reading / writing Date Columns</summary>
    CLASS DbfDateColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN
        /// <inheritdoc/>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result := NULL AS IDate
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            LOCAL nValue := SELF:_GetNumericValue(buffer) AS INT64
            LOCAL lOk := FALSE AS LOGIC
            IF nValue != 0
                LOCAL nDay   := (LONG) nValue % 100 AS INT
                nValue /= 100
                LOCAL nMonth  := (LONG) nValue % 100 AS INT
                nValue /= 100
                LOCAL nYear   := (LONG) nValue AS INT
                IF nYear > 0 .AND. nDay > 0 .AND. nMonth > 0
                    result := DbDate{nYear,nMonth,nDay}
                    lOk := TRUE
                ENDIF
            ENDIF
            IF ! lOk
                result := DbDate{0,0,0}
            ENDIF
            RETURN result
        /// <inheritdoc/>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
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
                    VAR dt2 := DateTime{dValue:Year, dValue:Month, dValue:Day}
                    str := dt2:ToString( "yyyyMMdd" )
                ENDIF
                SELF:_PutString(buffer, str)
                RETURN TRUE
            ENDIF
            SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("Date",oValue))
            RETURN FALSE

        /// <inheritdoc/>

        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbDate{0,0,0}
        /// <inheritdoc/>

    END CLASS

    /// <summary>Class for reading / writing Logic Columns</summary>
    CLASS DbfLogicColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN

        /// <inheritdoc/>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS LOGIC
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            SWITCH buffer[SELF:Offset]
            CASE 84 // T
            CASE 116 // t
            CASE 89 // Y
            CASE 121 // y
                result := TRUE
            OTHERWISE
                result := FALSE
            END SWITCH
            RETURN result

        /// <inheritdoc/>
       OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS LOGIC VAR logicValue
                buffer[SELF:Offset] := IIF( logicValue, (BYTE)'T', (BYTE)'F' )
                RETURN TRUE
            ENDIF
            SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("Logic",oValue))
            RETURN FALSE
        /// <inheritdoc/>
       OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN FALSE

    END CLASS

    /// <summary>Class for reading / writing Numeric Columns</summary>
    CLASS DbfNumericColumn INHERIT DbfColumn

    CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN
        /// <inheritdoc/>
    OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS IFloat
            LOCAL r8 AS REAL8
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            LOCAL nPos, nLen AS LONG
            nLen := SELF:Length
            nPos := 0
            DO WHILE  nPos < nLen .AND. buffer[SELF:Offset+nPos] == 32
                nPos++
            ENDDO
            LOCAL lDec := FALSE AS LOGIC
            LOCAL lValid := TRUE AS LOGIC
            LOCAL nBefore, nCount, nDec AS INT64
            LOCAL lNegative := FALSE AS LOGIC
            LOCAL lFirst    := TRUE AS LOGIC
            LOCAL lLast     := FALSE AS LOGIC
            nBefore :=  nCount := nDec:= 0
            FOR VAR i := nPos TO nLen -1
                LOCAL b AS BYTE
                b := buffer[SELF:Offset+i]
                IF lLast .AND. b != 32
                    lValid := FALSE
                    EXIT
                ENDIF
                SWITCH b
                CASE 43  // +
                    IF ! lFirst             // in the middle is invalid
                        lValid := FALSE
                    ELSE
                        lNegative := FALSE
                    ENDIF
                CASE 45  // -
                    IF ! lFirst             // in the middle is invalid
                        lValid := FALSE
                    ELSE
                        lNegative := TRUE
                    ENDIF
                CASE 48  // 0
                CASE 49  // 1
                CASE 50  // 2
                CASE 51  // 3
                CASE 52  // 4
                CASE 53  // 5
                CASE 54  // 6
                CASE 55  // 7
                CASE 56  // 8
                CASE 57  // 9
                    nCount := nCount * 10 + (b - 48)
                    nDec   += 1
                CASE 46  // .
                    IF lDec // second Dot ?
                        lValid := FALSE
                    ELSE
                        nBefore := nCount
                        nCount  := 0
                        nDec    := 0
                        lDec    := TRUE
                    ENDIF
                CASE 32
                    lLast := TRUE
                CASE 42  // *  Numeric overflow
                OTHERWISE
                    lValid := FALSE
                END SWITCH
                IF ! lValid
                    EXIT
                ENDIF
                lFirst := FALSE
            NEXT
            IF lValid
                IF lDec
                    r8 := nBefore + (nCount * 1.0 / 10 ^ nDec)
                ELSE
                    r8 := nCount
                ENDIF
                IF lNegative
                    r8 := -r8
                ENDIF
            ELSE
                 r8 := 0
            ENDIF
            result := DbFloat{r8, SELF:Length, SELF:Decimals}
            RETURN result
        /// <inheritdoc/>
      OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL r8Value AS REAL8
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT r8Value)
                SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("Numeric",oValue))
                RETURN FALSE
            ENDIF
            VAR numformat := SELF:RDD:_numformat
            numformat:NumberDecimalDigits := SELF:Decimals
            LOCAL str AS STRING
            str := r8Value:ToString("F", numformat)
            LOCAL lDataWidthError := FALSE AS LOGIC
            IF str:Length > Length
                str := STRING{'*', Length}
                lDataWidthError := TRUE
            ELSE
                str := str:PadLeft(Length,' ')
            ENDIF
            SELF:_PutString(buffer, str)
            IF lDataWidthError
                SELF:RDD:_dbfError(Subcodes.ERDD_DATAWIDTH, EG_DATAWIDTH,__ENTITY__, i"Numeric value for column '{ColumnName}' ({oValue}) is too large")
            ENDIF
            RETURN TRUE
        /// <inheritdoc/>
        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbFloat{0.0, SELF:Length, SELF:Decimals}
        /// <inheritdoc/>
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

        /// <inheritdoc/>
        /// <remarks>At this level this returns the memo block number.</remarks>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            // Read the Memo Block Number
            LOCAL result AS LONG
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            IF SELF:Length == 10
                result  := (LONG) SELF:_GetNumericValue(buffer)
            ELSE
                result := BuffToLong(buffer, SELF:Offset)
            ENDIF
            RETURN result
        /// <inheritdoc/>
        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes((LONG) 0)
                Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            ENDIF
            RETURN
        /// <inheritdoc/>
        /// <remarks>At this level this writes the memo block number.</remarks>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            // oValue is the memo block number
            LOCAL intValue AS LONG
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT intValue)
                SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, i"Memo block number expected for column '{ColumnName}' but received the value ({oValue}) of type {oValue.GetType()}")
                RETURN FALSE
            ENDIF
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes(intValue)
                Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
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

        /// <inheritdoc/>
        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN NULL

        /// <inheritdoc/>
       OVERRIDE METHOD Validate() AS LOGIC
            IF SELF:Length != 4
                SELF:Length := 10
            ENDIF
            SELF:Decimals := 0
            RETURN TRUE


    END CLASS

    /// <summary>Class for reading / writing Integer Columns. </summary>
    CLASS DbfIntegerColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN
        /// <inheritdoc/>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS LONG
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            IF SELF:Length == 4
                result := BuffToLong(buffer, SELF:Offset)
            ELSEIF Length == 2
                result := BuffToShort(buffer, SELF:Offset)
            ELSE
                result := 0
            ENDIF
            RETURN result

        /// <inheritdoc/>
        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes((LONG) 0)
                Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)

            ELSEIF SELF:Length == 2
                VAR data := BitConverter.GetBytes((SHORT) 0)
                Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)

            ENDIF
            RETURN
        /// <inheritdoc/>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL intValue AS LONG
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT intValue)
                SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("Integer",oValue))
                RETURN FALSE
            ENDIF
            IF SELF:Length == 4
                VAR data := BitConverter.GetBytes(intValue)
                Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            ELSEIF SELF:Length == 2
                VAR data := BitConverter.GetBytes((SHORT) intValue)
                Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            ENDIF
            RETURN TRUE

        /// <inheritdoc/>
        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN 0

        /// <inheritdoc/>
       OVERRIDE METHOD Validate() AS LOGIC
            SELF:Decimals := 0
            IF SELF:Length == 0
                SELF:Length := 4
            ENDIF
            RETURN SELF:Length == 4 .OR. SELF:Length == 2

    END CLASS

    /// <summary>Class for reading / writing AutoIncrement Columns. </summary>
    CLASS DbfAutoIncrementColumn INHERIT DbfIntegerColumn
        INTERNAL IncrStep as LONG
        INTERNAL Counter  AS LONG

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN
        /// <summary>Read the next autoincrement number from the dbf header </summary>
        VIRTUAL METHOD Read() AS LOGIC
            LOCAL oField  as DbfField
            oField := DbfField{SELF:RDD:_Encoding}
            SELF:RDD:_readField(SELF:OffSetInHeader, oField)
            SELF:Counter := oField:Counter
            SELF:IncrStep := oField:IncStep
            RETURN TRUE

        /// <summary>Write the next autoincrement number to the dbf header </summary>
        VIRTUAL METHOD Write() AS LOGIC
            LOCAL oField  AS DbfField
            oField := DbfField{SELF:RDD:_Encoding}
            SELF:RDD:_readField(SELF:OffSetInHeader, oField)
            oField:Counter := SELF:Counter
            oField:IncStep := (BYTE) SELF:IncrStep
            SELF:RDD:_writeField(SELF:OffSetInHeader, oField)
            RETURN TRUE

        /// <inheritdoc/>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            // FoxPro throws an error when writing to an autonumber field. We do that too.
            SELF:RDD:_dbfError(Subcodes.ERDD_WRITE ,EG_READONLY,"PutValue", i"Field '{SELF:Name}' is ReadOnly")
            RETURN FALSE

        /// <inheritdoc/>
        OVERRIDE METHOD NewRecord(buffer AS BYTE[]) AS VOID
            // when shared then read the header again to get the current values of the counter and incrstep
            // increment the counter and write to the header
            // then write the current value to the buffer
            LOCAL nCurrent as LONG
            nCurrent := SELF:Counter
            DO WHILE ! SELF:RDD:HeaderLock(DbLockMode.Lock)
                NOP
            ENDDO
            SELF:Read()
            SELF:Counter += SELF:IncrStep
            SELF:Write()
            SELF:RDD:HeaderLock( DbLockMode.UnLock )

	    // Call SUPER:PutValue because we have overwritten PutValue to not allow to write,
	    // but this of course is the exception, since we HAVE to write the new autonumber field.
            SUPER:PutValue(nCurrent, buffer)
            RETURN


    END CLASS

    /// <summary>Class for reading / writing Double Columns. </summary>
    CLASS DbfDoubleColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN
        /// <inheritdoc/>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS REAL8
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            result := BitConverter.ToDouble(buffer, SELF:Offset)
            RETURN DbFloat{result,-1,-1}
        /// <inheritdoc/>
        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            VAR data := BitConverter.GetBytes((REAL8) 0)
            Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            RETURN
        /// <inheritdoc/>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL r8Value AS REAL8
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT r8Value)
                SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("Double",oValue))
                RETURN FALSE
            ENDIF
            VAR data := BitConverter.GetBytes(r8Value)
            Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            RETURN TRUE
        /// <inheritdoc/>
       OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DbFloat{0,-1,-1}
        /// <inheritdoc/>
        OVERRIDE METHOD Validate() AS LOGIC
            SELF:Decimals := 0
            RETURN SELF:Length == 8
    END CLASS

    /// <summary>Class for reading / writing Currency Columns. </summary>
    CLASS DbfCurrencyColumn INHERIT DbfColumn

        CONSTRUCTOR(oInfo AS RddFieldInfo,oRDD AS XSharp.RDD.DBF)
            SUPER(oInfo,oRDD)
            RETURN
        /// <inheritdoc/>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL tmp AS REAL8
            LOCAL result AS Decimal
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            tmp := BitConverter.ToInt64(buffer, SELF:Offset)
            result := (decimal) (tmp/ (10 ^ SELF:Decimals))
            RETURN DbCurrency{result}

        /// <inheritdoc/>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL currValue AS System.Decimal
            LOCAL i64Value AS System.Int64
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF ! SELF:GetNumber(oValue, OUT currValue)
                SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("Currency",oValue))
                RETURN FALSE
            ENDIF
            i64Value := (INT64) (currValue * (System.Decimal) (10^ SELF:Decimals))
            VAR data := BitConverter.GetBytes(i64Value)
            Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            RETURN TRUE

        /// <inheritdoc/>
        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN (Decimal) 0
        /// <inheritdoc/>
        OVERRIDE METHOD Validate() AS LOGIC
            SELF:Decimals   := 0
            SELF:Length     := 8
            RETURN TRUE
        /// <inheritdoc/>
        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            VAR blank := BYTE[]{SELF:Length}
            System.Array.Copy(blank, 0, buffer, SELF:Offset, SELF:Length)
            RETURN


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
        /// <inheritdoc/>
        OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            LOCAL result AS DateTime
            IF SELF:IsNull()
                RETURN DBNull.Value
            ENDIF
            result := DateTime.MinValue
            VAR julian  := System.BitConverter.ToInt32(buffer, SELF:Offset)
            VAR milsecs := System.BitConverter.ToInt32(buffer, SELF:Offset+4)
            IF julian == 0 .AND. milsecs == 0
                RETURN result
            ENDIF
            // blank buffer ?
            IF julian == 0x20202020 .AND. milsecs == 0x20202020
                RETURN result
            ENDIF
            IF milsecs > 0
                milsecs += 1
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
        /// <inheritdoc/>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            LOCAL empty AS LOGIC
            LOCAL dt AS DateTime
            IF oValue == NULL .or. oValue == DBNull.Value
                RETURN SELF:HandleNullValue(buffer)
            ELSEIF SELF:IsNullable
                SELF:ClearNullValue()
            ENDIF
            IF oValue IS DateTime
                dt := (DateTime) oValue
                empty := dt == DateTime.MinValue
            ELSEIF oValue IS IDate VAR oDate
                dt := DateTime{oDate:Year, oDate:Month, oDate:Day}
                empty := oDate:IsEmpty
            ELSE
                SELF:RDD:_dbfError(Subcodes.ERDD_DATATYPE, EG_DATATYPE,__ENTITY__, SELF:TypeError("DateTime",oValue))
                RETURN FALSE
            ENDIF
            IF empty
                SELF:InitValue(buffer)
                SELF:SetNullValue()
            ELSE
                VAR iJulian  := SELF:_dateTimeToJulian(dt)
                VAR milisecs := 1000 * dt:Second + dt:Minute * 1000 * 60 + dt:Hour * 1000 * 60 * 60
                IF milisecs > 0
                    milisecs -= 1
                ENDIF
                VAR data := BitConverter.GetBytes(iJulian)
                Array.Copy(data, 0, buffer, SELF:Offset, 4)
                data := BitConverter.GetBytes(milisecs)
                Array.Copy(data, 0, buffer, SELF:Offset+4, 4)
            ENDIF

            RETURN TRUE

        /// <inheritdoc/>
        OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            VAR data := BitConverter.GetBytes((LONG) 0x20202020)
            Array.Copy(data, 0, buffer, SELF:Offset, 4)
            Array.Copy(data, 0, buffer, SELF:Offset+4, 4)
            RETURN
        /// <inheritdoc/>
        OVERRIDE METHOD EmptyValue() AS OBJECT
            RETURN DateTime.MinValue
        /// <inheritdoc/>
        OVERRIDE METHOD Validate() AS LOGIC
            SELF:Decimals := 0
            SELF:Length   := 8
            RETURN TRUE

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

        /// <inheritdoc/>
       OVERRIDE METHOD InitValue(buffer AS BYTE[]) AS VOID
            SELF:bitArray:SetAll(FALSE)
            SELF:PutValue(0, buffer)
            RETURN

        /// <inheritdoc/>
       OVERRIDE METHOD GetValue(buffer AS BYTE[]) AS OBJECT
            // Read the bits from the buffer, dummy return value
            System.Array.Copy(buffer, SELF:Offset, data, 0, SELF:Length)
            bitArray:= System.Collections.BitArray{data}
            RETURN NULL

        /// <inheritdoc/>
        OVERRIDE METHOD PutValue(oValue AS OBJECT, buffer AS BYTE[]) AS LOGIC
            // Write the bits to the buffer, ignore oValue
            SELF:bitArray:CopyTo(data, 0)
            System.Array.Copy(data, 0, buffer, SELF:Offset, SELF:Length)
            RETURN FALSE
        /// <summary>Read a bit from the specified position</summary>
        METHOD GetBit(nPos AS LONG) AS LOGIC
            RETURN SELF:bitArray:Get(nPos)
        /// <summary>Set a bit at the specified position</summary>
        METHOD SetBit(nPos AS LONG, lSet AS LOGIC) AS VOID
            SELF:bitArray:Set(nPos, lSet)
            RETURN

    END CLASS

END NAMESPACE
