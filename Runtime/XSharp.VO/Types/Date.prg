//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Runtime.Serialization
USING System.Globalization
USING System.Runtime.InteropServices
USING System.Runtime.CompilerServices
USING XSharp
USING System.Diagnostics
BEGIN NAMESPACE XSharp	
	/// <summary>Internal type that implements the VO Compatible DATE type<br/>
	/// This type has many operators and implicit converters that normally are never directly called from user code.
	/// It holds year, month, day in 32 bits. For date calculations it uses the System.DateTime calculation logic. 
	/// There are implicit converters between Date and DateTime.
	/// </summary>
	/// <seealso cref="T:XSharp.IDate"/>
	/// <seealso cref="T:XSharp.RDD.DbDate"/>
	[DebuggerDisplay("{ToString(),nq}", Type := "DATE" )];
	[DebuggerTypeProxy(TYPEOF(DateDebugView))];
	[StructLayout(LayoutKind.Explicit,Pack := 1)];
	PUBLIC STRUCTURE __VODate IMPLEMENTS System.IComparable, ;
		System.IFormattable, ;
		System.IConvertible, ;
		IDate, ;
		IComparable<__VODate>, ;
		IEquatable<__VODate>
		// This structure uses an explicit layout to map
		// year, month and date into 32 bits
		// the _value field is for convenience, so we can do a single numeric comparison
		// to determine equality or relative comparisons in stead of doing 3 comparisons
		// for date calculation we use the Value PROPERTY which returns a System.DateTime type
		// Note that the Vulcan type uses a datetime which takes 8 bytes. We only use 4 bytes
		#region fields
			[FieldOffSet(00)] PRIVATE _value AS System.Int32
			[FieldOffSet(00)] PRIVATE _year  AS System.UInt16
			[FieldOffSet(02)] PRIVATE _month AS System.Byte
			[FieldOffSet(03)] PRIVATE _day   AS System.Byte
		#endregion
		
		#region STATIC fields
			/// <exclude />
			STATIC INITONLY PUBLIC  _NULL_DATE AS DATE
			/// <exclude />
			CONST  VO_MIN_DATE := 2415386U AS DWORD	// 1901-01-01
			/// <exclude />
			CONST  VO_MAX_DATE := 4606840U AS DWORD	// 7900-12-31
			/// <exclude />
			STATIC INITONLY _dtCalc AS Datetime
			
		#endregion
		
		#region datetime conversions
			/// <exclude />
			PROPERTY @@Value AS System.DateTime  GET SELF:_dtValue SET _dtValue := VALUE

			PRIVATE PROPERTY _dtValue AS System.DateTime 
				GET
					IF (_value == 0)
						RETURN System.DateTime.MinValue
					ENDIF
					RETURN System.DateTime{_year, _month, _day}
				END GET
				SET 
				IF VALUE != DateTime.MinValue
					_year  := (WORD) VALUE:Year
					_month := (BYTE) VALUE:Month
					_day   := (BYTE) VALUE:Day
				ELSE
					_value := 0
				ENDIF					
				END SET
			END PROPERTY
		#endregion
		
		#region constructors
			STATIC CONSTRUCTOR()
				_NULL_DATE  := DATE{}
				_dtCalc     := DateTime{1901,1,1}
				RETURN       
			
			/// <summary>Construct a date from a DateTime value.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			CONSTRUCTOR(lhs AS System.DateTime)
				_value := 0
				_year  := (WORD) lhs:Year
				_month := (BYTE) lhs:Month
				_day   := (BYTE) lhs:Day
				RETURN
			
			/// <summary>Construct a date from another IDate type.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			CONSTRUCTOR(d AS iDate)
				_value := 0
				_year  := (WORD) d:Year
				_month := (BYTE) d:Month
				_day   := (BYTE) d:Day
				RETURN
			
			/// <summary>Construct a date from a number of Ticks.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			CONSTRUCTOR(ticks AS INT64)
				SELF(System.DateTime{ticks})
				RETURN
			
			/// <summary>Construct a date from a string. This assumes the string is in current Date format</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			CONSTRUCTOR(strDate AS STRING)
				LOCAL dValue := CToD(strDate) AS DATE
				_value := 0
				_year  := dValue:_year
				_month := dValue:_month
				_day   := dValue:_day

			/// <summary>Construct a date from a string using the specified Date Format.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			CONSTRUCTOR(strDate AS STRING, strFormat AS STRING)
				LOCAL dValue := CToD(strDate,strFormat) AS DATE
				_value := 0
				_year  := dValue:_year
				_month := dValue:_month
				_day   := dValue:_day
			
			/// <summary>Construct a date from year, month, day </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			CONSTRUCTOR(year AS INT, month AS INT, day AS INT)
				TRY
					// this may throw an exception when the combination is not valid
					VAR lhs := System.DateTime{year, month, day}
					_value := 0
					_year  := (WORD) lhs:Year
					_month := (BYTE) lhs:Month
					_day   := (BYTE) lhs:Day
				CATCH /*e*/ AS Exception
					_value := 0 // null_date
					_year  := 0
					_month := 0
					_day   := 0
					// THROW e // cpc: VO allows invalid DATE literals, which are treated as NULL_DATE
				END TRY
				RETURN
			
			/// <summary>Construct a date from year, month, day </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			CONSTRUCTOR(year AS DWORD, month AS DWORD, day AS DWORD)
				// Chain to Int constructor
				SELF( (INT) year, (INT) month, (INT) day)
				RETURN
			
		#endregion
		
		#region methods
			/// <inheritdoc />			
			METHOD CompareTo(o AS OBJECT) AS LONG
				VAR rhs := (DATE)o 
				RETURN _value:CompareTo(rhs:_value)

			/// <inheritdoc />			
			METHOD CompareTo(rhs AS DATE) AS LONG
				RETURN _value:CompareTo(rhs:_value)

			/// <inheritdoc />	
			OVERRIDE METHOD GetHashCode() AS LONG
				RETURN _value:GetHashCode()
			
			/// <inheritdoc />	
			METHOD GetTypeCode() AS System.TypeCode
				RETURN TypeCode.DateTime
		#endregion
		#region Operators
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR ++(lhs AS DATE) AS DATE
				RETURN lhs:Add(1)

			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR +(lhs AS DATE, days AS USUAL) AS DATE
				RETURN lhs:Add(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR +(lhs AS DATE, days AS REAL8) AS DATE
				RETURN lhs:Add(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR +(lhs AS DATE, days AS LONG) AS DATE
				RETURN lhs:Add(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR +(lhs AS DATE, days AS INT64) AS DATE
				RETURN lhs:Add(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR +(lhs AS DATE, ts AS System.TimeSpan) AS DATE
				RETURN lhs:Add(ts)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR +(lhs AS DATE, days AS DWORD) AS DATE
				RETURN lhs:Add(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR +(lhs AS DATE, days AS UINT64) AS DATE
				RETURN lhs:Add(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR --(lhs AS DATE) AS DATE
				RETURN lhs:Subtract(1)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, rhs AS DATE) AS LONG
				RETURN lhs:Subtract(rhs)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, days AS USUAL) AS DATE
				RETURN lhs:Subtract(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, days AS REAL8) AS DATE
				RETURN lhs:Subtract(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, days AS LONG) AS DATE
				RETURN lhs:Subtract(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, days AS INT64) AS DATE
				RETURN lhs:Subtract(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, ts AS System.TimeSpan) AS DATE
				RETURN lhs:Subtract(ts)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, days AS DWORD) AS DATE
				RETURN lhs:Subtract(days)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			STATIC OPERATOR -(lhs AS DATE, days AS UINT64) AS DATE
				RETURN lhs:Subtract(days)
			
		#endregion
		#region Equality Operators
			
			/// <inheritdoc />	
			METHOD Equals(lhs AS DATE) AS LOGIC
				RETURN _value == lhs:_value
			
			/// <inheritdoc />	
			OVERRIDE METHOD Equals(o AS OBJECT) AS LOGIC
				IF o != NULL
					IF o:getType() == TYPEOF(DATE)
						RETURN SELF:Equals(  (DATE) o)
					ELSEIF o:getType() == TYPEOF(System.DateTime)
						RETURN SELF:Equals( DATE{ (System.DateTime) o})
					ENDIF
				ENDIF
			RETURN FALSE
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR ==(lhs AS DATE, rhs AS DATE) AS LOGIC
				RETURN (lhs:_value == rhs:_value)
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR !=(lhs AS DATE, rhs AS DATE) AS LOGIC
				RETURN (lhs:_value != rhs:_value)
			
		#endregion
		#region Implicit and Explicit converters
			
			/// <summary>This explicit converter is used in code generated by the compiler when you use a cast in your code.</summary>
			STATIC OPERATOR EXPLICIT(dvalue AS DATE) AS DWORD
				// convert to julian date like vo
				// # of days since 1-1-1901 + 2415386 
				LOCAL nDays AS DWORD
				nDays := (DWORD) (dValue:_dtValue - _dtCalc):Days + VO_MIN_DATE
				RETURN nDays

			
			/// <summary>This explicit converter is used in code generated by the compiler when you use a cast in your code.</summary>
			STATIC OPERATOR EXPLICIT(dValue AS DATE) AS INT
				LOCAL nDays AS LONG
				nDays := (dValue:_dtValue - _dtCalc):Days + VO_MIN_DATE
				RETURN nDays
			
			/// <summary>This explicit converter is used in code generated by the compiler when you use a cast in your code.</summary>
			STATIC OPERATOR EXPLICIT(dw AS DWORD ) AS DATE
				// convert julian date to our date
				LOCAL result := _NULL_DATE AS DATE
				IF dw >= VO_MIN_DATE .AND. dw <= VO_MAX_DATE
					LOCAL dt AS DateTime
					dt := _dtCalc:AddDays ( (INT) dw - VO_MIN_DATE)
					result := DATE{dt}
				ENDIF
				RETURN result
			
			/// <summary>This explicit converter is used in code generated by the compiler when you use a cast in your code.</summary>
			STATIC OPERATOR EXPLICIT(i AS INT) AS DATE
				LOCAL result := _NULL_DATE AS DATE
				IF i >= VO_MIN_DATE .AND. i <= VO_MAX_DATE
					LOCAL dt AS DateTime
					dt := _dtCalc:AddDays ( i - VO_MIN_DATE)
					result := DATE{dt}
				ENDIF
				RETURN result

			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR IMPLICIT(v AS System.DateTime) AS DATE
				RETURN DATE{v}

			/// <exclude />				
			METHOD ToDateTime() AS System.DateTime
				RETURN SELF:_dtValue
			
			/// <exclude />	
			METHOD FromDateTime(dtvalue AS System.DateTime) AS DATE
				RETURN DATE{dtvalue}
			
		#endregion
		#region Comparison Operators
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR >(lhs AS DATE, rhs AS DATE) AS LOGIC
				// note: cannot use _value for > comparison because that would depend
				//       on the processor layout 
				IF lhs:_value == rhs:_value
					RETURN FALSE
				ELSEIF lhs:_year > rhs:_year
					RETURN TRUE
				ELSEIF lhs:_year == rhs:_year 
					IF lhs:_month > rhs:_month
						RETURN TRUE
					ELSEIF lhs:_month == rhs:_month .AND. lhs:_day > rhs:_day
						RETURN TRUE
					ENDIF
				ENDIF
				RETURN FALSE
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR >=(lhs AS DATE, rhs AS DATE) AS LOGIC
				// note: cannot use _value for > comparison because that would depend
				//       on the processor layout 
				IF lhs:_value == rhs:_value
					RETURN TRUE
				ELSEIF lhs:_year > rhs:_year
					RETURN TRUE
				ELSEIF lhs:_year == rhs:_year 
					IF lhs:_month > rhs:_month
						RETURN TRUE
					ELSEIF lhs:_month == rhs:_month .AND. lhs:_day >= rhs:_day
						RETURN TRUE
					ENDIF
				ENDIF
				RETURN FALSE
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR IMPLICIT(v AS DATE) AS System.DateTime
				RETURN v:_dtValue
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR <(lhs AS DATE, rhs AS DATE) AS LOGIC
				// note: cannot use _value for < comparison because that would depend
				//       on the processor layout 
				IF lhs:_value == rhs:_value
					RETURN FALSE
				ELSEIF lhs:_year < rhs:_year
					RETURN TRUE
				ELSEIF lhs:_year == rhs:_year 
					IF lhs:_month < rhs:_month
						RETURN TRUE
					ELSEIF lhs:_month == rhs:_month .AND. lhs:_day < rhs:_day
						RETURN TRUE
					ENDIF
				ENDIF
				RETURN FALSE
			
			/// <summary>This operator is used in code generated by the compiler when needed.</summary>
			STATIC OPERATOR <=(lhs AS DATE, rhs AS DATE) AS LOGIC
				// note: cannot use _value for < comparison because that would depend
				//       on the processor layout 
				IF lhs:_value == rhs:_value
					RETURN TRUE
				ELSEIF lhs:_year < rhs:_year
					RETURN TRUE
				ELSEIF lhs:_year == rhs:_year 
					IF lhs:_month < rhs:_month
						RETURN TRUE 
					ELSEIF lhs:_month == rhs:_month .AND. lhs:_day <= rhs:_day
						RETURN TRUE
					ENDIF
				ENDIF
				RETURN FALSE 
		#endregion
		
		#region ADD and Subtract Methods
			/// <exclude />	
			METHOD ADD(days AS USUAL) AS DATE
				IF days:IsLong
					RETURN SELF:Add( (LONG) days)
				ELSEIF days:IsFloat
					 RETURN SELF:Add( (REAL8) days)
				ELSE
					THROW Error.ArgumentError(__ENTITY__,NAMEOF(days), "Incompatible argument for Date:Add()")
				ENDIF
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD ADD(days AS REAL8) AS DATE
				VAR res := SELF:_dtValue:AddDays(days)
				RETURN DATE{res}
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD ADD(days AS LONG) AS DATE
				VAR res := SELF:_dtValue:AddDays(days)
				RETURN DATE{res}
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD ADD(days AS INT64) AS DATE
				VAR res := SELF:_dtValue:AddDays(days)
				RETURN DATE{res}
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD ADD(span AS System.TimeSpan) AS DATE
				VAR res := SELF:_dtValue:Add(span)
				RETURN DATE{res}
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD ADD(days AS DWORD) AS DATE
				VAR res := SELF:_dtValue:AddDays(days)
				RETURN DATE{res}
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD ADD(days AS UINT64) AS DATE
				VAR res := SELF:_dtValue:AddDays(days)
				RETURN DATE{res}
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(lhs AS DATE) AS LONG
				LOCAL span AS System.TimeSpan
				span := (System.TimeSpan)(SELF:_dtValue - lhs:_dtValue) 
				RETURN span:Days
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(days AS USUAL) AS DATE
				IF days:IsLong
					RETURN SELF:Add( -(LONG) days)
				ELSEIF days:IsFloat
					 RETURN SELF:Add( -(REAL8) days)
				ELSE
					THROW Error.ArgumentError(__ENTITY__,NAMEOF(days), "Incompatible argument for Date:Subtract()")
				ENDIF
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(days AS REAL8) AS DATE
				RETURN SELF:Add(-days)
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(days AS LONG) AS DATE
				RETURN SELF:Add(-days)
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(days AS INT64) AS DATE
				RETURN SELF:Add(-days)
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(ts AS System.TimeSpan) AS DATE
				RETURN SELF:Add(-ts)
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(days AS DWORD) AS DATE
				RETURN SELF:Add(-days)
			
			/// <exclude />	
            [MethodImpl(MethodImplOptions.AggressiveInlining)];        
			METHOD Subtract(days AS UINT64) AS DATE
				RETURN SELF:Add(-(INT64)days)
		#endregion
		 
		#region IConvertable 
			// forward most methods to the DateTime class so there will
			// be a proper (localized) error message
			/// <inheritdoc />
			METHOD IConvertible.ToBoolean(provider AS System.IFormatProvider) AS LOGIC
				RETURN ((IConvertible)VALUE):ToBoolean(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToByte(provider AS System.IFormatProvider) AS BYTE
				RETURN ((IConvertible)VALUE):ToByte(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToChar(provider AS System.IFormatProvider) AS CHAR
				RETURN ((IConvertible)VALUE):ToChar(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToDateTime(provider AS System.IFormatProvider) AS System.DateTime
				RETURN VALUE
			
			/// <inheritdoc />
			METHOD IConvertible.ToDecimal(provider AS System.IFormatProvider) AS Decimal
				RETURN ((IConvertible)VALUE):ToDecimal(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToDouble(provider AS System.IFormatProvider) AS REAL8
				RETURN ((IConvertible)VALUE):ToDouble(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToInt16(provider AS System.IFormatProvider) AS SHORT
				RETURN ((IConvertible)VALUE):ToInt16(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToInt32(provider AS System.IFormatProvider) AS LONG
				RETURN ((IConvertible)VALUE):ToInt32(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToInt64(provider AS System.IFormatProvider) AS INT64
				RETURN ((IConvertible)VALUE):ToInt64(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToSByte(provider AS System.IFormatProvider) AS SByte
				RETURN ((IConvertible)VALUE):ToSByte(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToSingle(provider AS System.IFormatProvider) AS REAL4
				RETURN ((IConvertible)VALUE):ToSingle(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToType(conversionType AS System.Type, provider AS System.IFormatProvider) AS OBJECT
				IF conversionType == TYPEOF(DATE)
					RETURN SELF
				ELSEIF conversionType == TYPEOF(System.DateTime)
					RETURN SELF:_dtValue
				ENDIF
				RETURN ((IConvertible)VALUE):ToType(conversionType, provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToUInt16(provider AS System.IFormatProvider) AS WORD
				RETURN ((IConvertible)VALUE):ToUInt16(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToUInt32(provider AS System.IFormatProvider) AS DWORD
				RETURN ((IConvertible)VALUE):ToUInt32(provider)
			
			/// <inheritdoc />
			METHOD IConvertible.ToUInt64(provider AS System.IFormatProvider) AS UINT64
				RETURN ((IConvertible)VALUE):ToUInt64(provider)
		#endregion
		
		#region ToString()
			// Use DateTime ToString) methods as helpers
			INTERNAL STATIC PROPERTY _NullDateString AS STRING GET RuntimeState.GetValue<STRING>(Set.DateFormatEmpty)
			/// <inheritdoc />
			OVERRIDE METHOD ToString() AS STRING
				IF (_value == 0)
					RETURN _NullDateString
				ENDIF
			RETURN DToC(SELF)
			/// <inheritdoc />
			METHOD ToString(provider AS System.IFormatProvider) AS STRING
				IF (_value == 0)
					RETURN _NullDateString
				ENDIF
				RETURN VALUE:ToString(provider)
			/// <inheritdoc />
			METHOD ToString(s AS STRING) AS STRING
				IF (_value == 0)
					RETURN _NullDateString
				ENDIF
				RETURN VALUE:ToString(s)
			/// <inheritdoc />
			METHOD ToString(s AS STRING, fp AS System.IFormatProvider) AS STRING
				IF (_value == 0)
					RETURN _NullDateString
				ENDIF
				IF (s == NULL)
					s := XSharp.RuntimeState.DateFormat
				ENDIF
				RETURN VALUE:ToString(s, fp)
		#endregion
		#region properties
			
			/// <inheritdoc />
			PROPERTY IsEmpty AS LOGIC
				GET
					RETURN _value == 0
				END GET
			END PROPERTY
			/// <inheritdoc />
			PROPERTY Month	AS INT GET _month 
			/// <inheritdoc />
			PROPERTY Year	AS INT GET _year 
			/// <inheritdoc />
			PROPERTY Day	AS INT GET _day 
			// Next properties for easy access in right type
			INTERNAL PROPERTY DYear		AS DWORD GET _year
			INTERNAL PROPERTY DMonth	AS DWORD GET _month
			INTERNAL PROPERTY DDay		AS DWORD GET _day
			
			INTERNAL CLASS DateDebugView
				PRIVATE _value AS DATE
				PUBLIC CONSTRUCTOR (d AS DATE)
					_value := d
				
				PUBLIC PROPERTY Year	AS INT GET _value:Year
				PUBLIC PROPERTY Month	AS INT GET _value:Month
				PUBLIC PROPERTY Day		AS INT GET _value:Day
				
			END CLASS
			
		#endregion
		
		#region STATIC Properties
			
			/// <exclude />	
			STATIC METHOD NullDate AS DATE 
				RETURN  _NULL_DATE
			
			
		#endregion
	END STRUCTURE
END NAMESPACE
