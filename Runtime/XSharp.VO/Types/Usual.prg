//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.InteropServices
using System.Diagnostics
using XSharp.Internal
BEGIN NAMESPACE XSharp
	[StructLayout(LayoutKind.Sequential)];
	[DebuggerDisplay("{Value} ({UsualType})", Type := "USUAL")];
	[DebuggerTypeProxy(typeof(UsualDebugView))];
	PUBLIC STRUCTURE __Usual IMPLEMENTS IConvertible,IComparable
		#region static fields
		PUBLIC STATIC _NIL as __Usual
		#endregion

		#region private fields
		PRIVATE INITONLY _flags    	AS __Usual_flags	// type, byref, width, decimals
		PRIVATE INITONLY _valueData	as __UsualData		// for non GC data
		PRIVATE INITONLY _refData  	as Object			// for GC data
		#endregion

		#region constructors
		STATIC Constructor
			_NIL := __Usual{}
		return
	

		private constructor(u as __Usual)
			SELF:_flags     := u:_flags
			SELF:_valueData	:= u:_valueData
			SELF:_refData 	:= u:_refData 

		return

		private constructor(f as __VoFloat)
			SELF:_valueData:r8		:= f:Value
			SELF:_flags:usualType	:= __UsualType.Float
			SELF:_flags:Width		:= (Sbyte) f:Digits
			SELF:_flags:Decimals	:= (Sbyte) f:Decimals

		return

		PRIVATE CONSTRUCTOR(r8 AS Real8)
			SELF:_valueData:r8		:= r8
			SELF:_flags:usualType	:= __UsualType.Float
			SELF:_flags:Width		:= -1
			SELF:_flags:Decimals	:= -1

		return

		private constructor(value as Logic)
			SELF:_flags:usualType	:= __UsualType.LOGIC
			SELF:_valueData:l		:= value
		return

		private constructor(value as __Array)
			SELF:_flags:usualType	:= __UsualType.Array
			SELF:_refData			:= value
		return

		private constructor(value as __VoDate)
			SELF:_flags:usualType	:= __UsualType.Date
			SELF:_valueData:d		:= value
		return

		private constructor(value as System.DateTime)
		SELF:_flags:usualType	:= __UsualType.DateTime
		SELF:_valueData:dt		:= value

		return

		private constructor(value as Long)
			SELF:_flags:usualType	:= __UsualType.LONG
			_valueData:i			:= value
		return

		private constructor(value as Int64)
			SELF:_flags:usualType	:= __UsualType.INT64
			SELF:_valueData:i64		:= value
		return

		private constructor(value as UInt64)
			if value < Int64.MaxValue
				SELF:_flags:usualType	:= __UsualType.INT64
				SELF:_valueData:i64:= (int64) value
			else
				SELF:_flags:usualType	:= __UsualType.FLOAT
				SELF:_valueData:r8 := value
			endif
		return

		PRIVATE CONSTRUCTOR(d AS System.Decimal)
			SELF:_flags:usualType  := __UsualType.Decimal
			SELF:_refdata	:= d


		PRIVATE CONSTRUCTOR(value AS System.IntPtr)
			SELF:_flags:usualType	:= __UsualType.PTR
			SELF:_valueData:p		:= value
		return

		public constructor(o as Object)
			local u				as __Usual
			if o != null
				if o:GetType() == typeof(__Usual)
					// boxed __Usual
					u		:= (__Usual)o 
					SELF:_flags		:= u:_flags
					SELF:_refData	:= u:_refData 
					SELF:_valueData	:= u:_valueData
				else
					//  decode type from typecode
					VAR vartype := o:GetType()
					VAR typeCode := System.Type.GetTypeCode(vartype)
					switch typeCode
					CASE  System.TypeCode.DBNull
						SELF:_flags:usualType := __UsualType.Void
						SELF:_refData	:= null

					CASE System.TypeCode.Boolean
						SELF:_flags:usualType := __UsualType.LOGIC
						SELF:_valueData:l := (Logic)o 

					CASE System.TypeCode.Char
						SELF:_flags:usualType		:= __UsualType.Long
						SELF:_valueData:i	:= (Char)o 

					CASE System.TypeCode.SByte
						SELF:_flags:usualType		:= __UsualType.Long
						SELF:_valueData:i	:= (SByte)o 

					CASE System.TypeCode.Byte
						SELF:_flags:usualType		:= __UsualType.Long
						SELF:_valueData:i	:= (Byte)o 

					CASE System.TypeCode.Int16 
						SELF:_flags:usualType		:= __UsualType.Long
						SELF:_valueData:i	:= (Short)o 

					CASE System.TypeCode.UInt16
						SELF:_flags:usualType		:= __UsualType.Long
						SELF:_valueData:i	:= (Word)o 

					CASE System.TypeCode.Int32
						SELF:_flags:usualType		:= __UsualType.Long
						SELF:_valueData:i	:= (Long)o 

					CASE System.TypeCode.UInt32
						if (DWord)o  <= Int32.MaxValue
							SELF:_flags:usualType := __UsualType.Long
							SELF:_valueData:i := (Long)(DWord)o  
						else
							SELF:_flags:usualType := __UsualType.Float
							SELF:_valueData:r8:= (REAL8) (UInt32) o 
							SELF:_flags:width	:= -1
							SELF:_flags:decimals := -1
						endif
					CASE System.TypeCode.Int64 
						SELF:_flags:usualType		:= __UsualType.Int64
						SELF:_valueData:i64	:= (Int64)o 

					CASE System.TypeCode.UInt64 
						IF (Uint64) o  <= Int64.MaxValue
							SELF:_flags:usualType	:= __UsualType.Int64
							SELF:_valueData:i64		:= (Int64)(UInt64)o  
						ELSE
							SELF:_flags:usualType := __UsualType.FLOAT
							SELF:_valueData:r8 := (Real8)(UInt64)o  
							SELF:_flags:width	:= -1
							SELF:_flags:decimals := -1
						ENDIF
					CASE System.TypeCode.Single  
						SELF:_flags:usualType		:= __UsualType.Float
						SELF:_valueData:r8	:= (real8)o 
						SELF:_flags:width	:= -1
						SELF:_flags:decimals := -1
					
					CASE System.TypeCode.Double 
						SELF:_flags:usualType := __UsualType.Float
						SELF:_valueData:r8 := (real8)o 
						SELF:_flags:width := -1
						SELF:_flags:decimals := -1
					
					CASE System.TypeCode.Decimal 
						SELF:_flags:usualType := __UsualType.Decimal
						SELF:_refData  := o

					CASE System.TypeCode.DateTime 
						SELF:_flags:usualType := __UsualType.DateTime
						SELF:_valueData:dt := (System.DateTime) o 

					CASE System.TypeCode.String 
						SELF:_flags:usualType := __UsualType.STRING
						SELF:_refData  := (STRING)o 

					OTHERWISE
						if vartype == typeof(__Array)
							SELF:_flags:usualType := __UsualType.Array
							SELF:_refData  := o
						elseif vartype == typeof(__VODate)
							SELF:_flags:usualType := __UsualType.Date
							SELF:_valueData:d :=  (__VoDate) o
						elseif vartype == typeof(__Symbol)
							SELF:_flags:usualType := __UsualType.Symbol
							SELF:_valueData:s :=  (__Symbol) o
						endif
					end switch
				endif
			endif
		return

		private constructor(s as string)
			SELF:_flags:usualType	:= __UsualType.STRING
			SELF:_refData 			:= s
		return
		#endregion

		#region properties
		PRIVATE PROPERTY _usualType		AS __UsualType	GET _flags:usualType 
		PRIVATE PROPERTY _decimalValue	as System.Decimal GET (System.Decimal) _refData 
		PRIVATE PROPERTY _i64Value		as Int64	GET _valueData:i64 
		PRIVATE PROPERTY _r8Value		as REAL8	GET _valueData:r8 
		PRIVATE PROPERTY _dateValue		as __VoDate GET _valueData:d 
		PRIVATE PROPERTY _dateTimeValue as DateTime GET _valueData:dt
		PRIVATE PROPERTY _intValue		as INT		GET _valueData:i  
		PRIVATE PROPERTY _symValue		as __Symbol	GET _valueData:s 
		PRIVATE PROPERTY _logicValue	as LOGIC	GET _valueData:l 
		PRIVATE PROPERTY _ptrValue		as IntPtr	GET _valueData:p 
		PRIVATE PROPERTY _stringValue   as STRING	GET IIF(UsualType == __UsualType.String, (String) _refData , String.Empty)
		PRIVATE PROPERTY _floatValue    as __VoFloat GET __VoFloat{ _valueData:r8, _width, _decimals}
		// properties for floats
		PRIVATE PROPERTY _width			AS SBYTE GET _flags:width 
		PRIVATE PROPERTY _decimals		AS SBYTE GET _flags:decimals 
 		INTERNAL PROPERTY UsualType		as __UsualType GET  _usualType

		PRIVATE PROPERTY isReferenceType as LOGIC
			GET
				switch _usualType
				CASE __UsualType.Array
				CASE __UsualType.Object
				CASE __UsualType.Decimal
				CASE __UsualType.String
					return true
				otherwise
					return false
				end switch
			END GET
		end property

		INTERNAL PROPERTY IsNil as Logic
			Get
				return SELF:usualType == __UsualType.Void .or. ;
				(SELF:IsReferenceType .and. SELF:_refData  == null)

			End Get
		end property
		#endregion
		#region Properties for the Debugger
		PROPERTY Value as OBJECT 
			GET
				SWITCH UsualType
				CASE __UsualType.Array		; RETURN (__Array) _refData
				CASE __UsualType.Date		; RETURN _dateValue
				CASE __UsualType.DateTime	; RETURN _dateTimeValue
				CASE __UsualType.Decimal	; RETURN _decimalValue
				CASE __UsualType.Float		; RETURN _r8Value
				CASE __UsualType.Int64		; RETURN _i64Value
				CASE __UsualType.Long		; RETURN _intValue
				CASE __UsualType.Logic		; RETURN _logicValue
				CASE __UsualType.Ptr		; RETURN _ptrValue
				CASE __UsualType.Symbol		; RETURN _symValue
				CASE __UsualType.String		; RETURN _stringValue
				CASE __UsualType.Void		; RETURN "NIL"
				CASE __UsualType.Object
				OTHERWISE					; RETURN _refData
			END SWITCH
			END GET
		END PROPERTY

		#endregion


		#region implementation IComparable
		public method CompareTo(o as Object) as Long
			local rhs as __Usual
			rhs := (__Usual) o
			if SELF:UsualType == rhs:UsualType
				// Compare ValueTypes
				switch UsualType
				case __UsualType.Date		; return SELF:_dateValue:CompareTo(rhs:_dateValue)
				case __UsualType.DateTime	; return SELF:_dateTimeValue:CompareTo(rhs:_dateTimeValue)
				case __UsualType.Decimal	; return SELF:_decimalValue:CompareTo(rhs:_decimalValue)
				case __UsualType.Int64		; return SELF:_i64Value:CompareTo(rhs:_i64Value)
				case __UsualType.Logic		; return SELF:_logicValue:CompareTo(rhs:_logicValue)
				case __UsualType.Long		; return SELF:_intValue:CompareTo(rhs:_intValue)
				case __UsualType.Ptr		; return SELF:_ptrValue:ToInt64():CompareTo(rhs:_ptrValue:ToInt64())
				// Uses String Comparison rules
				// Vulcan does a case insensitive comparison ?
				case __UsualType.String		; return String.Compare( _stringValue,  rhs:_stringValue)
				case __UsualType.Symbol		; return String.Compare( (string) SELF:_symValue, (string) rhs:_symValue)
				otherwise					; return 0
				END SWITCH
			ELSE
				// Type of LHS different from type of RHS
				SWITCH SELF:UsualType
				CASE __UsualType.Void
					RETURN -1
				CASE __UsualType.Date
					// Upscale when needed to avoid overflow errors
					SWITCH rhs:UsualType
					case __UsualType.DateTime	; return _dateValue.CompareTo((__VoDate) rhs:_dateTimeValue)
					case __UsualType.Decimal	; return ((System.Decimal) (int) _dateValue).CompareTo(rhs:_decimalValue)
					case __UsualType.Float		; return ((real8) (int) _dateValue).CompareTo(rhs:_r8Value)
					CASE __UsualType.Int64		; return ( (int64) (int) _dateValue).CompareTo(rhs:_i64Value)
					case __UsualType.Long		; return ((int) _dateValue).CompareTo(rhs:_intValue)
					otherwise
						nop	// uses comparison by type
					end switch

				CASE __UsualType.Float
					switch rhs:UsualType
					case __UsualType.Date		; return _r8Value.CompareTo( (real8) (int) rhs:_dateValue)
					case __UsualType.Decimal	; return _r8Value.CompareTo( (real8) rhs:_decimalValue)
					case __UsualType.Long		; return _r8Value.CompareTo( (real8) rhs:_intValue)
					case __UsualType.Int64		; return _r8Value.CompareTo( (real8) rhs:_i64Value)
					otherwise
						nop	// uses comparison by type
					end switch

				CASE __UsualType.Long
					// Upscale when needed to avoid overflow errors
					switch rhs:UsualType
					case __UsualType.Date		; return _intValue.CompareTo((Int) rhs:_dateValue)
					case __UsualType.Int64		; return ((int64)_intValue).CompareTo(rhs:_i64Value)
					case __UsualType.Float		; return ((real8)_intValue).CompareTo(rhs:_r8Value)
					case __UsualType.Decimal	; return ((System.Decimal)_intValue).CompareTo(rhs:_decimalValue)
					otherwise
						nop	// uses comparison by type
					END SWITCH

				CASE __UsualType.Int64
					switch rhs:UsualType
					case __UsualType.Date		; return _i64Value.CompareTo((int) rhs:_dateValue)
					case __UsualType.Long		; return _i64Value.CompareTo( rhs:_intValue)
					case __UsualType.Float		; return _i64Value.CompareTo( rhs:_r8Value)
					case __UsualType.Decimal	; return _i64Value.CompareTo( rhs:_decimalValue)
					otherwise
						nop	// uses comparison by type
					end switch

				CASE __UsualType.Decimal
					switch rhs:UsualType
					case __UsualType.Date		; return _decimalValue.CompareTo((int) rhs:_dateValue)
					case __UsualType.Long		; return _decimalValue.CompareTo(rhs:_intValue)
					case __UsualType.Float		; return _decimalValue.CompareTo(rhs:_r8Value)
					case __UsualType.Int64		; return _decimalValue.CompareTo(rhs:_i64Value)
					otherwise
						nop	// uses comparison by type
					end switch
				END SWITCH 
			ENDIF
			IF rhs:UsualType == __UsualType.Void
				return 1
			ELSEif self:UsualType > rhs:UsualType
				return 1
			elseif self:UsualType < rhs:UsualType
				return -1
			endif
			return 0
		


		#endregion
				
		#region Comparison Operators 
		static operator >(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_intValue > rhs:_intValue
				case __UsualType.Int64		; return lhs:_intValue > rhs:_i64Value
				case __UsualType.Float		; return lhs:_intValue > rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_intValue > rhs:_decimalValue
				otherwise
					throw BinaryError(">", "Argument not numeric", false, lhs, rhs)
				end switch
			
			CASE __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_i64Value > rhs:_intValue
				case __UsualType.Int64		; return lhs:_i64Value > rhs:_i64Value
				case __UsualType.Float		; return lhs:_i64Value > rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_i64Value > rhs:_decimalValue
				otherwise
					throw BinaryError(">", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_r8Value > rhs:_intValue
				case __UsualType.Int64		; return lhs:_r8Value > rhs:_i64Value
				case __UsualType.Float		; return lhs:_r8Value > rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_r8Value > (REAL8) rhs:_decimalValue
				otherwise
					throw BinaryError(">", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.Decimal
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_decimalValue > rhs:_intValue
				case __UsualType.Int64		; return lhs:_decimalValue > rhs:_i64Value
				case __UsualType.Float		; return lhs:_decimalValue > (System.Decimal) rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_decimalValue >  rhs:_decimalValue
				otherwise
					throw BinaryError(">", "Argument not numeric", false, lhs, rhs)
				end switch

			CASE __UsualType.String
				if rhs:UsualType == __UsualType.String
					return lhs:_stringValue> rhs:_stringValue
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_symValue > rhs:_symValue
				else
					nop
				endif
			case __UsualType.Date
				switch (rhs:UsualType)
				case __UsualType.Date		; return lhs:_dateValue > rhs:_dateValue
				case __UsualType.DateTime	; return lhs:_dateValue > (__VoDate) rhs:_dateTimeValue
				otherwise
					nop
				end switch
			case __UsualType.DateTime
				switch (rhs:UsualType)
				case __UsualType.DateTime	; return lhs:_dateTimeValue > rhs:_dateTimeValue
				case __UsualType.Date		; return lhs:_dateTimeValue > (DateTime) rhs:_dateValue
				otherwise
					nop
				end switch
			otherwise
				nop
			end switch
			throw BinaryError(">", "Incompatible Arguments", false, lhs, rhs)

		static operator >=(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType	
				CASE __UsualType.Long		; return lhs:_intValue >= rhs:_intValue
				case __UsualType.Int64		; return lhs:_intValue >= rhs:_i64Value
				case __UsualType.Float		; return lhs:_intValue >= rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_intValue >= rhs:_decimalValue
				otherwise
					throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_i64Value >= rhs:_intValue
				case __UsualType.Int64		; return lhs:_i64Value >= rhs:_i64Value
				case __UsualType.Float		; return lhs:_i64Value >= rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_i64Value >= rhs:_decimalValue
				otherwise
					throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_r8Value >= rhs:_intValue
				case __UsualType.Int64		; return lhs:_r8Value >= rhs:_i64Value
				case __UsualType.Float		; return lhs:_r8Value >= rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_r8Value >= (REAL8) rhs:_decimalValue
				otherwise
					throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
				end switch

			CASE __UsualType.Decimal
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_decimalValue >= rhs:_intValue
				case __UsualType.Int64		; return lhs:_decimalValue >= rhs:_i64Value
				case __UsualType.Float		; return lhs:_decimalValue >= (System.Decimal) rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_decimalValue >=  rhs:_decimalValue
				otherwise
					throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.String
				if rhs:UsualType == __UsualType.String
					return lhs:_stringValue>= rhs:_stringValue
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_symValue >= rhs:_symValue
				else
					nop
				endif
			case __UsualType.Date
				switch (rhs:UsualType)
				case __UsualType.Date		; return lhs:_dateValue		>= rhs:_dateValue
				case __UsualType.DateTime	; return lhs:_dateTimeValue >= (__VoDate) rhs:_dateTimeValue
				otherwise
					nop
				end switch
			case __UsualType.DateTime
				switch (rhs:UsualType)
				case __UsualType.Date		; return lhs:_dateValue		>= (DateTime) rhs:_dateValue
				case __UsualType.DateTime	; return lhs:_dateTimeValue >=  rhs:_dateTimeValue
				otherwise
					nop
				end switch
			otherwise
				throw BinaryError(">=", "Incompatible Arguments", true, lhs, rhs)
			end switch
			throw BinaryError(">=", "Incompatible Arguments", false, lhs, rhs)

		static operator <(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_intValue < rhs:_intValue
				case __UsualType.Int64		; return lhs:_intValue < rhs:_i64Value
				case __UsualType.Float		; return lhs:_intValue < rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_intValue < rhs:_decimalValue
				otherwise
					throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_i64Value < rhs:_intValue
				case __UsualType.Int64		; return lhs:_i64Value < rhs:_i64Value
				case __UsualType.Float		; return lhs:_i64Value < rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_i64Value < rhs:_decimalValue
				otherwise
					throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_r8Value < rhs:_intValue
				case __UsualType.Int64		; return lhs:_r8Value < rhs:_i64Value
				case __UsualType.Float		; return lhs:_r8Value < rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_r8Value < (REAL8) rhs:_decimalValue
				otherwise
					throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
				end switch

			CASE __UsualType.Decimal
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_decimalValue < rhs:_intValue
				case __UsualType.Int64		; return lhs:_decimalValue < rhs:_i64Value
				case __UsualType.Float		; return lhs:_decimalValue < (System.Decimal) rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_decimalValue <  rhs:_decimalValue
				otherwise
					throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.String
				if rhs:UsualType == __UsualType.String
					return lhs:_stringValue< rhs:_stringValue
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_symValue < rhs:_symValue
				else
					nop
				endif
			case __UsualType.Date
				switch (rhs:UsualType)
				case __UsualType.Date		; return lhs:_dateValue	< rhs:_dateValue
				case __UsualType.DateTime	; return lhs:_dateValue < (__VoDate) rhs:_dateTimeValue
				otherwise
					nop
				end switch
			case __UsualType.DateTime
				switch (rhs:UsualType)
				case __UsualType.Date		; return lhs:_dateValue		< (DateTime) rhs:_dateValue
				case __UsualType.DateTime	; return lhs:_dateTimeValue <  rhs:_dateTimeValue
				otherwise
					nop
				end switch
			otherwise
				throw BinaryError("<", "Incompatible Arguments", true, lhs, rhs)
			end switch
			throw BinaryError("<", "Incompatible Arguments", false, lhs, rhs)

		static operator <=(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_intValue <= rhs:_intValue
				case __UsualType.Int64		; return lhs:_intValue <= rhs:_i64Value
				case __UsualType.Float		; return lhs:_intValue <= rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_intValue <= rhs:_decimalValue
				otherwise
					throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_i64Value <= rhs:_intValue
				case __UsualType.Int64		; return lhs:_i64Value <= rhs:_i64Value
				case __UsualType.Float		; return lhs:_i64Value <= rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_i64Value <= rhs:_decimalValue
				otherwise
					throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_r8Value <= rhs:_intValue
				case __UsualType.Int64		; return lhs:_r8Value <= rhs:_i64Value
				case __UsualType.Float		; return lhs:_r8Value <= rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_r8Value <= (REAL8) rhs:_decimalValue
				otherwise
					throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
				end switch

			CASE __UsualType.Decimal
				switch rhs:UsualType
				case __UsualType.Long		; return lhs:_decimalValue <= rhs:_intValue
				case __UsualType.Int64		; return lhs:_decimalValue <= rhs:_i64Value
				case __UsualType.Float		; return lhs:_decimalValue <= (System.Decimal) rhs:_r8Value
				case __UsualType.Decimal	; return lhs:_decimalValue <=  rhs:_decimalValue
				otherwise
					throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.String
				if rhs:UsualType == __UsualType.String
					return  lhs:_stringValue<= rhs:_stringValue
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_symValue <= rhs:_symValue
				else
					nop
				endif
			case __UsualType.Date
				switch (rhs:UsualType)
				case __UsualType.Date		; return lhs:_dateValue	<= rhs:_dateValue
				case __UsualType.DateTime	; return lhs:_dateValue <= (__VoDate) rhs:_dateTimeValue
				otherwise
					nop
				end switch
			case __UsualType.DateTime
				switch (rhs:UsualType)
				case __UsualType.Date		; return lhs:_dateValue		<= (DateTime) rhs:_dateValue
				case __UsualType.DateTime	; return lhs:_dateTimeValue <=  rhs:_dateTimeValue
				otherwise
					nop
				end switch
			otherwise
				throw BinaryError("<=", "Incompatible Arguments", true, lhs, rhs)
			end switch
			throw BinaryError("<=", "Incompatible Arguments", false, lhs, rhs)
#endregion

#region Operators for Equality
		public method Equals(obj as object) as logic
			return super:Equals(obj)

		public method GetHashCode() as int
			return super:GetHashCode()

		static operator ==(lhs as __Usual, rhs as __Usual) as Logic
			return lhs:UsualEquals(rhs, "==")

		static operator !=(lhs as __Usual, rhs as __Usual) as Logic
			if lhs:UsualType == __UsualType.STRING .and. rhs:UsualType == __UsualType.STRING
				// Todo __StringEquals
				return ! String.Equals(  lhs:_stringValue, rhs:_stringValue)
			else
				return ! lhs:UsualEquals(rhs, "!=")
			endif

		method UsualEquals( rhs as __Usual, operator as STRING) as LOGIC
			switch self:UsualType
			case __UsualType.Object
				if rhs:UsualType == __UsualType.Object
					return _refData == rhs:_refData
				else
					nop
				endif
			
			CASE __UsualType.Void
				return rhs:UsualType == __UsualType.Void

			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long		; return _intValue == rhs:_intValue
				case __UsualType.Int64		; return (int64) _intValue == rhs:_i64Value	// cast lhs to int64 to avoid overflow 
				case __UsualType.Float		; return (real8) _intValue == rhs:_r8Value // cast lhs to real8 to avoid overflow 
				case __UsualType.Decimal	; return (System.Decimal) _intValue == rhs:_decimalValue	// cast lhs to decimal to avoid overflow 
				case __UsualType.Logic		; return rhs:_logicValue == (SELF:_intValue <> 0)
				otherwise
					nop
				end switch

			CASE __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long		; return _i64Value == (int64) rhs:_intValue
				case __UsualType.Int64		; return _i64Value == rhs:_i64Value
				case __UsualType.Float		; return  (real8) _i64Value == rhs:_r8Value
				case __UsualType.Decimal	; return _i64Value == rhs:_decimalValue
				case __UsualType.Logic		; return rhs:_logicValue == (SELF:_i64Value <> 0)
				otherwise
					nop
				end switch

			CASE __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long		; return self:_r8Value == (REAL8) rhs:_intValue
				case __UsualType.Int64		; return self:_r8Value == (REAL8) rhs:_i64Value
				case __UsualType.Float		; return self:_r8Value ==  rhs:_r8Value
				case __UsualType.Decimal	; return self:_r8Value ==  (REAL8) rhs:_decimalValue
				otherwise
					nop
				end switch

			CASE __UsualType.Decimal
				switch rhs:UsualType
				case __UsualType.Long		; return self:_decimalValue == rhs:_intValue
				case __UsualType.Int64		; return self:_decimalValue == rhs:_i64Value
				case __UsualType.Float		; return self:_decimalValue == (System.Decimal) rhs:_r8Value
				case __UsualType.Decimal	; return self:_decimalValue == rhs:_decimalValue
				otherwise
					nop
				end switch

			CASE __UsualType.LOGIC
				switch rhs:UsualType
				case __UsualType.LOGIC		; return SELF:_logicValue == rhs:_logicValue
				case __UsualType.Long		; return SELF:_logicValue == (rhs:_intValue <> 0)
				case __UsualType.Int64		; return SELF:_logicValue == (rhs:_i64Value <> 0)
				case __UsualType.Decimal	; return SELF:_logicValue == (rhs:_decimalValue <> 0)
				otherwise
					nop
				end switch

			CASE __UsualType.DATE
				switch rhs:UsualType
				case __UsualType.DATE		; return SELF:_dateValue == rhs:_dateValue
				case __UsualType.DateTime	; return SELF:_dateValue == (__VoDate) rhs:_dateTimeValue
				otherwise
					nop
				end switch

			CASE __UsualType.DateTime
				switch rhs:UsualType
				case __UsualType.DateTime	; return SELF:_dateTimeValue == rhs:_dateTimeValue
				case __UsualType.DATE		; return SELF:_dateTimeValue == (DateTime) rhs:_dateValue
				otherwise
					nop
				end switch

			CASE __UsualType.STRING
				switch rhs:UsualType
				case __UsualType.STRING		; return SELF:_stringValue== rhs:_stringValue
				case __UsualType.Symbol		; return SELF:_stringValue == rhs:_symValue
				otherwise
					nop
				end switch

			CASE __UsualType.ARRAY
				switch rhs:UsualType
				case __UsualType.ARRAY		; return SELF:_refData == rhs:_refData
				otherwise
					nop
				end switch

			CASE __UsualType.CodeBlock
				switch rhs:UsualType
				case __UsualType.CodeBlock	; return SELF:_refData == rhs:_refData
				otherwise
					nop
				end switch

			CASE __UsualType.Ptr
				switch rhs:UsualType
				case __UsualType.Ptr		; return SELF:_ptrValue == rhs:_ptrValue
				otherwise
					nop
				end switch

			CASE __UsualType.Symbol
				switch rhs:UsualType
				case __UsualType.Symbol		; return SELF:_symValue == rhs:_symValue
				case __UsualType.String		; return SELF:_symValue == rhs:_stringValue
				otherwise
					nop
				end switch
			otherwise
				THROW BinaryError(operator, "Arguments Incompatible", true, SELF, rhs)

			END SWITCH
			THROW BinaryError(operator, "Arguments Incompatible", false, SELF, rhs)
		
#endregion

#region Unary Operators
		STATIC OPERATOR !(u AS __Usual) AS LOGIC
			if u:UsualType == __UsualType.LOGIC
				return !u:_logicValue
			endif
			throw UnaryError("!", u)

		STATIC OPERATOR ~(u AS __Usual) AS __Usual
			if u:UsualType == __UsualType.Long
				return ~u:_intValue
			endif
			if u:UsualType == __UsualType.Int64
				return ~u:_i64Value
			endif
			THROW UnaryError("~", u)

		STATIC OPERATOR -(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG		; return -u:_intValue
			CASE __UsualType.Int64		; return -u:_i64Value
			CASE __UsualType.Float		; return -u:_floatValue
			CASE __UsualType.Decimal	; return -u:_decimalValue
			OTHERWISE
				THROW UnaryError("-", u)
			END SWITCH

		STATIC OPERATOR +(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG		; return u:_intValue
			CASE __UsualType.Int64		; return u:_i64Value
			CASE __UsualType.Float		; return u:_floatValue
			CASE __UsualType.Decimal	; return u:_decimalValue
			OTHERWISE
				THROW UnaryError("+", u)
			END SWITCH
		
		STATIC OPERATOR --(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG		; return u:_intValue - 1
			CASE __UsualType.Int64		; return u:_i64Value - 1
			CASE __UsualType.Float		; return u:_floatValue -1
			CASE __UsualType.Decimal	; return u:_decimalValue - 1 
			OTHERWISE
				THROW UnaryError("--", u)
			END SWITCH
		
		STATIC OPERATOR ++(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG		; return u:_intValue + 1	
			CASE __UsualType.Int64		; return u:_i64Value + 1
			CASE __UsualType.Float		; return u:_floatValue +1
			CASE __UsualType.Decimal	; return u:_decimalValue + 1 
			OTHERWISE
				THROW UnaryError("++", u)
			END SWITCH

#endregion
#region Numeric Operators for Add, Delete etc (also for strings)

		static operator +(lhs as __Usual, rhs as __Usual) as __Usual
			SWITCH lhs:UsualType
			CASE __UsualType.Long
				SWITCH rhs:UsualType	
				CASE __UsualType.Long		; return lhs:_intValue + rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_intValue + rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_intValue + rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_intValue + rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Int64
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_i64Value + rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_i64Value + rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_i64Value + rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_i64Value + rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Float
				LOCAL result as Real8
				SWITCH rhs:UsualType
				CASE __UsualType.Long		
					result :=  lhs:_r8Value + rhs:_intValue 
					return __VoFloat{result, lhs:_width, lhs._decimals}
				CASE __UsualType.Int64		
					result :=  lhs:_r8Value + rhs:_i64Value
					return __VoFloat{result, lhs:_width, lhs._decimals}
				CASE __UsualType.Float
					result :=  lhs:_r8Value + rhs:_r8Value
					return __VoFloat{result, lhs:_width, lhs._decimals}
				CASE __UsualType.Decimal
					result :=  lhs:_r8Value + (REAL8) rhs:_decimalValue
					return __VoFloat{result, lhs:_width, lhs._decimals}

				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Decimal
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_decimalValue + rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_decimalValue + rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_decimalValue + (System.Decimal) rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_decimalValue + rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.String
				SWITCH rhs:UsualType
				CASE __UsualType.String		; return lhs:_stringValue+ rhs:_stringValue
				OTHERWISE
					throw BinaryError("+", "Argument Not String", false, lhs, rhs)
				END SWITCH
			CASE __UsualType.Date
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_dateValue + rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_dateValue + rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_dateValue + rhs:_r8Value
				OTHERWISE
					throw BinaryError("+", "Argument Not Numeric", false, lhs, rhs)
				END SWITCH

			OTHERWISE
				throw BinaryError("+", "Invalid Arguments", true, lhs, rhs)
			END SWITCH
			throw BinaryError("+", "Argument Not Numeric", false, lhs, rhs)

		static operator /(lhs as __Usual, rhs as __Usual) as __Usual
			
			SWITCH lhs:UsualType

			CASE __UsualType.Long
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					local result as INT
					LOCAL remainder as INT
					result := Math.DivRem(lhs:_intValue, rhs:_intValue, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_intValue / rhs:_intValue
					endif
				CASE __UsualType.Int64
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem((int64) lhs:_intValue, rhs:_i64Value, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_intValue / rhs:_i64Value
					endif
				CASE __UsualType.Float
					return __VoFloat{lhs:_intValue / rhs:_r8Value, rhs:_width, rhs:_decimals}

				CASE __UsualType.Decimal
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem((int64) lhs:_intValue, (int64) rhs:_decimalValue, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_intValue / rhs:_decimalValue
					endif
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Int64
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem(lhs:_i64Value, rhs:_intValue, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_i64Value / rhs:_intValue
					endif
				CASE __UsualType.Int64
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem( lhs:_i64Value, rhs:_i64Value, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_i64Value / rhs:_i64Value
					endif
				CASE __UsualType.Float
					return __VoFloat{lhs:_i64Value / rhs:_r8Value, rhs:_width, rhs:_decimals}
				CASE __UsualType.Decimal
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem(lhs:_i64Value, (int64) rhs:_decimalValue, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_i64Value / rhs:_decimalValue
					endif
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Float
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return __VoFloat{lhs:_r8Value / rhs:_intValue, lhs:_width, lhs:_decimals}
				CASE __UsualType.Int64
					return __VoFloat{lhs:_r8Value / rhs:_i64Value, lhs:_width, lhs:_decimals}
				CASE __UsualType.Float
					return __VoFloat{lhs:_r8Value / rhs:_r8Value, Math.Max(lhs:_width,rhs:_width), lhs:_decimals+ rhs:_decimals}
				CASE __UsualType.Decimal
					return __VoFloat{lhs:_r8Value / (REAL8) rhs:_decimalValue, lhs:_width, lhs:_decimals}
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Decimal
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return lhs:_decimalValue / rhs:_intValue
				CASE __UsualType.Int64
					return lhs:_decimalValue / rhs:_i64Value
				CASE __UsualType.Float
					return lhs:_decimalValue / (System.Decimal) rhs:_r8Value
				CASE __UsualType.Decimal
					return lhs:_decimalValue /  rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			OTHERWISE
				throw BinaryError("/", "Invalid Arguments", true, lhs, rhs)
			END SWITCH
			throw BinaryError("/", "Argument Not Numeric", false, lhs, rhs)

		static operator %(lhs as __Usual, rhs as __Usual) as __Usual
			SWITCH lhs:UsualType
			CASE __UsualType.Long
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_intValue % rhs:_intValue
				CASE __UsualType.Int64		; return lhs:_intValue % rhs:_i64Value
				CASE __UsualType.Float		; return __VoFloat{lhs:_intValue % rhs:_r8Value, rhs:_width, rhs:_decimals}
				CASE __UsualType.Decimal	; return lhs:_intValue % rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Int64
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_i64Value % rhs:_intValue
				CASE __UsualType.Int64		; return lhs:_i64Value % rhs:_i64Value
				CASE __UsualType.Float		; return __VoFloat{lhs:_i64Value % rhs:_r8Value, rhs:_width, rhs:_decimals}
				CASE __UsualType.Decimal	; return lhs:_i64Value % rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Float
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return __VoFloat{lhs:_r8Value % rhs:_intValue, lhs:_width, lhs:_decimals}
				CASE __UsualType.Int64		; return __VoFloat{lhs:_r8Value % rhs:_i64Value, lhs:_width, lhs:_decimals}
				CASE __UsualType.Float		; return __VoFloat{lhs:_r8Value % rhs:_r8Value, Math.Max(lhs:_width,rhs:_width), lhs:_decimals+ rhs:_decimals}
				CASE __UsualType.Decimal	; return __VoFloat{lhs:_r8Value % (REAL8) rhs:_decimalValue, lhs:_width, lhs:_decimals}
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Decimal
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_decimalValue % rhs:_intValue
				CASE __UsualType.Int64		; return lhs:_decimalValue % rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_decimalValue % (System.Decimal) rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_decimalValue %  rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH


			OTHERWISE
				throw BinaryError("%", "Invalid Arguments", true, lhs, rhs)
			END SWITCH
			throw BinaryError("%", "Argument Not Numeric", false, lhs, rhs)

		STATIC OPERATOR *(lhs AS __Usual, rhs AS __Usual) AS __Usual
			SWITCH lhs:UsualType
			CASE __UsualType.Long
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_intValue * rhs:_intValue
				CASE __UsualType.Int64		; return lhs:_intValue * rhs:_i64Value
				CASE __UsualType.Float		; return __VoFloat{lhs:_intValue * rhs:_r8Value, rhs:_width, rhs:_decimals}
				CASE __UsualType.Decimal	; return lhs:_intValue * rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Int64
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_i64Value * rhs:_intValue
				CASE __UsualType.Int64		; return lhs:_i64Value * rhs:_i64Value
				CASE __UsualType.Float		; return __VoFloat{lhs:_i64Value * rhs:_r8Value, rhs:_width, rhs:_decimals}
				CASE __UsualType.Decimal	; return lhs:_i64Value * rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Float
				SWITCH rhs:UsualType		
				CASE __UsualType.Long		; return __VoFloat{lhs:_r8Value * rhs:_intValue, lhs:_width, lhs:_decimals}
				CASE __UsualType.Int64		; return __VoFloat{lhs:_r8Value * rhs:_i64Value, lhs:_width, lhs:_decimals}
				CASE __UsualType.Float		; return __VoFloat{lhs:_r8Value * rhs:_r8Value, Math.Max(lhs:_width,rhs:_width), lhs:_decimals+ rhs:_decimals}
				CASE __UsualType.Decimal	; return __VoFloat{lhs:_r8Value * (REAL8) rhs:_decimalValue, lhs:_width, lhs:_decimals}
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Decimal
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_decimalValue * rhs:_intValue
				CASE __UsualType.Int64		; return lhs:_decimalValue * rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_decimalValue * (System.Decimal) rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_decimalValue *  rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			OTHERWISE
				throw BinaryError("*", "Invalid Arguments", true, lhs, rhs)
			END SWITCH
			throw BinaryError("*", "Argument Not Numeric", false, lhs, rhs)

		static operator >>(lhs as __Usual, rhs as int) as __Usual
			// Right shift
			switch lhs:UsualType
			case __UsualType.Long	; return lhs:_intValue >> rhs
			case __UsualType.Int64	; return lhs:_i64Value >> rhs
			otherwise
				throw BinaryError(">>", "Argument not Integer", true, lhs, rhs)
			end switch

		static operator <<(lhs as __Usual, rhs as Long) as __Usual
			// Left shift
			switch (lhs:UsualType)
			case __UsualType.Long	; return lhs:_intValue << rhs
			case __UsualType.Int64	; return lhs:_i64Value << rhs
			otherwise
				throw BinaryError("<<", "Argument not Integer", true, lhs, rhs)
			end switch

		static operator -(lhs as __Usual, rhs as __Usual) as __Usual
			SWITCH lhs:UsualType
			CASE __UsualType.Long
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_intValue - rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_intValue - rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_intValue - rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_intValue - rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.Int64
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_i64Value - rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_i64Value - rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_i64Value - rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_i64Value - rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.Float
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_r8Value - rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_r8Value - rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_r8Value - rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_r8Value - (real8) rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Decimal
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_decimalValue - rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_decimalValue - rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_decimalValue - (System.Decimal) rhs:_r8Value
				CASE __UsualType.Decimal	; return lhs:_decimalValue - rhs:_decimalValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.String
				SWITCH rhs:UsualType
				CASE __UsualType.String
					return CompilerServices.__StringSubtract(lhs, rhs)
				OTHERWISE
					throw BinaryError("-", "Argument Not String", false, lhs, rhs)
				END SWITCH
			CASE __UsualType.Date
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_dateValue - rhs:_intValue 
				CASE __UsualType.Int64		; return lhs:_dateValue - rhs:_i64Value
				CASE __UsualType.Float		; return lhs:_dateValue - rhs:_r8Value
				CASE __UsualType.Date		; return lhs:_dateValue - rhs:_dateValue
				CASE __UsualType.DateTime	; return lhs:_dateValue - (__VoDate) rhs:_dateTimeValue
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.DateTime
				SWITCH rhs:UsualType
				CASE __UsualType.Long		; return lhs:_dateTimeValue:Subtract(TimeSpan{rhs:_intValue,0,0,0})
				CASE __UsualType.Int64		; return lhs:_dateTimeValue:Subtract( TimeSpan{(int)rhs:_i64Value,0,0,0})
				CASE __UsualType.Float		; return lhs:_dateTimeValue:Subtract( TimeSpan{(int)rhs:_r8Value,0,0,0})
				CASE __UsualType.Date		; return lhs:_dateTimeValue:Subtract((DateTime) rhs:_dateValue):Days
				CASE __UsualType.DateTime	; return lhs:_dateTimeValue:Subtract( rhs:_dateTimeValue):Days
				OTHERWISE
					nop
				END SWITCH

			OTHERWISE
				throw BinaryError("-", "Invalid Arguments", true, lhs, rhs)
			END SWITCH
			throw BinaryError("-", "Argument Not Numeric", false, lhs, rhs)

		static operator &(lhs as __Usual, rhs as __Usual) as __Usual
			// Bitwise And
			switch (lhs:UsualType)
			case __UsualType.Long
				switch (rhs:UsualType)
				case __UsualType.Long		; return lhs:_intValue & rhs:_intValue
				case __UsualType.Int64		; return (int64) lhs:_intValue & rhs:_i64Value
				otherwise
					nop
				END SWITCH
			case __UsualType.Int64
				switch (rhs:UsualType)
				case __UsualType.Long		; return lhs:_i64Value & (int64) rhs:_intValue
				case __UsualType.Int64		; return  lhs:_i64Value & rhs:_i64Value
				otherwise
					nop
				END SWITCH
			otherwise
				throw BinaryError("&", "Argument not Integer", true, lhs, rhs)
			end switch
			throw BinaryError("&", "Argument not Integer", false, lhs, rhs)

		STATIC OPERATOR |(lhs AS __Usual, rhs AS __Usual) AS __Usual
			// Bitwise or
			switch (lhs:UsualType)
			case __UsualType.Long
				switch (rhs:UsualType)
				case __UsualType.Long		; return lhs:_intValue | rhs:_intValue
				case __UsualType.Int64		; return (int64) lhs:_intValue | rhs:_i64Value
				otherwise
					nop
				END SWITCH
			case __UsualType.Int64
				switch (rhs:UsualType)
				case __UsualType.Long		; return lhs:_i64Value | (int64) rhs:_intValue
				case __UsualType.Int64		; return  lhs:_i64Value | rhs:_i64Value
				otherwise
					nop
				END SWITCH
			otherwise
				throw BinaryError("|", "Argument not Integer", true, lhs, rhs)
			end switch
			throw BinaryError("|", "Argument not Integer", false, lhs, rhs)
		#endregion

#region Implicit From Usual to Other Type

		STATIC OPERATOR IMPLICIT(u AS __Usual) AS __Array
		switch u:UsualType
			CASE __UsualType.Array	; return (__Array) u:_refData
			CASE __UsualType.Void	; return NULL
			CASE __UsualType.Object	
				if u:_refData== null
					return null
				elseif u:_refData is __Array
					return (__Array) u:_refData
				endif
			END SWITCH
			throw ConversionError("ARRAY", typeof(__Array), u)
			
		// Todo
		//STATIC OPERATOR IMPLICIT(u AS __Usual) AS CodeBlock
		//	THROW NotImplementedException{}

		STATIC OPERATOR IMPLICIT(u AS __Usual) AS LOGIC
			switch u:UsualType
			CASE __UsualType.Logic		; return u:_logicValue
			CASE __UsualType.Long		; return u:_intValue != 0
			CASE __UsualType.Int64		; return u:_i64Value != 0
			CASE __UsualType.Decimal	; return u:_decimalValue != 0
			CASE __UsualType.Void
				return false
			otherwise
				throw ConversionError("LOGIC", typeof(LOGIC), u)
			end switch

		STATIC OPERATOR IMPLICIT(u AS __Usual) AS __VODate
			switch u:UsualType
			CASE __UsualType.Date		; return u:_dateValue
			CASE __UsualType.DateTime	; return (__VoDate) u:_dateTimeValue
			CASE __UsualType.Void		; return __VODate{0}
			otherwise
				throw ConversionError("DATE", typeof(__VODate), u)
			end switch

		STATIC OPERATOR IMPLICIT(u AS __Usual) AS DateTime
			switch u:UsualType
			CASE __UsualType.Date		; return (DateTime) u:_dateValue
			CASE __UsualType.DateTime	; return u:_dateTimeValue
			CASE __UsualType.Void		; return DateTime.MinValue
			otherwise
				throw ConversionError("DATE", typeof(__VODate), u)
			end switch

		static operator implicit(u as __Usual) as System.IntPtr
			switch u:UsualType
			CASE __UsualType.Ptr		; return u:_ptrValue
			CASE __UsualType.LONG		; return (IntPtr) u:_intValue
			CASE __UsualType.Int64		; return (IntPtr) u:_i64Value
			CASE __UsualType.Decimal	; return (IntPtr) u:_decimalValue 
			CASE __UsualType.Void		; return IntPtr.Zero
			otherwise
				throw ConversionError("PTR", typeof(IntPtr), u)
			end switch

		static operator implicit(u as __Usual) as string
			switch u:UsualType
			CASE __UsualType.String	; return u:_stringValue
			CASE __UsualType.Void	; return ""
			CASE __UsualType.SYMBOL	; return (STRING) u:_symValue
			otherwise
				throw ConversionError("STRING", typeof(STRING), u)
			end switch

		static operator implicit(u as __Usual) as __Symbol
			switch u:UsualType
			CASE __UsualType.String	; return (__Symbol) u:_stringValue
			CASE __UsualType.Void	; return __Symbol{""}
			CASE __UsualType.SYMBOL	; return u:_symValue
			otherwise
				throw ConversionError("SYMBOL", typeof(__Symbol), u)
			end switch

		static operator implicit(u as __Usual) as __Psz
			switch u:UsualType
			CASE __UsualType.Ptr	; return (__Psz) u:_ptrValue
			CASE __UsualType.String	; return __Psz{u:_stringValue}
			CASE __UsualType.Void	; return __Psz._Null_Psz
			otherwise
				throw ConversionError("PSZ", typeof(__PSZ), u)
			end switch

#endregion
#region Implicit Numeric Operators
		STATIC OPERATOR IMPLICIT(u AS __Usual) AS BYTE
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long		; return (Byte) u:_intValue
					CASE __UsualType.Int64		; return (Byte) u:_i64Value
					CASE __UsualType.Float		; return (Byte) u:_r8Value
					CASE __UsualType.Logic		; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Decimal	; return (Byte) u:_decimalValue 
					CASE __UsualType.Void		; return 0
					otherwise
						throw ConversionError("BYTE", typeof(Byte), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "BYTE", typeof(Byte), u)
			end try

		static operator implicit(u as __Usual) as SHORT
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return (SHORT) u:_intValue
					CASE __UsualType.Int64	; return (SHORT) u:_i64Value
					CASE __UsualType.Float	; return (SHORT) u:_r8Value
					CASE __UsualType.Decimal; return (Short) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("SHORT", typeof(SHORT), u)
					end switch
				END CHECKED
			catch ex as OverflowException
				throw OverflowError(ex, "SHORT", typeof(SHORT), u)
			end try

		static operator implicit(u as __Usual) as Long
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return u:_intValue
					CASE __UsualType.Int64	; return (long) u:_i64Value
					CASE __UsualType.Float	; return (long) u:_r8Value
					CASE __UsualType.Decimal; return (long) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("LONG", typeof(Long), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "LONG", typeof(Long), u)
			end try
			
		static operator implicit(u as __Usual) as Int64
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return u:_intValue
					CASE __UsualType.Int64	; return (int64) u:_i64Value
					CASE __UsualType.Float	; return (int64) u:_r8Value
					CASE __UsualType.Decimal; return (int64) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("INT64", typeof(int64), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "INT64", typeof(int64), u)
			end try

		static operator implicit(u as __Usual) as System.Decimal
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return u:_intValue	
					CASE __UsualType.Int64	; return u:_i64Value
					CASE __UsualType.Float	; return (System.Decimal) u:_r8Value
					CASE __UsualType.Decimal; return u:_decimalValue
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("DECIMAL", typeof(int64), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "DECIMAL", typeof(int64), u)
			end try

		STATIC OPERATOR IMPLICIT(u AS __Usual) AS SByte
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return (SByte) u:_intValue
					CASE __UsualType.Int64	; return (SByte) u:_i64Value
					CASE __UsualType.Float	; return (SByte) u:_r8Value
					CASE __UsualType.Decimal; return (SByte) u:_decimalValue 
					CASE __UsualType.Logic	; return (SByte) iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("SBYTE", typeof(SByte), u)
					end switch
				END CHECKED
			catch ex as OverflowException
				throw OverflowError(ex, "SBYTE", typeof(SByte), u)
			end try

		// Unsigned
		static operator implicit(u as __Usual) as Word
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return (WORD) u:_intValue
					CASE __UsualType.Int64	; return (WORD) u:_i64Value
					CASE __UsualType.Float	; return (WORD) u:_r8Value
					CASE __UsualType.Decimal; return (WORD) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("WORD", typeof(WORD), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "WORD", typeof(WORD), u)
			end try

		static operator implicit(u as __Usual) as DWord
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return (DWORD) u:_intValue
					CASE __UsualType.Int64	; return (DWORD) u:_i64Value
					CASE __UsualType.Float	; return (DWORD) u:_r8Value
					CASE __UsualType.Decimal; return (DWORD) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("DWORD", typeof(DWORD), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "DWORD", typeof(DWORD), u)
			end try

		static operator implicit(u as __Usual) as UInt64
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return (UINT64) u:_intValue
					CASE __UsualType.Int64	; return (UINT64) u:_i64Value
					CASE __UsualType.Float	; return (UINT64) u:_r8Value
					CASE __UsualType.Decimal; return (UINT64) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("UINT64", typeof(UINT64), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "UINT64", typeof(UINT64), u)
			end try

		// Single, Double and FLoat
		STATIC OPERATOR IMPLICIT(u AS __Usual) AS REAL4
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return (REAL4) u:_intValue
					CASE __UsualType.Int64	; return (REAL4) u:_i64Value
					CASE __UsualType.Float	; return (REAL4) u:_r8Value
					CASE __UsualType.Decimal; return (REAL4) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("REAL4", typeof(REAL4), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "REAL4", typeof(REAL4), u)
			end try

		static operator implicit(u as __Usual) as real8
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return (real8) u:_intValue
					CASE __UsualType.Int64	; return (real8) u:_i64Value
					CASE __UsualType.Float	; return (real8) u:_r8Value
					CASE __UsualType.Decimal; return (REAL8) u:_decimalValue 
					CASE __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
					CASE __UsualType.Void	; return 0
					otherwise
						throw ConversionError("REAL8", typeof(real8), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "REAL8", typeof(real8), u)
			end try

		static operator implicit(u as __Usual) as __VoFloat
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long	; return __VoFloat{(real8) u:_intValue}
					CASE __UsualType.Int64	; return __VoFloat{(real8) u:_i64Value}
					CASE __UsualType.Float	; return __VoFloat{(real8) u:_r8Value}
					CASE __UsualType.Decimal; return __VoFloat{(real8) u:_decimalValue}
					CASE __UsualType.Logic	; return __VoFloat{iif(u:_logicValue, 1, 0)}
					CASE __UsualType.Void	; return __VoFloat{0}
					otherwise
						throw ConversionError("FLOAT", typeof(__VoFloat), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "FLOAT", typeof(__VoFloat), u)
			end try

#endregion
#region Implicit from Other Type to Usual
		static operator implicit(value as Logic) as __Usual
			return __Usual{value}

		static operator implicit(value as Byte) as __Usual
			return __Usual{(int)value}

		static operator implicit(value as __Array) as __Usual
			return __Usual{value}

		static operator implicit(value as __VoDate) as __Usual
			return __Usual{value}

		static operator implicit(value as System.DateTime) as __Usual
			return __Usual{value}

		STATIC OPERATOR IMPLICIT(value AS __VoFloat) AS __Usual
			return __Usual{value}

		static operator implicit(value as real8) as __Usual
			return __Usual{value}

		static operator implicit(value as Short) as __Usual
			return __Usual{(int)value}

		static operator implicit(value as Long) as __Usual
			return __Usual{value}

		static operator implicit(value as Int64) as __Usual
			return __Usual{value}

		static operator implicit(value as UInt64) as __Usual
			return __Usual{value}

		static operator implicit(value as System.Decimal) as __Usual
			return __Usual{value}

		STATIC OPERATOR IMPLICIT(value AS System.IntPtr) AS __Usual
			return __Usual{value}

		static operator implicit(value as SByte) as __Usual
			return __Usual{(int)value}

		static operator implicit(value as real4) as __Usual
			return __Usual{(real8)value }

		static operator implicit(value as string) as __Usual
			return __Usual{value}

		static operator implicit(value as Word) as __Usual
			return __Usual{(int)value}

		static operator implicit(value as DWord) as __Usual
			return IIF((value <= 0x7fffffff),__Usual{(Long)value },__Usual{(__VoFloat)value })
		#endregion

		#region implementation IConvertable
		public method ToBoolean(provider as System.IFormatProvider) as Logic
			return SELF

		public method ToByte(provider as System.IFormatProvider) as Byte
			return SELF

		public method ToChar(provider as System.IFormatProvider) as Char
			var o := __Usual.ToObject(SELF)
			if o is IConvertible
				return ((IConvertible)o):ToChar(provider)
			ENDIF
			THROW InvalidCastException{}

		public method ToDateTime(provider as System.IFormatProvider) as System.DateTime
			RETURN (__VoDate) SELF

		public method ToDecimal(provider as System.IFormatProvider) as Decimal
			RETURN SELF

		public method ToDouble(provider as System.IFormatProvider) as real8
			RETURN SELF

		public method ToInt16(provider as System.IFormatProvider) as Short
			RETURN SELF

		public method ToInt32(provider as System.IFormatProvider) as Long
			RETURN SELF

		public method ToInt64(provider as System.IFormatProvider) as Int64
			RETURN SELF

		static method ToObject(u as __Usual) as Object
			switch u:UsualType
			case __UsualType.ARRAY		; return u:_refData
			case __UsualType.CodeBlock	; return u:_refData			
			CASE __UsualType.Date		; return u:_dateValue
			CASE __UsualType.DateTime	; return u:_dateTimeValue
			case __UsualType.Decimal	; return u:_decimalValue
			case __UsualType.FLOAT		; return __VoFloat{u:_r8Value, u:_width, u:_decimals}
			case __UsualType.Int64		; return u:_i64Value
			case __UsualType.Long		; return u:_intValue
			case __UsualType.LOGIC		; return u:_logicValue
			case __UsualType.OBJECT		; return u:_refData
			case __UsualType.PTR		; return u:_ptrValue
			case __UsualType.STRING		; return u:_refData
			case __UsualType.SYMBOL		; return u:_symValue
			case __UsualType.Void		
			otherwise
				return null
			end switch

		public method ToSByte(provider as System.IFormatProvider) as SByte
			RETURN SELF

		public method ToSingle(provider as System.IFormatProvider) as real4
			RETURN SELF

		public method ToString() as string
			local strResult as STRING
	      
			switch (SELF:usualType)
			CASE __UsualType.Array
			CASE __UsualType.CODEBLOCK
			CASE __UsualType.OBJECT	; strResult := SELF:_refData:ToString()
			CASE __UsualType.Date	; strResult := SELF:_dateValue:ToString()
			CASE __UsualType.DateTime; strResult := SELF:_dateTimeValue:ToString()
			CASE __UsualType.Decimal; strResult := SELF:_decimalValue:ToString()
			CASE __UsualType.Float	; strResult := SELF:_r8Value:ToString()
			CASE __UsualType.Long	; strResult := SELF:_intValue:ToString()
			CASE __UsualType.Int64	; strResult := SELF:_i64Value:ToString()
			CASE __UsualType.LOGIC	; strResult := IIF(!SELF:_logicValue , ".F." , ".T.")
			CASE __UsualType.PTR	; strResult := SELF:_ptrValue:ToString()
			CASE __UsualType.STRING	; strResult := SELF:_stringValue
			CASE __UsualType.Symbol	; strResult := SELF:_symValue:ToString()
			CASE __UsualType.Void	; strResult := "NIL"
			otherwise
				strResult := ""
			end switch
			return strResult


		public method ToString(provider as System.IFormatProvider) as string
			RETURN SELF:ToString()

		public method ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
			IF conversionType:IsPointer
				switch SELF:UsualType	
				CASE __UsualType.PTR	; return _ptrValue
				case __UsualType.Long	; return (IntPtr) _intValue
				case __UsualType.Int64	; return (IntPtr) _i64Value
				otherwise	
					throw InvalidCastException{}
				end switch
			ELSE
				var o := __Usual:ToObject(SELF)
				if conversionType:IsAssignableFrom(o:GetType())
					return o
				elseif o is IConvertible
					return ((IConvertible) o):Totype(conversionType, provider)
				else
					throw InvalidCastException{}
				ENDIF
			ENDIF

		public method ToUInt16(provider as System.IFormatProvider) as Word
			return SELF

		public method ToUInt32(provider as System.IFormatProvider) as DWord
			return SELF

		public method ToUInt64(provider as System.IFormatProvider) as UInt64
			return SELF

		public method GetTypeCode() as System.TypeCode
			switch UsualType
			case __UsualType.ARRAY	; return TypeCode.Object
			case __UsualType.CodeBlock; return TypeCode.Object			
			CASE __UsualType.Date	; return TypeCode.Object
			CASE __UsualType.DateTime; return TypeCode.DateTime
			case __UsualType.Decimal; return TypeCode.Decimal
			case __UsualType.FLOAT	; return TypeCode.Object
			case __UsualType.Int64	; return TypeCode.Int64
			case __UsualType.Long	; return TypeCode.Int32
			case __UsualType.LOGIC	; return TypeCode.Boolean
			case __UsualType.OBJECT	; return TypeCode.Object
			case __UsualType.PTR	; return TypeCode.Object
			case __UsualType.STRING ; return TypeCode.String
			case __UsualType.SYMBOL ; return TypeCode.Object
			case __UsualType.Void
			otherwise				; return TypeCode.Object
			end switch
		#endregion

#region Error Method
	INTERNAL METHOD TypeString() AS STRING
	SWITCH SELF:UsualType
	CASE __UsualType.Array		; RETURN "ARRAY"
	CASE __UsualType.CodeBlock	; RETURN "CODEBLOCK"
	CASE __UsualType.Date		; RETURN "DATE"
	CASE __UsualType.DateTime	; RETURN "DATETIME"
	CASE __UsualType.DECIMAL	; RETURN "DECIMAL"
	CASE __UsualType.FLOAT		; RETURN "FLOAT"
	CASE __UsualType.Int64		; RETURN "INT64"
	CASE __UsualType.Long		; RETURN "LONG"
	CASE __UsualType.Logic		; RETURN "LOGIC"
	CASE __UsualType.PTR		; RETURN "PTR"
	CASE __UsualType.String		; RETURN "STRING"
	CASE __UsualType.Symbol		; RETURN "SYMBOL"
	CASE __UsualType.Void		; RETURN "USUAL"
	OTHERWISE
		if SELF:isReferenceType
			if _refData == null
				return ""
			else
				return _refData:GetType():FullName
			endif
		endif
	END SWITCH
	RETURN "?"

	STATIC METHOD ConversionError(toTypeString AS STRING, toType AS System.Type, u AS __Usual) AS Error
		var err			:= Error{InvalidCastException{}}
		err:GenCode		:= GenCode.DataType
		err:Severity	:= Severity.Error
		err:ArgTypeReq	:= toType
		err:ArgNum		:= 1
		err:FuncSym		:= "USUAL => "+toTypeString
		err:ArgType		:= toTypeString
		err:Description := i"Conversion Error from USUAL ({u:TypeString()})  to {toTypeString}"  
		err:Arg			:= u:ToString()
		RETURN err

	STATIC METHOD OverflowError(ex as OverflowException, toTypeString as string, toType AS System.Type, u AS __Usual) AS Error
		var err := Error{ex}
		err:GenCode		 := GenCode.DataType
		err:Severity	 := Severity.Error
		err:ArgTypeReq	 := toType
		err:ArgNum		 := 1
		err:FuncSym		 := "USUAL => "+toTypeString
		err:ArgType		 := toTypeString
		err:Description  := i"Overflow error converting from USUAL({u:TypeString()})  to {toTypeString}"  
		err:Arg			 := u:ToString()
		RETURN err

	STATIC METHOD BinaryError( operator as STRING, message as STRING, left as logic, lhs as __Usual, rhs as __Usual) as Error
		var err := Error{ArgumentException{}}
		err:GenCode		 := GenCode.ARG
		err:Severity	 := Severity.Error
		err:ArgNum		 := iif (left, 1, 2)
		err:FuncSym		 := operator
		err:Description  := message
		err:Arg			 := iif(left, lhs:ToString(), rhs:ToString())
		RETURN err

	STATIC METHOD UnaryError( operator AS STRING, u AS __Usual) AS Error
		var err := Error{ArgumentException{}}
		err:GenCode		 := GenCode.ARG
		err:Severity	 := Severity.Error
		err:ArgNum		 := 1
		err:FuncSym		 := operator
		err:Description  := "Invalid Argument Type"
		err:Arg			 := u:ToString()
		RETURN err

		
#endregion
	internal class UsualDebugView
		private _uvalue as __Usual
		public constructor (u as __Usual)
			_uvalue := u
		
		[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
		PUBLIC PROPERTY Value AS OBJECT GET _uvalue:VALUE
		PUBLIC PROPERTY Type  as __UsualType GET _uvalue:UsualType

	end class
	*/
	end structure			


	[StructLayout(LayoutKind.Explicit)];
	public structure __UsualData
		// Fields
		[FieldOffset(0)] export d as __VoDate
		[FieldOffset(0)] export r8 as real8
		[FieldOffset(0)] export i as Long
		[FieldOffset(0)] export i64 as Int64
		[FieldOffset(0)] export l as Logic
		[FieldOffset(0)] export p as System.IntPtr
		[FieldOffset(0)] export s as __Symbol
		[FieldOffset(0)] export dt as System.DateTime

	end structure

	public enum __UsualType as Byte
		// These numbers must match with the types defined in the compiler
		// They also match with the USUAL types in VO (BaseType.h)
		member @@Void		:=0
		member @@Long		:=1
		member @@Date		:=2
		member @@Float		:=3
		// Note # 4 (FIXED) was defined but never used in VO
		member @@Array		:=5
		member @@Object		:=6
		member @@String		:=7
		member @@Logic		:=8
		member @@CodeBlock	:=9
		member @@Symbol		:=10
		// see below for missing values
		// The follow numbers are defined but never stored inside a USUAL in VO and Vulcan
		member @@Byte		:=11
		member @@ShortInt	:=12
		member @@Word		:=13
		member @@DWord		:=14
		member @@Real4		:=15
		MEMBER @@Real8		:=16
		member @@Psz		:=17
		member @@PTR		:=18
		member @@Usual		:=19	// USUAL by Ref, not implemented in Vulcan
		member @@Int64		:=22
		member @@Uint64     :=23
		member @@Char		:=24    // not stored in a usual
		member @@Dynamic    :=25 
		member @@DateTime	:=26
		member @@Decimal	:=27
		member @@Memo		:=32	// Used in RDD system in VO
		member @@Invalid    :=99
	end enum

	[StructLayout(LayoutKind.Explicit)];
	public structure __Usual_flags
		[FieldOffset(0)] export usualType as __UsualType
		[FieldOffset(1)] export width as Sbyte
		[FieldOffset(2)] export decimals as Sbyte
		[FieldOffset(3)] export isByRef as logic

		CONSTRUCTOR(type as __UsualType)
			usualType := type
	end structure
		/// <summary>
	/// Determine the data type of an expression.
	/// </summary>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION UsualType(u AS __Usual) AS DWORD
		return (DWORD) u:UsualType
		 

end namespace