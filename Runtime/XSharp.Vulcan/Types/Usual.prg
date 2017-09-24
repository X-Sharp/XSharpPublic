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
	structure __Usual implements IConvertible,IComparable
		#region static fields
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		public static _NIL as __Usual
		#endregion
		#region private fields
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		PRIVATE INITONLY _flags    	AS __Usual_flags	// type, byref, width, decimals
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		private _valueData	as __UsualData		// for non GC data
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		private _refData  	as Object			// for GC data
		#endregion
		#region constructors
		STATIC Constructor()
			_NIL := __Usual{}
		return

		private constructor(u as __Usual)
			SELF:_flags     := u:_flags
			SELF:_valueData	:= u:_valueData
			SELF:_refData 	:= u:_refData 

		return

		private constructor(value as Logic)
			SELF:_flags			:= __Usual_flags{__UsualType.LOGIC}
			SELF:_valueData		:= __UsualData{}{ l:= value}
		return

		private constructor(value as __Array)
			SELF:_flags		:= __Usual_flags{__UsualType.Array}
			SELF:_refData	:= value
		return

		private constructor(value as System.DateTime)
			SELF:_flags		:= __Usual_flags{ __UsualType.Date}
			SELF:_refData	:= value
		return

		private constructor(value as Long)
			SELF:_flags		:= __Usual_flags{__UsualType.Long}
			SELF:_valueData	:= __UsualData{} {i := value}
		return

		private constructor(value as Int64)
			SELF:_flags		:= __Usual_flags{__UsualType.Int64}
			SELF:_valueData	:= __UsualData{} {i64 := value}
		return

		private constructor(value as System.IntPtr)
			SELF:_flags		:= __Usual_flags{__UsualType.PTR}
			SELF:_valueData	:= __UsualData{} {p := value}
		return

		public constructor(o as Object)
			local u				as __Usual
			if (o != null)
				if (o:GetType() == typeof(__Usual))
					// boxed __Usual
					u		:= (__Usual)o 
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
						if ((DWord)o  <= Int32.MaxValue)
							SELF:_flags:usualType := __UsualType.Long
							SELF:_valueData:i := (Long)(DWord)o  
						else
							SELF:_flags:usualType := __UsualType.Float
							SELF:_valueData:r8 := (DWord)o 
							SELF:_flags:width	:= 255
							SELF:_flags:decimals := 255
						endif
					CASE System.TypeCode.Int64 
						SELF:_flags:usualType		:= __UsualType.Int64
						SELF:_valueData:i64	:= (Int64)o 
					CASE System.TypeCode.UInt64 
						IF ((Uint64) o  <= Int64.MaxValue)
							SELF:_flags:usualType	:= __UsualType.Int64
							SELF:_valueData:i64		:= (Int64)(UInt64)o  
						ELSE
							SELF:_flags:usualType := __UsualType.FLOAT
							SELF:_valueData:r8 := (Real8)(UInt64)o  
							SELF:_flags:width	:= 255
							SELF:_flags:decimals := 255
						ENDIF
					CASE System.TypeCode.Single  
						SELF:_flags:usualType		:= __UsualType.Float
						SELF:_valueData:r8	:= (real8)o 
						SELF:_flags:width	:= 255
						SELF:_flags:decimals := 255
					CASE System.TypeCode.Double 
						SELF:_flags:usualType := __UsualType.Float
						SELF:_valueData:r8 := (real8)o 
						SELF:_flags:width := 255
						SELF:_flags:decimals := 255
					CASE System.TypeCode.Decimal 
						SELF:_flags:usualType := __UsualType.OBJECT
						SELF:_refData  := o
					CASE System.TypeCode.DateTime 
						SELF:_flags:usualType := __UsualType.Date
						SELF:_valueData:d := __VoDate{ (System.DateTime) o }
					CASE System.TypeCode.String 
						SELF:_flags:usualType := __UsualType.STRING
						SELF:_refData  := (STRING)o 
					otherwise
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
			SELF:_flags		:= __Usual_flags{__UsualType.STRING}
			SELF:_valueData	:= __UsualData{}
			SELF:_refData 	:= s
		return
		#endregion
		#region properties
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		private PROPERTY _usualType	AS __UsualType GET _flags:UsualType 
		// properties for floats
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		private PROPERTY width			AS BYTE GET _flags:width 
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		private PROPERTY decimals		AS BYTE GET _flags:decimals 
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
 		internal Property UsualType		as __UsualType GET  _usualType
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
 		internal property IsArray		as Logic GET _usualType == __UsualType.Array
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsByRef		as Logic GET SELF:_flags:IsByRef
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsDate		as Logic GET _usualType == __UsualType.Date
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsPtr			as Logic GET _usualType == __UsualType.PTR
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsString		as Logic GET _usualType == __UsualType.STRING
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsLong		as Logic GET _usualType == __UsualType.Int64
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsFloat		as Logic GET _usualType == __UsualType.Float
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsValueType	as LOGIC GET ! SELF:IsReferenceType
		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsReferenceType as LOGIC
			GET
				switch _usualType
				CASE __UsualType.Array
				CASE __UsualType.OBJECT
				CASE __UsualType.STRING
					return true
				otherwise
					return false
				end switch
			END GET
		end property

		[DebuggerBrowsable(DebuggerBrowsableState.Never)];
		internal property IsNil as Logic
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
			CASE __UsualType.Long
				return _valueData.i
			CASE __UsualType.Int64
				return _valueData.i64
			CASE __UsualType.Float
				return _valueData.r8
			CASE __UsualType.Logic
				return _valueData.l
			CASE __UsualType.Date
				return _valueData.d
			CASE __UsualType.Ptr
				return _valueData.p
			CASE __UsualType.Symbol
				return _valueData.s
			CASE __UsualType.String
				return (string) _refData
			CASE __UsualType.Array
				return (__Array) _refData
			CASE __UsualType.Void
				return "NIL"
			CASE __UsualType.Object
			OTHERWISE
				return _refData

			END SWITCH
			END GET
		END PROPERTY
		PROPERTY Type as STRING GET SELF:TypeString()

		#endregion


		#region implementation IComparable
		public method CompareTo(o as Object) as Long
			return 0
		#endregion
				
		#region Comparison Operators 
		static operator >(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i > rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i > rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i > rhs:_valueData.r8
				otherwise
					throw BinaryError(">", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i64 > rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i64 > rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i64 > rhs:_valueData.r8
				otherwise
					throw BinaryError(">", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.r8 > rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.r8 > rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.r8 > rhs:_valueData.r8
				otherwise
					throw BinaryError(">", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.String
				if rhs:UsualType == __UsualType.String
					return (string) lhs:_refData > (string) rhs:_refData
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_valuedata.s > rhs:_valuedata.s
				else
					nop
				endif
			case __UsualType.Date
				if rhs:UsualType == __UsualType.Date
					return lhs:_valuedata.d > rhs:_valuedata.d
				else
					nop
				endif
			otherwise
				nop
			end switch
			throw BinaryError(">", "Incompatible Arguments", false, lhs, rhs)

		static operator >=(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i >= rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i >= rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i >= rhs:_valueData.r8
				otherwise
					throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i64 >= rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i64 >= rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i64 >= rhs:_valueData.r8
				otherwise
					throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.r8 >= rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.r8 >= rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.r8 >= rhs:_valueData.r8
				otherwise
					throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.String
				if rhs:UsualType == __UsualType.String
					return (string) lhs:_refData >= (string) rhs:_refData
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_valuedata.s >= rhs:_valuedata.s
				else
					nop
				endif
			case __UsualType.Date
				if rhs:UsualType == __UsualType.Date
					return lhs:_valuedata.d >= rhs:_valuedata.d
				else
					nop
				endif
			otherwise
				throw BinaryError(">=", "Incompatible Arguments", true, lhs, rhs)
			end switch
			throw BinaryError(">=", "Incompatible Arguments", false, lhs, rhs)

		static operator <(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i < rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i < rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i < rhs:_valueData.r8
				otherwise
					throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i64 < rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i64 < rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i64 < rhs:_valueData.r8
				otherwise
					throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.r8 < rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.r8 < rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.r8 < rhs:_valueData.r8
				otherwise
					throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.String
				if rhs:UsualType == __UsualType.String
					return (string) lhs:_refData < (string) rhs:_refData
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_valuedata.s < rhs:_valuedata.s
				else
					nop
				endif
			case __UsualType.Date
				if rhs:UsualType == __UsualType.Date
					return lhs:_valuedata.d < rhs:_valuedata.d
				else
					nop
				endif
			otherwise
				throw BinaryError("<", "Incompatible Arguments", true, lhs, rhs)
			end switch
			throw BinaryError("<", "Incompatible Arguments", false, lhs, rhs)

		static operator <=(lhs as __Usual, rhs as __Usual) as Logic
			switch lhs:usualType
			case __UsualType.Long
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i <= rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i <= rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i <= rhs:_valueData.r8
				otherwise
					throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.i64 <= rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.i64 <= rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.i64 <= rhs:_valueData.r8
				otherwise
					throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
				end switch
			case __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long
					return lhs:_valueData.r8 <= rhs:_valueData.i
				case __UsualType.Int64
					return lhs:_valueData.r8 <= rhs:_valueData.i64
				case __UsualType.Float
					return lhs:_valueData.r8 <= rhs:_valueData.r8
				otherwise
					throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
				end switch

			case __UsualType.String
				if rhs:UsualType == __UsualType.String
					return (string) lhs:_refData <= (string) rhs:_refData
				else
					nop
				endif

			case __UsualType.Symbol
				if rhs:UsualType == __UsualType.Symbol
					return lhs:_valuedata.s <= rhs:_valuedata.s
				else
					nop
				endif
			case __UsualType.Date
				if rhs:UsualType == __UsualType.Date
					return lhs:_valuedata.d <= rhs:_valuedata.d
				else
					nop
				endif
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
				return ! String.Equals( (string) lhs:_refData, (string) rhs:_refData)
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
				case __UsualType.Long
					return _valueData.i == rhs:_valueData:i
				case __UsualType.Int64
					return (int64) _valueData:i == rhs:_valueData:i64
				case __UsualType.Float
					return (__VoFloat) _valueData:i == (__VOFloat) rhs
				case __UsualType.Logic
					return rhs:_valueData:l == (SELF:_ValueData:i <> 0)
				otherwise
					nop
				end switch

			CASE __UsualType.Int64
				switch rhs:UsualType
				case __UsualType.Long
					return _valueData:i64 == (int64) rhs:_valueData:i
				case __UsualType.Int64
					return  _valueData:i64 == rhs:_valueData:i64
				case __UsualType.Float
					return (__VoFloat) _valueData.i64 == (__VoFloat) rhs
				case __UsualType.Logic
					return rhs:_valueData:l == (SELF:_ValueData:i64 <> 0)
				otherwise
					nop
				end switch

			CASE __UsualType.Float
				switch rhs:UsualType
				case __UsualType.Long
					return (__VoFloat) SELF == (__VoFloat) rhs:_valueData.i
				case __UsualType.Int64
					return  _valueData.i64 == rhs:_valueData.i64
				case __UsualType.Float
					return (__VoFloat) _valueData.i64 == (__VOFloat) rhs
				otherwise
					nop
				end switch

			CASE __UsualType.LOGIC
				switch rhs:UsualType
				case __UsualType.LOGIC
					return SELF:_valueData:l == rhs:_valueData:l
				case __UsualType.Long
					return  SELF:_valueData:l == (rhs:_valueData:i <> 0)
				case __UsualType.Int64
					return  SELF:_valueData:l == (rhs:_valueData:i64 <> 0)
				otherwise
					nop
				end switch

			CASE __UsualType.DATE
				switch rhs:UsualType
				case __UsualType.DATE
					return SELF:_valueData:d == rhs:_valueData:d
				otherwise
					nop
				end switch

			CASE __UsualType.STRING
				switch rhs:UsualType
				case __UsualType.STRING
					return (string) SELF:_refData == (string) rhs:_refData
				case __UsualType.Symbol
					return (string) SELF:_refData == rhs:_valueData:s
				otherwise
					nop
				end switch

			CASE __UsualType.ARRAY
				switch rhs:UsualType
				case __UsualType.ARRAY
					return SELF:_refData == rhs:_refData
				otherwise
					nop
				end switch

			CASE __UsualType.CodeBlock
				switch rhs:UsualType
				case __UsualType.CodeBlock
					return SELF:_refData == rhs:_refData
				otherwise
					nop
				end switch

			CASE __UsualType.Ptr
				switch rhs:UsualType
				case __UsualType.Ptr
					return SELF:_valueData.p == rhs:_valueData.p
				otherwise
					nop
				end switch

			CASE __UsualType.Symbol
				switch rhs:UsualType
				case __UsualType.Symbol
					return SELF:_valueData.s == rhs:_valueData.s
				case __UsualType.String
					return SELF:_valueData.s == (String) rhs:_refData
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
			if (u:UsualType == __UsualType.LOGIC)
				return !u:_valueData.l
			endif
			throw UnaryError("!", u)

		STATIC OPERATOR ~(u AS __Usual) AS __Usual
			if (u:UsualType == __UsualType.Long)
				return ~u:_valueData.i
			endif
			if (u:UsualType == __UsualType.Int64)
				return ~u:_valueData.i64
			endif
			throw UnaryError("~", u)

		STATIC OPERATOR -(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG
				return -u:_valueData.i
			CASE __UsualType.Int64
				return -u:_valueData.i64
			CASE __UsualType.Float
				return __VoFloat{-u:_valueData.r8, u:_flags.width, u:_flags:decimals}
			OTHERWISE
				THROW UnaryError("-", u)
			END SWITCH

		STATIC OPERATOR +(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG
				return u:_valueData.i
			CASE __UsualType.Int64
				return u:_valueData.i64
			CASE __UsualType.Float
				return __VoFloat{u:_valueData.r8, u:_flags.width, u:_flags:decimals}
			OTHERWISE
				THROW UnaryError("+", u)
			END SWITCH
		
		STATIC OPERATOR --(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG
				return u:_valueData.i-1
			CASE __UsualType.Int64
				return u:_valueData.i64-1
			CASE __UsualType.Float
				return __VoFloat{u:_valueData.r8 -1, u:_flags.width, u:_flags:decimals}
			OTHERWISE
				THROW UnaryError("--", u)
			END SWITCH
		
		STATIC OPERATOR ++(u AS __Usual) AS __Usual
			SWITCH u:UsualType
			CASE __UsualType.LONG
				return u:_valueData.i+1
			CASE __UsualType.Int64
				return u:_valueData.i64+1
			CASE __UsualType.Float
				return __VoFloat{u:_valueData.r8 +1, u:_flags.width, u:_flags:decimals}
			OTHERWISE
				THROW UnaryError("++", u)
			END SWITCH

#endregion
#region Numeric Operators for Add, Delete etc (also for strings)

		static operator +(lhs as __Usual, rhs as __Usual) as __Usual
			SWITCH lhs:UsualType
			CASE __UsualType.Long
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i + rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.i + rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.i + rhs:_valueData.r8
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.Int64
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i64 + rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.i64 + rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.i64 + rhs:_valueData.r8
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.Float
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.r8 + rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.r8 + rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.r8 + rhs:_valueData.r8
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.String
				SWITCH rhs:UsualType
				CASE __UsualType.String
					return (String) lhs:_refData + (string) rhs:_refData
				OTHERWISE
					throw BinaryError("+", "Argument Not String", false, lhs, rhs)
				END SWITCH
			CASE __UsualType.Date
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.d + rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.d + rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.d + rhs:_valueData.r8
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
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					local result as INT
					LOCAL remainder as INT
					result := Math.DivRem(lhs:_valueData.i, rhs:_valueData.i, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_valueData.i / rhs:_valueData.i
					endif
				CASE __UsualType.Int64
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem((int64) lhs:_valueData.i, rhs:_valueData.i64, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_valueData.i / rhs:_valueData.i64
					endif
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.i / rhs:_valueData.r8, rhs:_flags.width, rhs:_flags.decimals}
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Int64
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem(lhs:_valueData.i64, rhs:_valueData.i, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_valueData.i64 / rhs:_valueData.i
					endif
				CASE __UsualType.Int64
					local result as INT64
					LOCAL remainder as INT64
					result := Math.DivRem( lhs:_valueData.i64, rhs:_valueData.i64, OUT remainder)
					if remainder == 0
						return result
					else
						return lhs:_valueData.i64 / rhs:_valueData.i64
					endif
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.i64 / rhs:_valueData.r8, rhs:_flags.width, rhs:_flags.decimals}
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Float
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return __VoFLoat{lhs:_valueData.r8 / rhs:_valueData.i, lhs:_flags.width, rhs:_flags.decimals}
				CASE __UsualType.Int64
					return __VoFLoat{lhs:_valueData.r8 / rhs:_valueData.i64, lhs:_flags.width, rhs:_flags.decimals}
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.r8 / rhs:_valueData.r8, Math.Max(lhs:_flags.width,rhs:_flags.width), lhs:_flags.decimals+ rhs:_flags.decimals}
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
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i % rhs:_valueData.i
				CASE __UsualType.Int64
					return lhs:_valueData.i % rhs:_valueData.i64
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.i % rhs:_valueData.r8, rhs:_flags.width, rhs:_flags.decimals}
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Int64
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i64 % rhs:_valueData.i
				CASE __UsualType.Int64
					return lhs:_valueData.i64 % rhs:_valueData.i64
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.i64 % rhs:_valueData.r8, rhs:_flags.width, rhs:_flags.decimals}
				OTHERWISE
					nop
				END SWITCH

			CASE __UsualType.Float
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return __VoFLoat{lhs:_valueData.r8 % rhs:_valueData.i, lhs:_flags.width, rhs:_flags.decimals}
				CASE __UsualType.Int64
					return __VoFLoat{lhs:_valueData.r8 % rhs:_valueData.i64, lhs:_flags.width, rhs:_flags.decimals}
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.r8 % rhs:_valueData.r8, Math.Max(lhs:_flags.width,rhs:_flags.width), lhs:_flags.decimals+ rhs:_flags.decimals}
				OTHERWISE
					nop
				END SWITCH

			OTHERWISE
				throw BinaryError("%", "Invalid Arguments", true, lhs, rhs)
			END SWITCH
			throw BinaryError("%", "Argument Not Numeric", false, lhs, rhs)
		static operator *(lhs as __Usual, rhs as __Usual) as __Usual
			SWITCH lhs:UsualType
			CASE __UsualType.Long
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i * rhs:_valueData.i
				CASE __UsualType.Int64
					return lhs:_valueData.i * rhs:_valueData.i64
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.i * rhs:_valueData.r8, rhs:_flags.width, rhs:_flags.decimals}
				OTHERWISE
					throw BinaryError("*", "Argument Not Numeric", false, lhs, rhs)
				END SWITCH

			CASE __UsualType.Int64
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i64 * rhs:_valueData.i
				CASE __UsualType.Int64
					return lhs:_valueData.i64 * rhs:_valueData.i64
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.i64 * rhs:_valueData.r8, rhs:_flags.width, rhs:_flags.decimals}
				OTHERWISE
					throw BinaryError("*", "Argument Not Numeric", false, lhs, rhs)
				END SWITCH

			CASE __UsualType.Float
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return __VoFLoat{lhs:_valueData.r8 * rhs:_valueData.i, lhs:_flags.width, rhs:_flags.decimals}
				CASE __UsualType.Int64
					return __VoFLoat{lhs:_valueData.r8 * rhs:_valueData.i64, lhs:_flags.width, rhs:_flags.decimals}
				CASE __UsualType.Float
					return __VoFLoat{lhs:_valueData.r8 * rhs:_valueData.r8, Math.Max(lhs:_flags.width,rhs:_flags.width), lhs:_flags.decimals+ rhs:_flags.decimals}
				OTHERWISE
					throw BinaryError("*", "Argument Not Numeric", false, lhs, rhs)
				END SWITCH


			OTHERWISE
				throw BinaryError("*", "Invalid Arguments", true, lhs, rhs)
			END SWITCH

		static operator >>(lhs as __Usual, rhs as int) as __Usual
			// Right shift
			switch lhs:UsualType
			case __UsualType.Long
				return lhs:_valuedata.i >> rhs
			case __UsualType.Int64
				return lhs:_valuedata.i64 >> rhs
			otherwise
				throw BinaryError(">>", "Argument not Integer", true, lhs, rhs)
			end switch

		static operator <<(lhs as __Usual, rhs as Long) as __Usual
			// Left shift
			switch (lhs:UsualType)
			case __UsualType.Long
				return lhs:_valuedata.i << rhs
			case __UsualType.Int64
				return lhs:_valuedata.i64 << rhs
			otherwise
				throw BinaryError("<<", "Argument not Integer", true, lhs, rhs)
			end switch

		static operator -(lhs as __Usual, rhs as __Usual) as __Usual
			SWITCH lhs:UsualType
			CASE __UsualType.Long
				SWITCH lhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i - rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.i - rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.i - rhs:_valueData.r8
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.Int64
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.i64 - rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.i64 - rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.i64 - rhs:_valueData.r8
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.Float
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.r8 - rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.r8 - rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.r8 - rhs:_valueData.r8
				OTHERWISE
					nop
				END SWITCH
			CASE __UsualType.String
				SWITCH rhs:UsualType
				CASE __UsualType.String
					return __StringSubtract(lhs, rhs)
				OTHERWISE
					throw BinaryError("-", "Argument Not String", false, lhs, rhs)
				END SWITCH
			CASE __UsualType.Date
				SWITCH rhs:UsualType
				CASE __UsualType.Long
					return lhs:_valueData.d - rhs:_valueData.i 
				CASE __UsualType.Int64
					return lhs:_valueData.d - rhs:_valueData.i64
				CASE __UsualType.Float
					return lhs:_valueData.d - rhs:_valueData.r8
				CASE __UsualType.Date
					return lhs:_valueData.d - rhs:_valueData.d
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
				case __UsualType.Long
					return lhs:_valuedata.i & rhs:_valuedata.i
				case __UsualType.Int64
					return (int64) lhs:_valuedata.i & rhs:_valuedata.i64
				otherwise
					throw BinaryError("&", "Argument not Integer", false, lhs, rhs)
				END SWITCH
			case __UsualType.Int64
				switch (rhs:UsualType)
				case __UsualType.Long
					return lhs:_valuedata.i64 & (int64) rhs:_valuedata.i
				case __UsualType.Int64
					return  lhs:_valuedata.i64 & rhs:_valuedata.i64
				otherwise
					throw BinaryError("&", "Argument not Integer", false, lhs, rhs)
				END SWITCH
			otherwise
				throw BinaryError("&", "Argument not Integer", true, lhs, rhs)
			end switch
		static operator |(lhs as __Usual, rhs as __Usual) as __Usual
			// Bitwise or
			switch (lhs:UsualType)
			case __UsualType.Long
				switch (rhs:UsualType)
				case __UsualType.Long
					return lhs:_valuedata.i | rhs:_valuedata.i
				case __UsualType.Int64
					return (int64) lhs:_valuedata.i | rhs:_valuedata.i64
				otherwise
					throw BinaryError("|", "Argument not Integer", false, lhs, rhs)
				END SWITCH
			case __UsualType.Int64
				switch (rhs:UsualType)
				case __UsualType.Long
					return lhs:_valuedata.i64 | (int64) rhs:_valuedata.i
				case __UsualType.Int64
					return  lhs:_valuedata.i64 | rhs:_valuedata.i64
				otherwise
					throw BinaryError("|", "Argument not Integer", false, lhs, rhs)
				END SWITCH
			otherwise
				throw BinaryError("|", "Argument not Integer", true, lhs, rhs)
			end switch
		#endregion

#region Implicit From Usual to Other Type

		STATIC OPERATOR IMPLICIT(u AS __Usual) AS __Array
		switch u:UsualType
			CASE __UsualType.Array
				return (__Array) u:_refData
			CASE __UsualType.Void
				return NULL
			CASE __UsualType.Object
				if (u:_refData== null)
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
			CASE __UsualType.Logic
				return u:_valueData.l
			CASE __UsualType.Long
				return u:_valueData.i != 0
			CASE __UsualType.Int64
				return u:_valueData.i64 != 0
			CASE __UsualType.Void
				return false
			otherwise
				throw ConversionError("LOGIC", typeof(LOGIC), u)
			end switch

		STATIC OPERATOR IMPLICIT(u AS __Usual) AS __VODate
			switch u:UsualType
			CASE __UsualType.Date
				return u:_valueData.d
			CASE __UsualType.Void
				return __VODate{0}
			otherwise
				throw ConversionError("DATE", typeof(__VODate), u)
			end switch

		static operator implicit(u as __Usual) as System.IntPtr
			switch u:UsualType
			CASE __UsualType.Ptr
				return u:_valueData.p
			CASE __UsualType.LONG
				return (IntPtr) u:_valueData.i
			CASE __UsualType.Int64
				return (IntPtr) u:_valueData.i64
			CASE __UsualType.Void
				return IntPtr.Zero
			otherwise
				throw ConversionError("PTR", typeof(IntPtr), u)
			end switch

		static operator implicit(u as __Usual) as string
			switch u:UsualType
			CASE __UsualType.String
				return (STRING) u:_refData
			CASE __UsualType.Void
				return ""
			CASE __UsualType.SYMBOL
				return (STRING) u:_valueData.s
			otherwise
				throw ConversionError("STRING", typeof(STRING), u)
			end switch

		static operator implicit(u as __Usual) as __Symbol
			switch u:UsualType
			CASE __UsualType.String
				return (__Symbol) (String) u:_refData
			CASE __UsualType.Void
				return __Symbol{""}
			CASE __UsualType.SYMBOL
				return u:_valueData.s
			otherwise
				throw ConversionError("SYMBOL", typeof(__Symbol), u)
			end switch

		static operator implicit(u as __Usual) as __Psz
			switch u:UsualType
			CASE __UsualType.Ptr
				return (__Psz) u:_valueData.p
			CASE __UsualType.String
				return __Psz{(STRING) u:_refData}
			CASE __UsualType.Void
				return __Psz._Null_Psz
			otherwise
				throw ConversionError("PSZ", typeof(__PSZ), u)
			end switch

#endregion
#region Implicit Numeric Operators
		STATIC OPERATOR IMPLICIT(u AS __Usual) AS BYTE
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long
						return (Byte) u:_valueData:i
					CASE __UsualType.Int64
						return (Byte) u:_valueData:I64
					CASE __UsualType.Float
						return (Byte) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return (SHORT) u:_valueData:i
					CASE __UsualType.Int64
						return (SHORT) u:_valueData:I64
					CASE __UsualType.Float
						return (SHORT) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return u:_valueData:i
					CASE __UsualType.Int64
						return (long) u:_valueData:I64
					CASE __UsualType.Float
						return (long) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return u:_valueData:i
					CASE __UsualType.Int64
						return (int64) u:_valueData:I64
					CASE __UsualType.Float
						return (int64) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
					otherwise
						throw ConversionError("INT64", typeof(int64), u)
					end switch
				END CHECKED
			CATCH ex AS OverflowException
				throw OverflowError(ex, "INT64", typeof(int64), u)
			end try

		static operator implicit(u as __Usual) as SByte
			try
				BEGIN CHECKED
					switch u:UsualType
					CASE __UsualType.Long
						return (SByte) u:_valueData:i
					CASE __UsualType.Int64
						return (SByte) u:_valueData:I64
					CASE __UsualType.Float
						return (SByte) u:_ValueData:R8
					CASE __UsualType.Logic
						return (SByte) iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return (WORD) u:_valueData:i
					CASE __UsualType.Int64
						return (WORD) u:_valueData:I64
					CASE __UsualType.Float
						return (WORD) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return (DWORD) u:_valueData:i
					CASE __UsualType.Int64
						return (DWORD) u:_valueData:I64
					CASE __UsualType.Float
						return (DWORD) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return (UINT64) u:_valueData:i
					CASE __UsualType.Int64
						return (UINT64) u:_valueData:I64
					CASE __UsualType.Float
						return (UINT64) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return (REAL4) u:_valueData:i
					CASE __UsualType.Int64
						return (REAL4) u:_valueData:I64
					CASE __UsualType.Float
						return (REAL4) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return (real8) u:_valueData:i
					CASE __UsualType.Int64
						return (real8) u:_valueData:I64
					CASE __UsualType.Float
						return (real8) u:_ValueData:R8
					CASE __UsualType.Logic
						return iif(u:_ValueData:l, 1, 0)
					CASE __UsualType.Void
						return 0
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
					CASE __UsualType.Long
						return __VoFloat{(real8) u:_valueData:i}
					CASE __UsualType.Int64
						return __VoFloat{(real8) u:_valueData:I64}
					CASE __UsualType.Float
						return __VoFloat{(real8) u:_ValueData:R8}
					CASE __UsualType.Logic
						return __VoFloat{iif(u:_ValueData:l, 1, 0)}
					CASE __UsualType.Void
						return __VoFloat{0}
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

		static operator implicit(value as __VoFLoat) as __Usual
			return __Usual{value}

		STATIC OPERATOR IMPLICIT(value AS System.DateTime) AS __Usual
			return __Usual{value}

		static operator implicit(value as real8) as __Usual
			return __Usual{value}

		static operator implicit(value as Short) as __Usual
			return __Usual{(int)value}

		static operator implicit(value as Long) as __Usual
			return __Usual{value}

		static operator implicit(value as Int64) as __Usual
			return __Usual{value}

		static operator implicit(value as System.IntPtr) as __Usual
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
			return IIF((value > 0x7fffffff),__Usual{(Long)value },__Usual{(__VoFloat)value })
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
			RETURN (System.Decimal) (REAL8) SELF

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
			case __UsualType.ARRAY
				return u:_refData
			case __UsualType.CodeBlock
				return u:_refData			
			CASE __UsualType.Date
				return u:_valueData.d
			case __UsualType.FLOAT
				return __VoFLoat{u:_valueData.r8, u:_flags.width, u:_flags.decimals}
			case __UsualType.Long
				return u:_valueData.i
			case __UsualType.Int64
				return u:_valueData.i64
			case __UsualType.LOGIC
				return u:_valueData.l
			case __UsualType.OBJECT
				return u:_refData
			case __UsualType.PTR
				return u:_valueData.p
			case __UsualType.STRING
				return u:_refData
			case __UsualType.SYMBOL
				return u:_valueData.s
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
			CASE __UsualType.OBJECT
				strResult := SELF:_refData:ToString()

			CASE __UsualType.Date
				strResult := SELF:_valueData:d:ToString()

			CASE __UsualType.Float
				strResult := SELF:_valueData:r8:ToString()

			CASE __UsualType.Long
				strResult := SELF:_valueData:i:ToString()

			CASE __UsualType.Int64
				strResult := SELF:_valueData:i64:ToString()

			CASE __UsualType.LOGIC
				strResult := IIF(!SELF:_valueData:l , ".F." , ".T.")

			CASE __UsualType.PTR
				strResult := SELF:_valueData:p:ToString()

			CASE __UsualType.STRING
				strResult := (string) SELF:_refData;

			CASE __UsualType.Symbol
				strResult := SELF:_valueData:s:ToString()

			CASE __UsualType.Void
				strResult := "NIL"
			otherwise
				strResult := ""
			end switch
			return strResult


		public method ToString(provider as System.IFormatProvider) as string
			RETURN SELF:ToString()

		public method ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
			IF conversionType:IsPointer
				switch SELF:UsualType 
				case __UsualType.PTR
					return _valueData.p
				case __UsualType.Long
					return (IntPtr) _valueData.i
				case __UsualType.Int64
					return (IntPtr) _valueData.i64
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
			case __UsualType.ARRAY
				return TypeCode.Object
			case __UsualType.CodeBlock
				return TypeCode.Object			
			CASE __UsualType.Date
				return TypeCode.Object
			case __UsualType.FLOAT
				return TypeCode.Object
			case __UsualType.Long
				return TypeCode.Int32
			case __UsualType.Int64
				return TypeCode.Int64
			case __UsualType.LOGIC
				return TypeCode.Boolean
			case __UsualType.OBJECT
				return TypeCode.Object
			case __UsualType.PTR
				return TypeCode.Object
			case __UsualType.STRING
				return TypeCode.String
			case __UsualType.SYMBOL
				return TypeCode.Object
			case __UsualType.Void
			otherwise
				return TypeCode.Object
			end switch
		#endregion

#region Error Method
	INTERNAL METHOD TypeString() AS STRING
	SWITCH SELF:UsualType
	CASE __UsualType.Array
		RETURN "ARRAY"
	CASE __UsualType.CodeBlock
		RETURN "CODEBLOCK"
	CASE __UsualType.Date
		RETURN "DATE"
	CASE __UsualType.FLOAT
		RETURN "FLOAT"
	CASE __UsualType.Long
		RETURN "LONG"
	CASE __UsualType.Int64
		RETURN "INT64"
	CASE __UsualType.Logic
		RETURN "LOGIC"
	CASE __UsualType.OBJECT
		if (_refData != null)
			return ""
		else
			return _refData:GetType():FullName
		endif
	CASE __UsualType.PTR
		RETURN "PTR"
	CASE __UsualType.String
		RETURN "STRING"
	CASE __UsualType.Symbol
		RETURN "SYMBOL"
	CASE __UsualType.Void
		RETURN "USUAL"
	END SWITCH
	RETURN "?"

	STATIC METHOD ConversionError(toTypeString AS STRING, toType AS System.Type, u AS __Usual) AS Error
		var err := Error{InvalidCastException{}}
		err:GenCode		 := GenCode.DataType
		err:Severity	 := Severity.Error
		err:ArgTypeReq	 := toType
		err:ArgNum		 := 1
		err:FuncSym		 := "USUAL => "+toTypeString
		err:ArgType		 := toTypeString
		err:Description  := i"Conversion Error from USUAL ({u:TypeString()})  to {toTypeString}"  
		err:Arg			 := u:ToString()
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
		member @@Psz		:=17
		member @@PTR		:=18
		member @@Usual		:=19	// USUAL by Ref, not implemented in Vulcan
		member @@Int64		:=22
		// The follow numbers are defined but never stored inside a USUAL in VO and Vulcan
		member @@Byte		:=11
		member @@ShortInt	:=12
		member @@Word		:=13
		member @@DWord		:=14
		member @@Real4		:=15
		member @@Real8		:=16
		member @@Uint64     :=23
		member @@Memo		:=32	// Used in RDD system in VO
		member @@Invalid    :=99
	end enum

	[StructLayout(LayoutKind.Explicit)];
	public structure __Usual_flags
		[FieldOffset(0)] export usualType as __UsualType
		[FieldOffset(1)] export width as byte
		[FieldOffset(2)] export decimals as byte
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