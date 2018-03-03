//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.InteropServices
using System.Diagnostics
using XSharp.Internal
begin namespace XSharp
	[StructLayout(LayoutKind.Sequential)];
		[DebuggerDisplay("{Value} ({UsualType})", Type := "USUAL")];
		[DebuggerTypeProxy(typeof(UsualDebugView))];
		public structure __Usual implements IConvertible,IComparable
		#region static fields
			public static _NIL as __Usual
		#endregion
		
		#region private fields
			private initonly _flags    	as __Usual_flags	// type, byref, width, decimals
			private initonly _valueData	as __UsualData		// for non GC data
			private initonly _refData  	as object			// for GC data
		#endregion
		
		#region constructors
			static constructor
				_NIL := __Usual{}
				return
			
			
			private constructor(u as __Usual)
				self:_flags     := u:_flags
				self:_valueData	:= u:_valueData
				self:_refData 	:= u:_refData 
				
				return
			
			private constructor(f as __VoFloat)
				self:_valueData:r8		:= f:Value
				self:_flags:usualType	:= __UsualType.Float
				self:_flags:Width		:= (Sbyte) f:Digits
				self:_flags:Decimals	:= (Sbyte) f:Decimals
				
				return
			
			private constructor(r8 as real8)
				self:_valueData:r8		:= r8
				self:_flags:usualType	:= __UsualType.Float
				self:_flags:Width		:= -1
				self:_flags:Decimals	:= -1
				
				return
			
			private constructor(value as logic)
				self:_flags:usualType	:= __UsualType.LOGIC
				self:_valueData:l		:= value
				return
			
			private constructor(value as __Array)
				self:_flags:usualType	:= __UsualType.Array
				self:_refData			:= value
				return
			
			private constructor(value as __VoDate)
				self:_flags:usualType	:= __UsualType.Date
				self:_valueData:d		:= value
				return
			
			private constructor(value as System.DateTime)
				self:_flags:usualType	:= __UsualType.DateTime
				self:_valueData:dt		:= value
				
				return
			
			private constructor(value as long)
				self:_flags:usualType	:= __UsualType.LONG
				_valueData:i			:= value
				return
			
			private constructor(value as int64)
				self:_flags:usualType	:= __UsualType.INT64
				self:_valueData:i64		:= value
				return
			
			private constructor(value as uint64)
				if value < Int64.MaxValue
					self:_flags:usualType	:= __UsualType.INT64
					self:_valueData:i64:= (int64) value
				else
					self:_flags:usualType	:= __UsualType.FLOAT
					self:_valueData:r8 := value
				endif
				return
			
			private constructor(d as System.Decimal)
				self:_flags:usualType  := __UsualType.Decimal
				self:_refdata	:= d
			
			
			private constructor(value as System.IntPtr)
				self:_flags:usualType	:= __UsualType.PTR
				self:_valueData:p		:= value
				return
			
			public constructor(o as object)
				local u				as __Usual
				if o != null
					if o:GetType() == typeof(__Usual)
						// boxed __Usual
						u		:= (__Usual)o 
						self:_flags		:= u:_flags
						self:_refData	:= u:_refData 
						self:_valueData	:= u:_valueData
					else
						//  decode type from typecode
						var vartype := o:GetType()
						var typeCode := System.Type.GetTypeCode(vartype)
						switch typeCode
							case  System.TypeCode.DBNull
								self:_flags:usualType := __UsualType.Void
						self:_refData	:= null
							
							case System.TypeCode.Boolean
								self:_flags:usualType := __UsualType.LOGIC
						self:_valueData:l := (logic)o 
							
							case System.TypeCode.Char
								self:_flags:usualType		:= __UsualType.Long
						self:_valueData:i	:= (char)o 
							
							case System.TypeCode.SByte
								self:_flags:usualType		:= __UsualType.Long
						self:_valueData:i	:= (SByte)o 
							
							case System.TypeCode.Byte
								self:_flags:usualType		:= __UsualType.Long
						self:_valueData:i	:= (byte)o 
							
							case System.TypeCode.Int16 
								self:_flags:usualType		:= __UsualType.Long
						self:_valueData:i	:= (short)o 
							
							case System.TypeCode.UInt16
								self:_flags:usualType		:= __UsualType.Long
						self:_valueData:i	:= (word)o 
							
							case System.TypeCode.Int32
								self:_flags:usualType		:= __UsualType.Long
						self:_valueData:i	:= (long)o 
							
							case System.TypeCode.UInt32
								if (dword)o  <= Int32.MaxValue
									self:_flags:usualType := __UsualType.Long
									self:_valueData:i := (long)(dword)o  
								else
									self:_flags:usualType := __UsualType.Float
									self:_valueData:r8:= (real8) (UInt32) o 
									self:_flags:width	:= -1
									self:_flags:decimals := -1
						endif
							case System.TypeCode.Int64 
								self:_flags:usualType		:= __UsualType.Int64
						self:_valueData:i64	:= (int64)o 
							
							case System.TypeCode.UInt64 
								if (uint64) o  <= Int64.MaxValue
									self:_flags:usualType	:= __UsualType.Int64
									self:_valueData:i64		:= (int64)(uint64)o  
								else
									self:_flags:usualType := __UsualType.FLOAT
									self:_valueData:r8 := (real8)(uint64)o  
									self:_flags:width	:= -1
									self:_flags:decimals := -1
						endif
							case System.TypeCode.Single  
								self:_flags:usualType		:= __UsualType.Float
								self:_valueData:r8	:= (real8)o 
								self:_flags:width	:= -1
						self:_flags:decimals := -1
							
							case System.TypeCode.Double 
								self:_flags:usualType := __UsualType.Float
								self:_valueData:r8 := (real8)o 
								self:_flags:width := -1
						self:_flags:decimals := -1
							
							case System.TypeCode.Decimal 
								self:_flags:usualType := __UsualType.Decimal
						self:_refData  := o
							
							case System.TypeCode.DateTime 
								self:_flags:usualType := __UsualType.DateTime
						self:_valueData:dt := (System.DateTime) o 
							
							case System.TypeCode.String 
								self:_flags:usualType := __UsualType.STRING
						self:_refData  := (string)o 
							
							otherwise
								if vartype == typeof(__Array)
									self:_flags:usualType := __UsualType.Array
									self:_refData  := o
								elseif vartype == typeof(__VODate)
									self:_flags:usualType := __UsualType.Date
									self:_valueData:d :=  (__VoDate) o
								elseif vartype == typeof(__Symbol)
									self:_flags:usualType := __UsualType.Symbol
									self:_valueData:s :=  (__Symbol) o
						endif
						end switch
					endif
				endif
				return
			
			private constructor(s as string)
				self:_flags:usualType	:= __UsualType.STRING
				self:_refData 			:= s
				return
		#endregion
		
		#region properties
			private property _usualType		as __UsualType	get _flags:usualType 
			private property _decimalValue	as System.Decimal get (System.Decimal) _refData 
			private property _i64Value		as int64	get _valueData:i64 
			private property _r8Value		as real8	get _valueData:r8 
			private property _dateValue		as __VoDate get _valueData:d 
			private property _dateTimeValue as DateTime get _valueData:dt
			private property _intValue		as int		get _valueData:i  
			private property _symValue		as __Symbol	get _valueData:s 
			private property _logicValue	as logic	get _valueData:l 
			private property _ptrValue		as IntPtr	get _valueData:p 
			private property _stringValue   as string	get iif(UsualType == __UsualType.String, (string) _refData , String.Empty)
			private property _floatValue    as __VoFloat get __VoFloat{ _valueData:r8, _width, _decimals}
			// properties for floats
			private property _width			as SBYTE get _flags:width 
			private property _decimals		as SBYTE get _flags:decimals 
			internal property UsualType		as __UsualType get  _usualType
			
			private property isReferenceType as logic
				get
					switch _usualType
					case __UsualType.Array
					case __UsualType.Object
					case __UsualType.Decimal
					case __UsualType.String
						return true
					otherwise
						return false
					end switch
				end get
			end property
			
			internal property IsNil as logic
				get
					return self:usualType == __UsualType.Void .or. ;
						(self:IsReferenceType .and. self:_refData  == null)
					
				end get
			end property
		#endregion
		#region Properties for the Debugger
			property Value as object 
				get
					switch UsualType
						case __UsualType.Array		; return (__Array) _refData
						case __UsualType.Date		; return _dateValue
						case __UsualType.DateTime	; return _dateTimeValue
						case __UsualType.Decimal	; return _decimalValue
						case __UsualType.Float		; return _r8Value
						case __UsualType.Int64		; return _i64Value
						case __UsualType.Long		; return _intValue
						case __UsualType.Logic		; return _logicValue
						case __UsualType.Ptr		; return _ptrValue
						case __UsualType.Symbol		; return _symValue
						case __UsualType.String		; return _stringValue
						case __UsualType.Void		; return "NIL"
						case __UsualType.Object
						otherwise					; return _refData
					end switch
				end get
			end property
			
		#endregion
		
		
		#region implementation IComparable
			public method CompareTo(o as object) as long
				local rhs as __Usual
				rhs := (__Usual) o
				if self:UsualType == rhs:UsualType
					// Compare ValueTypes
					switch UsualType
						case __UsualType.Date		; return self:_dateValue:CompareTo(rhs:_dateValue)
						case __UsualType.DateTime	; return self:_dateTimeValue:CompareTo(rhs:_dateTimeValue)
						case __UsualType.Decimal	; return self:_decimalValue:CompareTo(rhs:_decimalValue)
						case __UsualType.Int64		; return self:_i64Value:CompareTo(rhs:_i64Value)
						case __UsualType.Logic		; return self:_logicValue:CompareTo(rhs:_logicValue)
						case __UsualType.Long		; return self:_intValue:CompareTo(rhs:_intValue)
						case __UsualType.Ptr		; return self:_ptrValue:ToInt64():CompareTo(rhs:_ptrValue:ToInt64())
						// Uses String Comparison rules
						// Vulcan does a case insensitive comparison ?
						case __UsualType.String		; return String.Compare( _stringValue,  rhs:_stringValue)
						case __UsualType.Symbol		; return String.Compare( (string) self:_symValue, (string) rhs:_symValue)
						otherwise					; return 0
					end switch
				else
					// Type of LHS different from type of RHS
					switch self:UsualType
						case __UsualType.Void
							return -1
						case __UsualType.Date
							// Upscale when needed to avoid overflow errors
							switch rhs:UsualType
								case __UsualType.DateTime	; return _dateValue.CompareTo((__VoDate) rhs:_dateTimeValue)
								case __UsualType.Decimal	; return ((System.Decimal) (int) _dateValue).CompareTo(rhs:_decimalValue)
								case __UsualType.Float		; return ((real8) (int) _dateValue).CompareTo(rhs:_r8Value)
								case __UsualType.Int64		; return ( (int64) (int) _dateValue).CompareTo(rhs:_i64Value)
								case __UsualType.Long		; return ((int) _dateValue).CompareTo(rhs:_intValue)
								otherwise
									nop	// uses comparison by type
							end switch
						
						case __UsualType.Float
							switch rhs:UsualType
								case __UsualType.Date		; return _r8Value.CompareTo( (real8) (int) rhs:_dateValue)
								case __UsualType.Decimal	; return _r8Value.CompareTo( (real8) rhs:_decimalValue)
								case __UsualType.Long		; return _r8Value.CompareTo( (real8) rhs:_intValue)
								case __UsualType.Int64		; return _r8Value.CompareTo( (real8) rhs:_i64Value)
								otherwise
									nop	// uses comparison by type
							end switch
						
						case __UsualType.Long
							// Upscale when needed to avoid overflow errors
							switch rhs:UsualType
								case __UsualType.Date		; return _intValue.CompareTo((int) rhs:_dateValue)
								case __UsualType.Int64		; return ((int64)_intValue).CompareTo(rhs:_i64Value)
								case __UsualType.Float		; return ((real8)_intValue).CompareTo(rhs:_r8Value)
								case __UsualType.Decimal	; return ((System.Decimal)_intValue).CompareTo(rhs:_decimalValue)
								otherwise
									nop	// uses comparison by type
							end switch
						
						case __UsualType.Int64
							switch rhs:UsualType
								case __UsualType.Date		; return _i64Value.CompareTo((int) rhs:_dateValue)
								case __UsualType.Long		; return _i64Value.CompareTo( rhs:_intValue)
								case __UsualType.Float		; return _i64Value.CompareTo( rhs:_r8Value)
								case __UsualType.Decimal	; return _i64Value.CompareTo( rhs:_decimalValue)
								otherwise
									nop	// uses comparison by type
							end switch
						
						case __UsualType.Decimal
							switch rhs:UsualType
								case __UsualType.Date		; return _decimalValue.CompareTo((int) rhs:_dateValue)
								case __UsualType.Long		; return _decimalValue.CompareTo(rhs:_intValue)
								case __UsualType.Float		; return _decimalValue.CompareTo(rhs:_r8Value)
								case __UsualType.Int64		; return _decimalValue.CompareTo(rhs:_i64Value)
								otherwise
									nop	// uses comparison by type
							end switch
					end switch 
				endif
				if rhs:UsualType == __UsualType.Void
					return 1
				elseif self:UsualType > rhs:UsualType
					return 1
				elseif self:UsualType < rhs:UsualType
					return -1
				endif
				return 0
			
			
			
		#endregion
		
		#region Comparison Operators 
			static operator >(lhs as __Usual, rhs as __Usual) as logic
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
					
					case __UsualType.Int64
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
							case __UsualType.Decimal	; return lhs:_r8Value > (real8) rhs:_decimalValue
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
					
					case __UsualType.String
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
			
			static operator >=(lhs as __Usual, rhs as __Usual) as logic
				switch lhs:usualType
					case __UsualType.Long
						switch rhs:UsualType	
							case __UsualType.Long		; return lhs:_intValue >= rhs:_intValue
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
							case __UsualType.Decimal	; return lhs:_r8Value >= (real8) rhs:_decimalValue
							otherwise
								throw BinaryError(">=", "Argument not numeric", false, lhs, rhs)
						end switch
					
					case __UsualType.Decimal
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
			
			static operator <(lhs as __Usual, rhs as __Usual) as logic
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
							case __UsualType.Decimal	; return lhs:_r8Value < (real8) rhs:_decimalValue
							otherwise
								throw BinaryError("<", "Argument not numeric", false, lhs, rhs)
						end switch
					
					case __UsualType.Decimal
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
			
			static operator <=(lhs as __Usual, rhs as __Usual) as logic
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
							case __UsualType.Decimal	; return lhs:_r8Value <= (real8) rhs:_decimalValue
							otherwise
								throw BinaryError("<=", "Argument not numeric", false, lhs, rhs)
						end switch
					
					case __UsualType.Decimal
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

			static operator ==(lhs as __Usual, rhs as __Usual) as logic
				return lhs:UsualEquals(rhs, "==")
			
			static operator !=(lhs as __Usual, rhs as __Usual) as logic
				if lhs:UsualType == __UsualType.STRING .and. rhs:UsualType == __UsualType.STRING
					// Todo __StringEquals
					return ! String.Equals(  lhs:_stringValue, rhs:_stringValue)
				else
					return ! lhs:UsualEquals(rhs, "!=")
				endif
			
			method UsualEquals( rhs as __Usual, operator as string) as logic
				switch self:UsualType
					case __UsualType.Object
						if rhs:UsualType == __UsualType.Object
							return _refData == rhs:_refData
						else
							nop
						endif
					
					case __UsualType.Void
						return rhs:UsualType == __UsualType.Void
					
					case __UsualType.Long
						switch rhs:UsualType
							case __UsualType.Long		; return _intValue == rhs:_intValue
							case __UsualType.Int64		; return (int64) _intValue == rhs:_i64Value	// cast lhs to int64 to avoid overflow 
							case __UsualType.Float		; return (real8) _intValue == rhs:_r8Value // cast lhs to real8 to avoid overflow 
							case __UsualType.Decimal	; return (System.Decimal) _intValue == rhs:_decimalValue	// cast lhs to decimal to avoid overflow 
							case __UsualType.Logic		; return rhs:_logicValue == (self:_intValue <> 0)
							otherwise
								nop
						end switch
					
					case __UsualType.Int64
						switch rhs:UsualType
							case __UsualType.Long		; return _i64Value == (int64) rhs:_intValue
							case __UsualType.Int64		; return _i64Value == rhs:_i64Value
							case __UsualType.Float		; return  (real8) _i64Value == rhs:_r8Value
							case __UsualType.Decimal	; return _i64Value == rhs:_decimalValue
							case __UsualType.Logic		; return rhs:_logicValue == (self:_i64Value <> 0)
							otherwise
								nop
						end switch
					
					case __UsualType.Float
						switch rhs:UsualType
							case __UsualType.Long		; return self:_r8Value == (real8) rhs:_intValue
							case __UsualType.Int64		; return self:_r8Value == (real8) rhs:_i64Value
							case __UsualType.Float		; return self:_r8Value ==  rhs:_r8Value
							case __UsualType.Decimal	; return self:_r8Value ==  (real8) rhs:_decimalValue
							otherwise
								nop
						end switch
					
					case __UsualType.Decimal
						switch rhs:UsualType
							case __UsualType.Long		; return self:_decimalValue == rhs:_intValue
							case __UsualType.Int64		; return self:_decimalValue == rhs:_i64Value
							case __UsualType.Float		; return self:_decimalValue == (System.Decimal) rhs:_r8Value
							case __UsualType.Decimal	; return self:_decimalValue == rhs:_decimalValue
							otherwise
								nop
						end switch
					
					case __UsualType.LOGIC
						switch rhs:UsualType
							case __UsualType.LOGIC		; return self:_logicValue == rhs:_logicValue
							case __UsualType.Long		; return self:_logicValue == (rhs:_intValue <> 0)
							case __UsualType.Int64		; return self:_logicValue == (rhs:_i64Value <> 0)
							case __UsualType.Decimal	; return self:_logicValue == (rhs:_decimalValue <> 0)
							otherwise
								nop
						end switch
					
					case __UsualType.DATE
						switch rhs:UsualType
							case __UsualType.DATE		; return self:_dateValue == rhs:_dateValue
							case __UsualType.DateTime	; return self:_dateValue == (__VoDate) rhs:_dateTimeValue
							otherwise
								nop
						end switch
					
					case __UsualType.DateTime
						switch rhs:UsualType
							case __UsualType.DateTime	; return self:_dateTimeValue == rhs:_dateTimeValue
							case __UsualType.DATE		; return self:_dateTimeValue == (DateTime) rhs:_dateValue
							otherwise
								nop
						end switch
					
					case __UsualType.STRING
						switch rhs:UsualType
							case __UsualType.STRING		; return self:_stringValue== rhs:_stringValue
							case __UsualType.Symbol		; return self:_stringValue == rhs:_symValue
							otherwise
								nop
						end switch
					
					case __UsualType.ARRAY
						switch rhs:UsualType
							case __UsualType.ARRAY		; return self:_refData == rhs:_refData
							otherwise
								nop
						end switch
					
					case __UsualType.CodeBlock
						switch rhs:UsualType
							case __UsualType.CodeBlock	; return self:_refData == rhs:_refData
							otherwise
								nop
						end switch
					
					case __UsualType.Ptr
						switch rhs:UsualType
							case __UsualType.Ptr		; return self:_ptrValue == rhs:_ptrValue
							otherwise
								nop
						end switch
					
					case __UsualType.Symbol
						switch rhs:UsualType
							case __UsualType.Symbol		; return self:_symValue == rhs:_symValue
							case __UsualType.String		; return self:_symValue == rhs:_stringValue
							otherwise
								nop
						end switch
					otherwise
						throw BinaryError(operator, "Arguments Incompatible", true, self, rhs)
					
				end switch
				throw BinaryError(operator, "Arguments Incompatible", false, self, rhs)
			
		#endregion
		
		#region Unary Operators
			static operator !(u as __Usual) as logic
				if u:UsualType == __UsualType.LOGIC
					return !u:_logicValue
				endif
				throw UnaryError("!", u)
			
			static operator ~(u as __Usual) as __Usual
				if u:UsualType == __UsualType.Long
					return ~u:_intValue
				endif
				if u:UsualType == __UsualType.Int64
					return ~u:_i64Value
				endif
				throw UnaryError("~", u)
			
			static operator -(u as __Usual) as __Usual
				switch u:UsualType
					case __UsualType.LONG		; return -u:_intValue
					case __UsualType.Int64		; return -u:_i64Value
					case __UsualType.Float		; return -u:_floatValue
					case __UsualType.Decimal	; return -u:_decimalValue
					otherwise
						throw UnaryError("-", u)
				end switch
			
			static operator +(u as __Usual) as __Usual
				switch u:UsualType
					case __UsualType.LONG		; return u:_intValue
					case __UsualType.Int64		; return u:_i64Value
					case __UsualType.Float		; return u:_floatValue
					case __UsualType.Decimal	; return u:_decimalValue
					otherwise
						throw UnaryError("+", u)
				end switch
			
			static operator --(u as __Usual) as __Usual
				switch u:UsualType
					case __UsualType.LONG		; return u:_intValue - 1
					case __UsualType.Int64		; return u:_i64Value - 1
					case __UsualType.Float		; return u:_floatValue -1
					case __UsualType.Decimal	; return u:_decimalValue - 1 
					otherwise					
						throw UnaryError("--", u)
				end switch
			
			static operator ++(u as __Usual) as __Usual
				switch u:UsualType
					case __UsualType.LONG		; return u:_intValue + 1	
					case __UsualType.Int64		; return u:_i64Value + 1
					case __UsualType.Float		; return u:_floatValue +1
					case __UsualType.Decimal	; return u:_decimalValue + 1 
					otherwise
						throw UnaryError("++", u)
				end switch
			
		#endregion
		#region Numeric Operators for Add, Delete etc (also for strings)
			
			static operator +(lhs as __Usual, rhs as __Usual) as __Usual
				switch lhs:UsualType
					case __UsualType.Long
						switch rhs:UsualType	
							case __UsualType.Long		; return lhs:_intValue + rhs:_intValue 
							case __UsualType.Int64		; return lhs:_intValue + rhs:_i64Value
							case __UsualType.Float		; return lhs:_intValue + rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_intValue + rhs:_decimalValue
							otherwise					; nop
						end switch
					
					case __UsualType.Int64
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_i64Value + rhs:_intValue 
							case __UsualType.Int64		; return lhs:_i64Value + rhs:_i64Value
							case __UsualType.Float		; return lhs:_i64Value + rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_i64Value + rhs:_decimalValue
							otherwise					; nop
						end switch
					
					case __UsualType.Float
						switch rhs:UsualType
							case __UsualType.Long		; return __VoFloat{lhs:_r8Value + rhs:_intValue, lhs:_width, lhs._decimals}
							case __UsualType.Int64		; return __VoFloat{lhs:_r8Value + rhs:_i64Value, lhs:_width, lhs._decimals}
							case __UsualType.Float		; return __VoFloat{lhs:_r8Value + rhs:_r8Value, lhs:_width, lhs._decimals}
							case __UsualType.Decimal	; return __VoFloat{lhs:_r8Value + (real8) rhs:_decimalValue, lhs:_width, lhs._decimals}
							otherwise					; nop
						end switch
					
					case __UsualType.Decimal
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_decimalValue + rhs:_intValue 
							case __UsualType.Int64		; return lhs:_decimalValue + rhs:_i64Value
							case __UsualType.Float		; return lhs:_decimalValue + (System.Decimal) rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_decimalValue + rhs:_decimalValue
							otherwise					; nop
						end switch
					
					case __UsualType.String
						switch rhs:UsualType
							case __UsualType.String		; return lhs:_stringValue+ rhs:_stringValue
							otherwise
								throw BinaryError("+", "Argument Not String", false, lhs, rhs)
						end switch
					case __UsualType.Date
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_dateValue + rhs:_intValue 
							case __UsualType.Int64		; return lhs:_dateValue + rhs:_i64Value
							case __UsualType.Float		; return lhs:_dateValue + rhs:_r8Value
							otherwise
								throw BinaryError("+", "Argument Not Numeric", false, lhs, rhs)
						end switch
					
					otherwise
						throw BinaryError("+", "Invalid Arguments", true, lhs, rhs)
				end switch
				throw BinaryError("+", "Argument Not Numeric", false, lhs, rhs)
			
			static operator -(lhs as __Usual, rhs as __Usual) as __Usual
				switch lhs:UsualType
					case __UsualType.Long
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_intValue - rhs:_intValue 
							case __UsualType.Int64		; return lhs:_intValue - rhs:_i64Value
							case __UsualType.Float		; return lhs:_intValue - rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_intValue - rhs:_decimalValue
							otherwise					; nop
						end switch
					case __UsualType.Int64
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_i64Value - rhs:_intValue 
							case __UsualType.Int64		; return lhs:_i64Value - rhs:_i64Value
							case __UsualType.Float		; return lhs:_i64Value - rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_i64Value - rhs:_decimalValue
							otherwise					; nop
						end switch
					case __UsualType.Float
						switch rhs:UsualType
							case __UsualType.Long		; return __VoFloat{lhs:_r8Value - rhs:_intValue ,lhs:_width, lhs._decimals}
							case __UsualType.Int64		; return __VoFloat{lhs:_r8Value - rhs:_i64Value ,lhs:_width, lhs._decimals}
							case __UsualType.Float		; return __VoFloat{lhs:_r8Value - rhs:_r8Value	,lhs:_width, lhs._decimals}
							case __UsualType.Decimal	; return __VoFloat{lhs:_r8Value - (real8) rhs:_decimalValue ,lhs:_width, lhs._decimals}
							otherwise					; nop
						end switch
					
					case __UsualType.Decimal
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_decimalValue - rhs:_intValue 
							case __UsualType.Int64		; return lhs:_decimalValue - rhs:_i64Value
							case __UsualType.Float		; return lhs:_decimalValue - (System.Decimal) rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_decimalValue - rhs:_decimalValue
							otherwise					; nop
						end switch
					
					case __UsualType.String
						switch rhs:UsualType
							case __UsualType.String		; return CompilerServices.__StringSubtract(lhs, rhs)
							otherwise					; throw BinaryError("-", "Argument Not String", false, lhs, rhs)
				end switch
					case __UsualType.Date
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_dateValue - rhs:_intValue 
							case __UsualType.Int64		; return lhs:_dateValue - rhs:_i64Value
							case __UsualType.Float		; return lhs:_dateValue - rhs:_r8Value
							case __UsualType.Date		; return lhs:_dateValue - rhs:_dateValue
							case __UsualType.DateTime	; return lhs:_dateValue - (__VoDate) rhs:_dateTimeValue
							otherwise					; nop
						end switch
					
					case __UsualType.DateTime
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_dateTimeValue:Subtract(TimeSpan{rhs:_intValue,0,0,0})
							case __UsualType.Int64		; return lhs:_dateTimeValue:Subtract( TimeSpan{(int)rhs:_i64Value,0,0,0})
							case __UsualType.Float		; return lhs:_dateTimeValue:Subtract( TimeSpan{(int)rhs:_r8Value,0,0,0})
							case __UsualType.Date		; return lhs:_dateTimeValue:Subtract((DateTime) rhs:_dateValue):Days
							case __UsualType.DateTime	; return lhs:_dateTimeValue:Subtract( rhs:_dateTimeValue):Days
							otherwise					; nop
						end switch
					
					otherwise
						throw BinaryError("-", "Invalid Arguments", true, lhs, rhs)
				end switch
				throw BinaryError("-", "Argument Not Numeric", false, lhs, rhs)
			static operator /(lhs as __Usual, rhs as __Usual) as __Usual
				
				switch lhs:UsualType
					
					case __UsualType.Long
						switch rhs:UsualType
							case __UsualType.Long
								local result as int
								local remainder as int
								result := Math.DivRem(lhs:_intValue, rhs:_intValue, out remainder)
								if remainder == 0
									return result
								else
									return lhs:_intValue / rhs:_intValue
								endif
							case __UsualType.Int64
								local result as int64
								local remainder as int64
								result := Math.DivRem((int64) lhs:_intValue, rhs:_i64Value, out remainder)
								if remainder == 0
									return result
								else
									return lhs:_intValue / rhs:_i64Value
								endif
							case __UsualType.Float
								return __VoFloat{lhs:_intValue / rhs:_r8Value, rhs:_width, rhs:_decimals}
							
							case __UsualType.Decimal
								local result as int64
								local remainder as int64
								result := Math.DivRem((int64) lhs:_intValue, (int64) rhs:_decimalValue, out remainder)
								if remainder == 0
									return result
								else
									return lhs:_intValue / rhs:_decimalValue
								endif
							otherwise
								nop
						end switch
					
					case __UsualType.Int64
						switch rhs:UsualType
							case __UsualType.Long
								local result as int64
								local remainder as int64
								result := Math.DivRem(lhs:_i64Value, rhs:_intValue, out remainder)
								if remainder == 0
									return result
								else
									return lhs:_i64Value / rhs:_intValue
								endif
							case __UsualType.Int64
								local result as int64
								local remainder as int64
								result := Math.DivRem( lhs:_i64Value, rhs:_i64Value, out remainder)
								if remainder == 0
									return result
								else
									return lhs:_i64Value / rhs:_i64Value
								endif
							case __UsualType.Float
								return __VoFloat{lhs:_i64Value / rhs:_r8Value, rhs:_width, rhs:_decimals}
							case __UsualType.Decimal
								local result as int64
								local remainder as int64
								result := Math.DivRem(lhs:_i64Value, (int64) rhs:_decimalValue, out remainder)
								if remainder == 0
									return result
								else
									return lhs:_i64Value / rhs:_decimalValue
								endif
							otherwise
								nop
						end switch
					
					case __UsualType.Float
						switch rhs:UsualType
							case __UsualType.Long		; return __VoFloat{lhs:_r8Value / rhs:_intValue, lhs:_width, lhs:_decimals}
							case __UsualType.Int64		; return __VoFloat{lhs:_r8Value / rhs:_i64Value, lhs:_width, lhs:_decimals}
							case __UsualType.Float		; return __VoFloat{lhs:_r8Value / rhs:_r8Value, Math.Max(lhs:_width,rhs:_width), lhs:_decimals+ rhs:_decimals}
							case __UsualType.Decimal	; return __VoFloat{lhs:_r8Value / (real8) rhs:_decimalValue, lhs:_width, lhs:_decimals}
							otherwise					; nop
						end switch
					
					case __UsualType.Decimal
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_decimalValue / rhs:_intValue
							case __UsualType.Int64		; return lhs:_decimalValue / rhs:_i64Value
							case __UsualType.Float		; return lhs:_decimalValue / (System.Decimal) rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_decimalValue / rhs:_decimalValue
							otherwise					; nop
						end switch
					
					otherwise
						throw BinaryError("/", "Invalid Arguments", true, lhs, rhs)
				end switch
				throw BinaryError("/", "Argument Not Numeric", false, lhs, rhs)
			
			static operator %(lhs as __Usual, rhs as __Usual) as __Usual
				switch lhs:UsualType
					case __UsualType.Long
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_intValue % rhs:_intValue
							case __UsualType.Int64		; return lhs:_intValue % rhs:_i64Value
							case __UsualType.Float		; return __VoFloat{lhs:_intValue % rhs:_r8Value, rhs:_width, rhs:_decimals}
							case __UsualType.Decimal	; return lhs:_intValue % rhs:_decimalValue
							otherwise					; nop
					end switch
					
					case __UsualType.Int64
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_i64Value % rhs:_intValue
							case __UsualType.Int64		; return lhs:_i64Value % rhs:_i64Value
							case __UsualType.Float		; return __VoFloat{lhs:_i64Value % rhs:_r8Value, rhs:_width, rhs:_decimals}
							case __UsualType.Decimal	; return lhs:_i64Value % rhs:_decimalValue
							otherwise					; nop
					end switch
					
					case __UsualType.Float
						switch rhs:UsualType
							case __UsualType.Long		; return __VoFloat{lhs:_r8Value % rhs:_intValue, lhs:_width, lhs:_decimals}
							case __UsualType.Int64		; return __VoFloat{lhs:_r8Value % rhs:_i64Value, lhs:_width, lhs:_decimals}
							case __UsualType.Float		; return __VoFloat{lhs:_r8Value % rhs:_r8Value, Math.Max(lhs:_width,rhs:_width), lhs:_decimals+ rhs:_decimals}
							case __UsualType.Decimal	; return __VoFloat{lhs:_r8Value % (real8) rhs:_decimalValue, lhs:_width, lhs:_decimals}
							otherwise					; nop
						end switch
					
					case __UsualType.Decimal
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_decimalValue % rhs:_intValue
							case __UsualType.Int64		; return lhs:_decimalValue % rhs:_i64Value
							case __UsualType.Float		; return lhs:_decimalValue % (System.Decimal) rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_decimalValue %  rhs:_decimalValue
							otherwise					; nop
						end switch
					
					
					otherwise
						throw BinaryError("%", "Invalid Arguments", true, lhs, rhs)
				end switch
				throw BinaryError("%", "Argument Not Numeric", false, lhs, rhs)
			
			static operator *(lhs as __Usual, rhs as __Usual) as __Usual
				switch lhs:UsualType
					case __UsualType.Long
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_intValue * rhs:_intValue
							case __UsualType.Int64		; return lhs:_intValue * rhs:_i64Value
							case __UsualType.Float		; return __VoFloat{lhs:_intValue * rhs:_r8Value, rhs:_width, rhs:_decimals}
							case __UsualType.Decimal	; return lhs:_intValue * rhs:_decimalValue
							otherwise					; nop
						end switch
					
					case __UsualType.Int64
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_i64Value * rhs:_intValue
							case __UsualType.Int64		; return lhs:_i64Value * rhs:_i64Value
							case __UsualType.Float		; return __VoFloat{lhs:_i64Value * rhs:_r8Value, rhs:_width, rhs:_decimals}
							case __UsualType.Decimal	; return lhs:_i64Value * rhs:_decimalValue
							otherwise					; nop
						end switch
					
					case __UsualType.Float
						switch rhs:UsualType		
							case __UsualType.Long		; return __VoFloat{lhs:_r8Value * rhs:_intValue, lhs:_width, lhs:_decimals}
							case __UsualType.Int64		; return __VoFloat{lhs:_r8Value * rhs:_i64Value, lhs:_width, lhs:_decimals}
							case __UsualType.Float		; return __VoFloat{lhs:_r8Value * rhs:_r8Value, Math.Max(lhs:_width,rhs:_width), lhs:_decimals+ rhs:_decimals}
							case __UsualType.Decimal	; return __VoFloat{lhs:_r8Value * (real8) rhs:_decimalValue, lhs:_width, lhs:_decimals}
							otherwise					; nop
						end switch
					
					case __UsualType.Decimal
						switch rhs:UsualType
							case __UsualType.Long		; return lhs:_decimalValue * rhs:_intValue
							case __UsualType.Int64		; return lhs:_decimalValue * rhs:_i64Value
							case __UsualType.Float		; return lhs:_decimalValue * (System.Decimal) rhs:_r8Value
							case __UsualType.Decimal	; return lhs:_decimalValue *  rhs:_decimalValue
							otherwise					; nop
						end switch
					
					otherwise
						throw BinaryError("*", "Invalid Arguments", true, lhs, rhs)
				end switch
				throw BinaryError("*", "Argument Not Numeric", false, lhs, rhs)
			
			static operator >>(lhs as __Usual, rhs as int) as __Usual
				// Right shift
				switch lhs:UsualType
					case __UsualType.Long	; return lhs:_intValue >> rhs
					case __UsualType.Int64	; return lhs:_i64Value >> rhs
					otherwise				
						throw BinaryError(">>", "Argument not Integer", true, lhs, rhs)
				end switch
			
			static operator <<(lhs as __Usual, rhs as long) as __Usual
				// Left shift
				switch (lhs:UsualType)
					case __UsualType.Long	; return lhs:_intValue << rhs
					case __UsualType.Int64	; return lhs:_i64Value << rhs
					otherwise
				throw BinaryError("<<", "Argument not Integer", true, lhs, rhs)
				end switch
			
			
			static operator &(lhs as __Usual, rhs as __Usual) as __Usual
				// Bitwise And
				switch (lhs:UsualType)
					case __UsualType.Long
						switch (rhs:UsualType)
							case __UsualType.Long		; return lhs:_intValue & rhs:_intValue
							case __UsualType.Int64		; return (int64) lhs:_intValue & rhs:_i64Value
							otherwise					; nop
						end switch
					case __UsualType.Int64
						switch (rhs:UsualType)
							case __UsualType.Long		; return lhs:_i64Value & (int64) rhs:_intValue
							case __UsualType.Int64		; return  lhs:_i64Value & rhs:_i64Value
							otherwise					; nop
						end switch
					otherwise
						throw BinaryError("&", "Argument not Integer", true, lhs, rhs)
				end switch
				throw BinaryError("&", "Argument not Integer", false, lhs, rhs)
			
			static operator |(lhs as __Usual, rhs as __Usual) as __Usual
				// Bitwise or
				switch (lhs:UsualType)
					case __UsualType.Long
						switch (rhs:UsualType)
							case __UsualType.Long		; return lhs:_intValue | rhs:_intValue
							case __UsualType.Int64		; return (int64) lhs:_intValue | rhs:_i64Value
							otherwise					; nop
						end switch
					case __UsualType.Int64
						switch (rhs:UsualType)
							case __UsualType.Long		; return lhs:_i64Value | (int64) rhs:_intValue
							case __UsualType.Int64		; return  lhs:_i64Value | rhs:_i64Value
							otherwise					; nop
						end switch
					otherwise
						throw BinaryError("|", "Argument not Integer", true, lhs, rhs)
				end switch
				throw BinaryError("|", "Argument not Integer", false, lhs, rhs)
		#endregion
		
		#region Implicit From Usual to Other Type
			
			static operator implicit(u as __Usual) as __Array
				switch u:UsualType
					case __UsualType.Array	; return (__Array) u:_refData
					case __UsualType.Void	; return null
					case __UsualType.Object	
						if u:_refData== null
							return null
						elseif u:_refData is __Array
							return (__Array) u:_refData
						endif
				end switch
				throw ConversionError("ARRAY", typeof(__Array), u)
			
			// Todo
			//STATIC OPERATOR IMPLICIT(u AS __Usual) AS CodeBlock
			//	THROW NotImplementedException{}
			
			static operator implicit(u as __Usual) as logic
				switch u:UsualType
					case __UsualType.Logic		; return u:_logicValue
					case __UsualType.Long		; return u:_intValue != 0
					case __UsualType.Int64		; return u:_i64Value != 0
					case __UsualType.Decimal	; return u:_decimalValue != 0
					case __UsualType.Void		; return false
					otherwise
						throw ConversionError("LOGIC", typeof(logic), u)
				end switch
			
			static operator implicit(u as __Usual) as __VODate
				switch u:UsualType
					case __UsualType.Date		; return u:_dateValue
					case __UsualType.DateTime	; return (__VoDate) u:_dateTimeValue
					case __UsualType.Void		; return __VODate{0}
					otherwise
						throw ConversionError("DATE", typeof(__VODate), u)
				end switch
			
			static operator implicit(u as __Usual) as DateTime
				switch u:UsualType
					case __UsualType.Date		; return (DateTime) u:_dateValue
					case __UsualType.DateTime	; return u:_dateTimeValue
					case __UsualType.Void		; return DateTime.MinValue
					otherwise
						throw ConversionError("DATE", typeof(__VODate), u)
				end switch
			
			static operator implicit(u as __Usual) as System.IntPtr
				switch u:UsualType
					case __UsualType.Ptr		; return u:_ptrValue
					case __UsualType.LONG		; return (IntPtr) u:_intValue
					case __UsualType.Int64		; return (IntPtr) u:_i64Value
					case __UsualType.Decimal	; return (IntPtr) u:_decimalValue 
					case __UsualType.Void		; return IntPtr.Zero
					otherwise
						throw ConversionError("PTR", typeof(IntPtr), u)
				end switch
			
			static operator implicit(u as __Usual) as string
				switch u:UsualType
					case __UsualType.String	; return u:_stringValue
					case __UsualType.Void	; return ""
					case __UsualType.SYMBOL	; return (string) u:_symValue
					otherwise
						throw ConversionError("STRING", typeof(string), u)
				end switch
			
			static operator implicit(u as __Usual) as __Symbol
				switch u:UsualType
					case __UsualType.String	; return (__Symbol) u:_stringValue
					case __UsualType.Void	; return __Symbol{""}
					case __UsualType.SYMBOL	; return u:_symValue
					otherwise
						throw ConversionError("SYMBOL", typeof(__Symbol), u)
				end switch
			
			static operator implicit(u as __Usual) as __Psz
				switch u:UsualType
					case __UsualType.Ptr	; return (__Psz) u:_ptrValue
					case __UsualType.String	; return __Psz{u:_stringValue}
					case __UsualType.Void	; return __Psz._Null_Psz
					otherwise
						throw ConversionError("PSZ", typeof(__PSZ), u)
				end switch
			
		#endregion
		#region Implicit Numeric Operators
			static operator implicit(u as __Usual) as byte
				try
					begin checked
						// todo use CompilerOptionVO11 for types with decimal
						switch u:UsualType
							case __UsualType.Long		; return (byte) u:_intValue
							case __UsualType.Int64		; return (byte) u:_i64Value
							case __UsualType.Float		; return (byte) u:_r8Value
							case __UsualType.Logic		; return iif(u:_logicValue, 1, 0)
							case __UsualType.Decimal	; return (byte) u:_decimalValue 
							case __UsualType.Void		; return 0
							otherwise
								throw ConversionError("BYTE", typeof(byte), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "BYTE", typeof(byte), u)
				end try
			
			static operator implicit(u as __Usual) as short
				try
					begin checked
							// todo use CompilerOptionVO11 for types with decimal
							switch u:UsualType
							case __UsualType.Long	; return (short) u:_intValue
							case __UsualType.Int64	; return (short) u:_i64Value
							case __UsualType.Float	; return (short) u:_r8Value
							case __UsualType.Decimal; return (short) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("SHORT", typeof(short), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "SHORT", typeof(short), u)
				end try
			
			static operator implicit(u as __Usual) as long
				try
					begin checked
							// todo use CompilerOptionVO11 for types with decimal
						switch u:UsualType
							case __UsualType.Long	; return u:_intValue
							case __UsualType.Int64	; return (long) u:_i64Value
							case __UsualType.Float	; return (long) u:_r8Value
							case __UsualType.Decimal; return (long) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("LONG", typeof(long), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "LONG", typeof(long), u)
				end try
			
			static operator implicit(u as __Usual) as int64
				try
					begin checked
							// todo use CompilerOptionVO11 for types with decimal
						switch u:UsualType
							case __UsualType.Long	; return u:_intValue
							case __UsualType.Int64	; return (int64) u:_i64Value
							case __UsualType.Float	; return (int64) u:_r8Value
							case __UsualType.Decimal; return (int64) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
						throw ConversionError("INT64", typeof(int64), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "INT64", typeof(int64), u)
				end try
			
			static operator implicit(u as __Usual) as System.Decimal
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return u:_intValue	
							case __UsualType.Int64	; return u:_i64Value
							case __UsualType.Float	; return (System.Decimal) u:_r8Value
							case __UsualType.Decimal; return u:_decimalValue
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("DECIMAL", typeof(int64), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "DECIMAL", typeof(int64), u)
				end try
			
			static operator implicit(u as __Usual) as SByte
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return (SByte) u:_intValue
							case __UsualType.Int64	; return (SByte) u:_i64Value
							case __UsualType.Float	; return (SByte) u:_r8Value
							case __UsualType.Decimal; return (SByte) u:_decimalValue 
							case __UsualType.Logic	; return (SByte) iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("SBYTE", typeof(SByte), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "SBYTE", typeof(SByte), u)
				end try
			
			// Unsigned
			static operator implicit(u as __Usual) as word
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return (word) u:_intValue
							case __UsualType.Int64	; return (word) u:_i64Value
							case __UsualType.Float	; return (word) u:_r8Value
							case __UsualType.Decimal; return (word) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("WORD", typeof(word), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "WORD", typeof(word), u)
				end try
			
			static operator implicit(u as __Usual) as dword
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return (dword) u:_intValue
							case __UsualType.Int64	; return (dword) u:_i64Value
							case __UsualType.Float	; return (dword) u:_r8Value
							case __UsualType.Decimal; return (dword) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("DWORD", typeof(dword), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "DWORD", typeof(dword), u)
				end try
			
			static operator implicit(u as __Usual) as uint64
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return (uint64) u:_intValue
							case __UsualType.Int64	; return (uint64) u:_i64Value
							case __UsualType.Float	; return (uint64) u:_r8Value
							case __UsualType.Decimal; return (uint64) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("UINT64", typeof(uint64), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "UINT64", typeof(uint64), u)
				end try
			
			// Single, Double and FLoat
			static operator implicit(u as __Usual) as real4
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return (real4) u:_intValue
							case __UsualType.Int64	; return (real4) u:_i64Value
							case __UsualType.Float	; return (real4) u:_r8Value
							case __UsualType.Decimal; return (real4) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
						throw ConversionError("REAL4", typeof(real4), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "REAL4", typeof(real4), u)
				end try
			
			static operator implicit(u as __Usual) as real8
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return (real8) u:_intValue
							case __UsualType.Int64	; return (real8) u:_i64Value
							case __UsualType.Float	; return (real8) u:_r8Value
							case __UsualType.Decimal; return (real8) u:_decimalValue 
							case __UsualType.Logic	; return iif(u:_logicValue, 1, 0)
							case __UsualType.Void	; return 0
							otherwise
								throw ConversionError("REAL8", typeof(real8), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "REAL8", typeof(real8), u)
				end try
			
			static operator implicit(u as __Usual) as __VoFloat
				try
					begin checked
						switch u:UsualType
							case __UsualType.Long	; return __VoFloat{(real8) u:_intValue}
							case __UsualType.Int64	; return __VoFloat{(real8) u:_i64Value}
							case __UsualType.Float	; return __VoFloat{(real8) u:_r8Value, u:_flags:Width, u:_flags:Decimals}
							case __UsualType.Decimal; return __VoFloat{(real8) u:_decimalValue}
							case __UsualType.Logic	; return __VoFloat{iif(u:_logicValue, 1, 0)}
							case __UsualType.Void	; return __VoFloat{0}
							otherwise
								throw ConversionError("FLOAT", typeof(__VoFloat), u)
						end switch
					end checked
				catch ex as OverflowException
					throw OverflowError(ex, "FLOAT", typeof(__VoFloat), u)
				end try
			
		#endregion
		#region Implicit from Other Type to Usual
			static operator implicit(value as logic) as __Usual
				return __Usual{value}
			
			static operator implicit(value as byte) as __Usual
				return __Usual{(int)value}
			
			static operator implicit(value as __Array) as __Usual
				return __Usual{value}
			
			static operator implicit(value as __VoDate) as __Usual
				return __Usual{value}
			
			static operator implicit(value as System.DateTime) as __Usual
				return __Usual{value}
			
			static operator implicit(value as __VoFloat) as __Usual
				return __Usual{value}
			
			static operator implicit(value as real8) as __Usual
				return __Usual{value}
			
			static operator implicit(value as short) as __Usual
				return __Usual{(int)value}
			
			static operator implicit(value as long) as __Usual
				return __Usual{value}
			
			static operator implicit(value as int64) as __Usual
				return __Usual{value}
			
			static operator implicit(value as uint64) as __Usual
				return __Usual{value}
			
			static operator implicit(value as System.Decimal) as __Usual
				return __Usual{value}
			
			static operator implicit(value as System.IntPtr) as __Usual
				return __Usual{value}
			
			static operator implicit(value as SByte) as __Usual
				return __Usual{(int)value}
			
			static operator implicit(value as real4) as __Usual
				return __Usual{(real8)value }
			
			static operator implicit(value as string) as __Usual
				return __Usual{value}
			
			static operator implicit(value as word) as __Usual
				return __Usual{(int)value}
			
			static operator implicit(value as dword) as __Usual
				return iif((value <= 0x7fffffff),__Usual{(long)value },__Usual{(__VoFloat)value })
		#endregion
		
		#region implementation IConvertable
			public method ToBoolean(provider as System.IFormatProvider) as logic
				return self
			
			public method ToByte(provider as System.IFormatProvider) as byte
				return self
			
			public method ToChar(provider as System.IFormatProvider) as char
				var o := __Usual.ToObject(self)
				if o is IConvertible
					return ((IConvertible)o):ToChar(provider)
				endif
				throw InvalidCastException{}
			
			public method ToDateTime(provider as System.IFormatProvider) as System.DateTime
				return (__VoDate) self
			
			public method ToDecimal(provider as System.IFormatProvider) as Decimal
				return self
			
			public method ToDouble(provider as System.IFormatProvider) as real8
				return self
			
			public method ToInt16(provider as System.IFormatProvider) as short
				return self
			
			public method ToInt32(provider as System.IFormatProvider) as long
				return self
			
			public method ToInt64(provider as System.IFormatProvider) as int64
				return self
			
			static method ToObject(u as __Usual) as object
				switch u:UsualType
					case __UsualType.ARRAY		; return u:_refData
					case __UsualType.CodeBlock	; return u:_refData			
					case __UsualType.Date		; return u:_dateValue
					case __UsualType.DateTime	; return u:_dateTimeValue
					case __UsualType.Decimal	; return u:_decimalValue
					case __UsualType.FLOAT		; return __VoFloat{u:_r8Value, u:_width, u:_decimals}
					case __UsualType.Int64		; return u:_i64Value
					case __UsualType.Long		; return u:_intValue
					case __UsualType.LOGIC		; return u:_logicValue
					case __UsualType.OBJECT		; return u:_refData
					case __UsualType.PTR		; return u:_ptrValue
					case __UsualType.STRING		; return u:_refData
					case __UsualType.SYMBOL		; return u:_symValue
					case __UsualType.Void		; return null
					otherwise					; return null
				end switch
			
			public method ToSByte(provider as System.IFormatProvider) as SByte
				return self
			
			public method ToSingle(provider as System.IFormatProvider) as real4
				return self
			
			public method ToString() as string
				local strResult as string
				
				switch (self:usualType)
					case __UsualType.Array
					case __UsualType.CODEBLOCK
					case __UsualType.OBJECT	; strResult := self:_refData:ToString()
					case __UsualType.Date	; strResult := self:_dateValue:ToString()
					case __UsualType.DateTime; strResult := self:_dateTimeValue:ToString()
					case __UsualType.Decimal; strResult := self:_decimalValue:ToString()
					case __UsualType.Float	; strResult := self:_r8Value:ToString()
					case __UsualType.Long	; strResult := self:_intValue:ToString()
					case __UsualType.Int64	; strResult := self:_i64Value:ToString()
					case __UsualType.LOGIC	; strResult := iif(!self:_logicValue , ".F." , ".T.")
					case __UsualType.PTR	; strResult := self:_ptrValue:ToString()
					case __UsualType.STRING	; strResult := self:_stringValue
					case __UsualType.Symbol	; strResult := self:_symValue:ToString()
					case __UsualType.Void	; strResult := "NIL"
					otherwise				; strResult := ""
				end switch
				return strResult
			
			
			public method ToString(provider as System.IFormatProvider) as string
				return self:ToString()
			
			public method ToType(conversionType as System.Type, provider as System.IFormatProvider) as object
				if conversionType:IsPointer
					switch self:UsualType	
						case __UsualType.PTR	; return _ptrValue
						case __UsualType.Long	; return (IntPtr) _intValue
						case __UsualType.Int64	; return (IntPtr) _i64Value
						otherwise	
							throw InvalidCastException{}
					end switch
				else
					var o := __Usual:ToObject(self)
					if conversionType:IsAssignableFrom(o:GetType())
						return o
					elseif o is IConvertible
						return ((IConvertible) o):Totype(conversionType, provider)
					else
						throw InvalidCastException{}
					endif
				endif
			
			public method ToUInt16(provider as System.IFormatProvider) as word
				return self
			
			public method ToUInt32(provider as System.IFormatProvider) as dword
				return self
			
			public method ToUInt64(provider as System.IFormatProvider) as uint64
				return self
			
			public method GetTypeCode() as System.TypeCode
				switch UsualType
					case __UsualType.ARRAY	; return TypeCode.Object
					case __UsualType.CodeBlock; return TypeCode.Object			
					case __UsualType.Date	; return TypeCode.Object
					case __UsualType.DateTime; return TypeCode.DateTime
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
			internal method TypeString() as string
				switch self:UsualType
					case __UsualType.Array		; return "ARRAY"
					case __UsualType.CodeBlock	; return "CODEBLOCK"
					case __UsualType.Date		; return "DATE"
					case __UsualType.DateTime	; return "DATETIME"
					case __UsualType.DECIMAL	; return "DECIMAL"
					case __UsualType.FLOAT		; return "FLOAT"
					case __UsualType.Int64		; return "INT64"
					case __UsualType.Long		; return "LONG"
					case __UsualType.Logic		; return "LOGIC"
					case __UsualType.PTR		; return "PTR"
					case __UsualType.String		; return "STRING"
					case __UsualType.Symbol		; return "SYMBOL"
					case __UsualType.Void		; return "USUAL"
					otherwise
						if self:isReferenceType
							if _refData == null
								return ""
							else
								return _refData:GetType():FullName
							endif
						endif
				end switch
				return "?"
			
			static method ConversionError(toTypeString as string, toType as System.Type, u as __Usual) as Error
				var err			:= Error{InvalidCastException{}}
				err:GenCode		:= GenCode.DataType
				err:Severity	:= Severity.Error
				err:ArgTypeReq	:= toType
				err:ArgNum		:= 1
				err:FuncSym		:= "USUAL => "+toTypeString
				err:ArgType		:= toTypeString
				err:Description := i"Conversion Error from USUAL ({u:TypeString()})  to {toTypeString}"  
				err:Arg			:= u:ToString()
				return err
			
			static method OverflowError(ex as OverflowException, toTypeString as string, toType as System.Type, u as __Usual) as Error
				var err := Error{ex}
				err:GenCode		 := GenCode.DataType
				err:Severity	 := Severity.Error
				err:ArgTypeReq	 := toType
				err:ArgNum		 := 1
				err:FuncSym		 := "USUAL => "+toTypeString
				err:ArgType		 := toTypeString
				err:Description  := i"Overflow error converting from USUAL({u:TypeString()})  to {toTypeString}"  
				err:Arg			 := u:ToString()
				return err
			
			static method BinaryError( operator as string, message as string, left as logic, lhs as __Usual, rhs as __Usual) as Error
				var err := Error{ArgumentException{}}
				err:GenCode		 := GenCode.ARG
				err:Severity	 := Severity.Error
				err:ArgNum		 := iif (left, 1, 2)
				err:FuncSym		 := operator
				err:Description  := message
				err:Arg			 := iif(left, lhs:ToString(), rhs:ToString())
				return err
			
			static method UnaryError( operator as string, u as __Usual) as Error
				var err := Error{ArgumentException{}}
				err:GenCode		 := GenCode.ARG
				err:Severity	 := Severity.Error
				err:ArgNum		 := 1
				err:FuncSym		 := operator
				err:Description  := "Invalid Argument Type"
				err:Arg			 := u:ToString()
				return err
			
			
		#endregion
		internal class UsualDebugView
			private _uvalue as __Usual
			public constructor (u as __Usual)
				_uvalue := u
			
			[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
			public property Value as object get _uvalue:VALUE
			public property Type  as __UsualType get _uvalue:UsualType
			
		end class
		*/
	end structure			
	
	
	[StructLayout(LayoutKind.Explicit)];
		public structure __UsualData
		// Fields
		[FieldOffset(0)] export d as __VoDate
		[FieldOffset(0)] export r8 as real8
		[FieldOffset(0)] export i as long
		[FieldOffset(0)] export i64 as int64
		[FieldOffset(0)] export l as logic
		[FieldOffset(0)] export p as System.IntPtr
		[FieldOffset(0)] export s as __Symbol
		[FieldOffset(0)] export dt as System.DateTime
		
	end structure
	
	public enum __UsualType as byte
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
		member @@Real8		:=16
		member @@Psz		:=17
		member @@Ptr		:=18
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
		
		constructor(type as __UsualType)
			usualType := type
	end structure
	/// <summary>
	/// Determine the data type of an expression.
	/// </summary>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	function UsualType(u as __Usual) as dword
		return (dword) u:UsualType
	
	
end namespace