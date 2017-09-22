//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.InteropServices

begin namespace XSharp
	[StructLayout(LayoutKind.Sequential)];
	structure __Usual implements IConvertible,IComparable
		#region static fields
		public static _NIL as __Usual
		#endregion
		#region private fields
		private initonly _flags    	as __Usual_flags	// type, byref, width, decimals
		private _valueData	as __UsualData		// for non GC data
		private _refData  	as Object			// for GC data
		#endregion
		#region constructors
		STATIC Constructor()
			_NIL := __Usual{}
		return

		private  constructor(u as __Usual)
			self:_flags     := u:_flags
			self:_valueData	:= u:_valueData
			SELF:_refData 	:= u:_refData 

		return

		private  constructor(value as Logic)
			self:_flags			:= __Usual_flags{__UsualType.LOGIC}
			self:_valueData		:= __UsualData{}{ l:= value}
		return

		private  constructor(value as __Array)
			self:_flags		:= __Usual_flags{__UsualType.Array}
			self:_refData	:= value
		return

		private  constructor(value as System.DateTime)
			self:_flags		:= __Usual_flags{ __UsualType.Date}
			self:_refData	:= value
		return

		private  constructor(value as Long)
			self:_flags		:= __Usual_flags{__UsualType.INT}
			self:_valueData	:= __UsualData{} {i := value}
		return

		private  constructor(value as Int64)
			self:_flags		:= __Usual_flags{__UsualType.INT64}
			self:_valueData	:= __UsualData{} {i64 := value}
		return

		private  constructor(value as System.IntPtr)
			self:_flags		:= __Usual_flags{__UsualType.PTR}
			self:_valueData	:= __UsualData{} {p := value}
		return

		public constructor(o as Object)
			local u				as __Usual
			local vartype		as System.Type
			local typeCode		as System.TypeCode
			self:_flags			:= __Usual_flags{__UsualType.PTR}
			self:_valueData		:= __UsualData{}
			self:_refData 		    := null
			if (o != null)
				if (o:GetType() == typeof(__Usual))
					// boxed __Usual
					u		:= (__Usual)o 
					self:_refData	:= u:_refData 
					self:_valueData	:= u:_valueData
				else
					//  decode type from typecode
					vartype := o:GetType()
					typeCode := System.Type.GetTypeCode(vartype)
					switch typeCode
					case  System.TypeCode.DBNull
						self:_flags:usualType := __UsualType.Void
						self:_refData	:= null
					case System.TypeCode.Boolean
						self:_flags:usualType := __UsualType.LOGIC
						self:_valueData:l := (Logic)o 
					case System.TypeCode.Char
						self:_flags:usualType		:= __UsualType.INT
						self:_valueData:i	:= (Char)o 
					case System.TypeCode.SByte
						self:_flags:usualType		:= __UsualType.INT
						self:_valueData:i	:= (SByte)o 
					case System.TypeCode.Byte
						self:_flags:usualType		:= __UsualType.INT
						self:_valueData:i	:= (Byte)o 
					case System.TypeCode.Int16 
						self:_flags:usualType		:= __UsualType.INT
						self:_valueData:i	:= (Short)o 
					case System.TypeCode.UInt16
						self:_flags:usualType		:= __UsualType.INT
						self:_valueData:i	:= (Word)o 
					case System.TypeCode.Int32
						self:_flags:usualType		:= __UsualType.INT
						self:_valueData:i	:= (Long)o 
					case System.TypeCode.UInt32
						if ((DWord)o  <= 0x7fffffff)
							self:_flags:usualType := __UsualType.INT
							self:_valueData:i := (Long)(DWord)o  
						else
							self:_flags:usualType := __UsualType.Float
							self:_valueData:r8 := (DWord)o 
						endif
					case System.TypeCode.Int64 
						self:_flags:usualType		:= __UsualType.INT64
						self:_valueData:i64	:= (Int64)o 
					case System.TypeCode.UInt64 
						self:_flags:usualType := __UsualType.INT64
						self:_valueData:i64 := (Int64)(UInt64)o  
					case System.TypeCode.Single  
						self:_flags:usualType		:= __UsualType.Float
						self:_valueData:r8	:= (real8)o 
						self:_flags:width := 255
						self:_flags:decimals := 255
					case System.TypeCode.Double 
						self:_flags:usualType := __UsualType.Float
						self:_valueData:r8 := (real8)o 
						self:_flags:width := 255
						self:_flags:decimals := 255
					case System.TypeCode.Decimal 
						self:_flags:usualType := __UsualType.OBJECT
						self:_refData  := o
					case System.TypeCode.DateTime 
						self:_flags:usualType := __UsualType.Date
						self:_valueData:d := __VoDate{ (System.DateTime) o }
					case System.TypeCode.String 
						self:_flags:usualType := __UsualType.STRING
						SELF:_refData  := (STRING)o 
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

		private  constructor(s as string)
			self:_flags		:= __Usual_flags{__UsualType.STRING}
			self:_valueData	:= __UsualData{}
			self:_refData 	:= s
		return
		#endregion
		#region properties
		PRIVATE  PROPERTY _usualType	AS __UsualType GET _flags:UsualType 
		// properties for floats
		PRIVATE  PROPERTY width			AS BYTE GET _flags:width 
		PRIVATE  PROPERTY decimals		AS BYTE GET _flags:decimals 

 		internal Property UsualType		as __UsualType GET  _usualType
 		internal property IsArray		as Logic GET _usualType == __UsualType.Array
		internal property IsByRef		as Logic GET self:_flags:IsByRef
		internal property IsDate		as Logic GET _usualType == __UsualType.Date
		internal property IsPtr			as Logic GET _usualType == __UsualType.PTR
		internal property IsString		as Logic GET _usualType == __UsualType.STRING
		internal property IsLong		as Logic GET _usualType == __UsualType.INT64
		internal property IsFloat		as Logic GET _usualType == __UsualType.Float
		internal property IsValueType	as LOGIC GET ! SELF:IsReferenceType
		internal property IsReferenceType as LOGIC
			GET
				switch _usualType
				case __UsualType.Array
				case __UsualType.OBJECT
				case __UsualType.STRING
					return true
				otherwise
					return false
				end switch
			END GET
		end property

		internal property IsNil as Logic
			Get
				return self:usualType == __UsualType.Void .or. ;
				(self:IsReferenceType .and. self:_refData  == null)

			End Get
		end property
		#endregion


		#region implementation IComparable
		public method CompareTo(o as Object) as Long
			return 0
		#endregion
				
		#region Comparison Operators 
		static operator >(leftOperand as __Usual, rightOperand as __Usual) as Logic
			THROW NotImplementedException{}

		static operator >=(leftOperand as __Usual, rightOperand as __Usual) as Logic
			THROW NotImplementedException{}

		static operator <(leftOperand as __Usual, rightOperand as __Usual) as Logic
			THROW NotImplementedException{}

		static operator <=(leftOperand as __Usual, rightOperand as __Usual) as Logic
			THROW NotImplementedException{}

#endregion

#region Operators for Equality
		public method Equals(obj as object) as logic
			return super:Equals(obj)

		public method GetHashCode() as int
			return super:GetHashCode()

		static operator ==(leftOperand as __Usual, rightOperand as __Usual) as Logic
			THROW NotImplementedException{}

		static operator !=(leftOperand as __Usual, rightOperand as __Usual) as Logic
			THROW NotImplementedException{}

#endregion

#region Unary Operators
		static operator -(u as __Usual) as __Usual
			THROW NotImplementedException{}
		static operator +(u as __Usual) as __Usual
			THROW NotImplementedException{}
		static operator --(u as __Usual) as __Usual
			THROW NotImplementedException{}

		static operator ++(u as __Usual) as __Usual
			THROW NotImplementedException{}

#endregion
#region Numeric Operators for Add, Delete etc (also for strings)

		static operator +(leftOperand as __Usual, rightOperand as __Usual) as __Usual
			THROW NotImplementedException{}

		static operator /(leftOperand as __Usual, rightOperand as __Usual) as __Usual
			THROW NotImplementedException{}



		static operator <<(leftOperand as __Usual, rightOperand as Long) as __Usual
			THROW NotImplementedException{}

		static method op_LogicalNot(u as __Usual) as Logic
			THROW NotImplementedException{}

		static operator %(leftOperand as __Usual, rightOperand as __Usual) as __Usual
			THROW NotImplementedException{}

		static operator *(leftOperand as __Usual, rightOperand as __Usual) as __Usual
			THROW NotImplementedException{}

		static operator >>(leftOperand as __Usual, rightOperand as Long) as __Usual
			THROW NotImplementedException{}

		static operator -(leftOperand as __Usual, rightOperand as __Usual) as __Usual
			THROW NotImplementedException{}


		static operator &(leftOperand as __Usual, rightOperand as __Usual) as __Usual
			THROW NotImplementedException{}

		static operator |(leftOperand as __Usual, rightOperand as __Usual) as __Usual
			THROW NotImplementedException{}
		#endregion

#region Implicit From Usual to Other Type
		static operator implicit(u as __Usual) as __Array
			THROW NotImplementedException{}
		static operator implicit(u as __Usual) as Logic
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as Byte
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as System.Collections.ArrayList
			THROW NotImplementedException{}
		static operator implicit(u as __Usual) as System.DateTime
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as real8
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as Short
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as Long
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as __VoFloat
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as Int64
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as System.IntPtr
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as SByte
			THROW NotImplementedException{}
		static operator implicit(u as __Usual) as real4
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as string
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as Word
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as DWord
			THROW NotImplementedException{}

		static operator implicit(u as __Usual) as UInt64
			THROW NotImplementedException{}
#endregion
#region Implicit from Other Type to Usual
		static operator implicit(value as Logic) as __Usual
			return __Usual{value}

		static operator implicit(value as Byte) as __Usual
			return __Usual{(int)value}

		static operator implicit(value as __Array) as __Usual
			return __Usual{value}

		static operator implicit(value as System.DateTime) as __Usual
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
			THROW NotImplementedException{}

		public method ToByte(provider as System.IFormatProvider) as Byte
			THROW NotImplementedException{}

		public method ToChar(provider as System.IFormatProvider) as Char
			THROW NotImplementedException{}

		public method ToDateTime(provider as System.IFormatProvider) as System.DateTime
			THROW NotImplementedException{}

		public method ToDecimal(provider as System.IFormatProvider) as Decimal
			THROW NotImplementedException{}

		public method ToDouble(provider as System.IFormatProvider) as real8
			THROW NotImplementedException{}

		public method ToInt16(provider as System.IFormatProvider) as Short
			THROW NotImplementedException{}

		public method ToInt32(provider as System.IFormatProvider) as Long
			return 0

		public method ToInt64(provider as System.IFormatProvider) as Int64
			return 0

		static method ToObject(u as __Usual) as Object
			return NULL

		public method ToSByte(provider as System.IFormatProvider) as SByte
			return 0

		public method ToSingle(provider as System.IFormatProvider) as real4
		return 0

		public method ToString() as string
			local strResult as STRING
	      
			switch (SELF:usualType)
			case __UsualType.Array
			case __UsualType.CODEBLOCK
			case __UsualType.OBJECT
				strResult := SELF:_refData:ToString()

			case __UsualType.Date
				strResult := self:_valueData:d:ToString()

			case __UsualType.Float
				strResult := self:_valueData:r8:ToString()

			case __UsualType.INT
				strResult := self:_valueData:i:ToString()

			case __UsualType.INT64
				strResult := self:_valueData:i64:ToString()

			case __UsualType.LOGIC
				strResult := IIF(!self:_valueData:l , ".F." , ".T.")

			case __UsualType.PTR
				strResult := self:_valueData:p:ToString()

			case __UsualType.STRING
				strResult := (string) SELF:_refData;

			case __UsualType.Symbol
				strResult := self:_valueData:s:ToString()

			case __UsualType.Void
				strResult := "NIL"
			otherwise
				strResult := ""
			end switch
			return strResult


		public method ToString(provider as System.IFormatProvider) as string
		return ""
		public method ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
		return NULL

		public method ToUInt16(provider as System.IFormatProvider) as Word
		return 0

		public method ToUInt32(provider as System.IFormatProvider) as DWord
		return 0

		public method ToUInt64(provider as System.IFormatProvider) as UInt64
		return 0

		public method GetTypeCode() as System.TypeCode
		THROW NotImplementedException{}
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
		member @@Int		:=1
		member @@LongInt	:=1	// Synonym to Int
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

end namespace