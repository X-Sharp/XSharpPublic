//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Runtime.InteropServices

[StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)];
structure Vulcan.__Usual implements System.IConvertible,System.IComparable
    #region static fields
	public static _NIL as __Usual
	#endregion
    #region private fields
    private flags    	as UsualFlags		// type, byref, width, decimals
    private refData  	as Object			// for GC data
    private valueData	as UsualData		// for non GC data
	#endregion
    #region constructors
	STATIC Constructor()
		_NIL := __Usual{}
	return

    private  constructor(u as __Usual)
		self:flags      := u:flags
        self:valueData	:= u:valueData
	    self:refData 	:= u:refData 
        self:_usualType := u:_usualType
	return

    private  constructor(l as Logic)
		self:flags			:= UsualFlags{UsualDataType.LOGIC}
		self:valueData		:= UsualData{}
		self:refData 		:= null
        self:valueData:l	:= l
	return

    private  constructor(a as __Array)
		self:flags		:= UsualFlags{UsualDataType.ARRAY}
		self:valueData	:= UsualData{}
        self:refData	:= a
	return

    private  constructor(dt as System.DateTime)
		self:flags		:= UsualFlags{ UsualDataType.DATE}
		self:valueData	:= UsualData{}
        self:refData	:= dt
	return

    private  constructor(i as Long)
		self:flags		:= UsualFlags{UsualDataType.INT}
		self:valueData	:= UsualData{}
		self:refData 	 := null
        self:valueData:i := i
	return

    private  constructor(i as Int64)
		self:flags		:= UsualFlags{UsualDataType.INT64}
		self:valueData	:= UsualData{}
		self:refData 	:= null
        self:valueData:i64 := i
	return

    private  constructor(p as System.IntPtr)
		self:flags		:= UsualFlags{UsualDataType.PTR}
		self:valueData	:= UsualData{}
		self:refData 	 := null
        self:valueData:p := p
	return

    public constructor(o as Object)
        local u				as __Usual
        local vartype		as System.Type
        local typeCode		as System.TypeCode
		self:flags			:= UsualFlags{UsualDataType.PTR}
		self:valueData		:= UsualData{}
		self:refData 		    := null
        if (o != null)
            if (o:GetType() == typeof(__Usual))
                // boxed Usual
                u		:= (__Usual)o 
                self:refData	:= u:refData 
                self:valueData	:= u:valueData
                self:_usualType := u:_usualType
            else
                //  decode type from typecode
                vartype := o:GetType()
                typeCode := System.Type.GetTypeCode(vartype)
                switch typeCode
                case  System.TypeCode.DBNull
                    self:_usualType := UsualDataType.NIL
                    self:refData	:= null
                case System.TypeCode.Boolean
                    self:_usualType := UsualDataType.LOGIC
                    self:valueData:l := (Logic)o 
                case System.TypeCode.Char
                    self:_usualType		:= UsualDataType.INT
                    self:valueData:i	:= (Char)o 
                case System.TypeCode.SByte
                    self:_usualType		:= UsualDataType.INT
                    self:valueData:i	:= (SByte)o 
                case System.TypeCode.Byte
                    self:_usualType		:= UsualDataType.INT
                    self:valueData:i	:= (Byte)o 
                case System.TypeCode.Int16 
                    self:_usualType		:= UsualDataType.INT
                    self:valueData:i	:= (Short)o 
                case System.TypeCode.UInt16
                    self:_usualType		:= UsualDataType.INT
                    self:valueData:i	:= (Word)o 
                case System.TypeCode.Int32
                    self:_usualType		:= UsualDataType.INT
                    self:valueData:i	:= (Long)o 
                case System.TypeCode.UInt32
                    if ((DWord)o  <= 0x7fffffff)
                        self:_usualType := UsualDataType.INT
                        self:valueData:i := (Long)(DWord)o  
                    else
						self:_usualType := UsualDataType.FLOAT
						self:valueData:f := (DWord)o 
                    endif
                case System.TypeCode.Int64 
                    self:_usualType		:= UsualDataType.INT64
                    self:valueData:i64	:= (Int64)o 
                case System.TypeCode.UInt64 
                    self:_usualType := UsualDataType.INT64
                    self:valueData:i64 := (Int64)(UInt64)o  
                case System.TypeCode.Single  
                    self:_usualType		:= UsualDataType.FLOAT
                    self:valueData:f	:= (real8)o 
					self:width := 255
					self:decimals := 255
                case System.TypeCode.Double 
                    self:_usualType := UsualDataType.FLOAT
                    self:valueData:f := (real8)o 
					self:width := 255
					self:decimals := 255
                case System.TypeCode.Decimal 
                    self:_usualType := UsualDataType.OBJECT
                    self:refData  := o
                case System.TypeCode.DateTime 
                    self:_usualType := UsualDataType.DATE
                    self:valueData:d := __VoDate{ (System.DateTime) o }
                case System.TypeCode.String 
                    self:_usualType := UsualDataType.STRING
                    self:refData  := (string)o 
                end switch
                if ((typeCode == System.TypeCode.Object) .and. (vartype== typeof(__Array)))
                    self:_usualType := UsualDataType.ARRAY
                    self:refData  := o
                endif
                if ((typeCode == System.TypeCode.Object) .and. (vartype== typeof(__VODate)))
                    self:_usualType := UsualDataType.DATE
                    self:valueData:d :=  (__VoDate) o
                endif

            endif
        endif
	return

    private  constructor(s as string)
        //
		self:flags		:= UsualFlags{UsualDataType.STRING}
		self:valueData	:= UsualData{}
        self:refData 	:= s
	return
	#endregion
	#region implementation IComparable
    public method CompareTo(o as Object) as Long
        return 0
	#endregion

	#region properties
	private  property _usualType as UsualDataType get flags:usualType set flags:usualType := value
	private  property width     as Byte GET flags:width SET flags:width := value
	private  property decimals  as Byte GET flags:decimals SET flags:decimals := value
	public   Property UsualType as UsualDataType GET  _usualType
    internal property IsArray   as Logic GET self:usualType == UsualDataType.ARRAY
    internal property IsByRef   as Logic GET self:flags:IsByRef
    internal property IsDate	as Logic GET self:usualType == UsualDataType.DATE
    internal property IsPtr		as Logic GET self:usualType == UsualDataType.PTR
    internal property IsString	as Logic GET self:usualType == UsualDataType.STRING
    internal property IsLong	as Logic GET self:usualType == UsualDataType.INT64
    internal property IsFloat   as Logic GET self:usualType == UsualDataType.FLOAT
	internal property IsValueType as LOGIC GET ! SELF:IsReferenceType

	internal property IsReferenceType as LOGIC
		GET
			if self:_usualType == UsualDataType.ARRAY
				return true
			elseif self:_usualType == UsualDataType.OBJECT
				return true
			elseif self:_usualType ==  UsualDataType.STRING
				return true
			else
				return false
			endif
		END GET
	end property


    internal property IsNil as Logic
        Get
            return self:usualType == UsualDataType.NIL .or. ;
			(self:IsReferenceType .and. self:refData  == null)

        End Get
    end property



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
        case UsualDataType.ARRAY
            strResult := SELF:refData:ToString()

        case UsualDataType.CODEBLOCK
            strResult := SELF:refData:ToString()

        case UsualDataType.DATE
            strResult := self:valueData:d:ToString()

        case UsualDataType.FLOAT
            strResult := self:valueData:f:ToString()

        case UsualDataType.INT
            strResult := self:valueData:i:ToString()

        case UsualDataType.INT64
            strResult := self:valueData:i64:ToString()

        case UsualDataType.LOGIC
            strResult := IIF(!self:valueData:l , ".F." , ".T.")

        case UsualDataType.OBJECT
			// todo
            strResult := ""

        case UsualDataType.PTR
			// todo
            strResult := ""

        case UsualDataType.STRING
            strResult := (string) SELF:refData;

        case UsualDataType.SYMBOL
            strResult := self:valueData:s:ToString()

        case UsualDataType.NIL
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
	#region operators
    static operator +(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        THROW NotImplementedException{}

    static operator &(leftOperand as __Usual, rightOperand as __Usual) as __Usual
		THROW NotImplementedException{}

    static operator |(leftOperand as __Usual, rightOperand as __Usual) as __Usual
		THROW NotImplementedException{}

    static operator --(u as __Usual) as __Usual
		THROW NotImplementedException{}

    static operator /(leftOperand as __Usual, rightOperand as __Usual) as __Usual
		THROW NotImplementedException{}

	public method Equals(obj as object) as logic
		return super:Equals(obj)

	public method GetHashCode() as int
		return super:GetHashCode()

    static operator ==(leftOperand as __Usual, rightOperand as __Usual) as Logic
		THROW NotImplementedException{}

    static operator >(leftOperand as __Usual, rightOperand as __Usual) as Logic
		THROW NotImplementedException{}

    static operator >=(leftOperand as __Usual, rightOperand as __Usual) as Logic
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

    static operator implicit(l as Logic) as __Usual
	    return __Usual{l}

    static operator implicit(i as Byte) as __Usual
	    return __Usual{(int)i}

    static operator implicit(a as __Array) as __Usual
		return __Usual{a}

    static operator implicit(dt as System.DateTime) as __Usual
    return __Usual{dt}

    static operator implicit(v as real8) as __Usual
    return __Usual{v}

    static operator implicit(i as Short) as __Usual
    return __Usual{(int)i}

    static operator implicit(i as Long) as __Usual
    return __Usual{i}

    static operator implicit(i64 as Int64) as __Usual
    return __Usual{i64}

    static operator implicit(i as System.IntPtr) as __Usual
    return __Usual{i}

    static operator implicit(i as SByte) as __Usual
    return __Usual{(int)i}

    static operator implicit(v as real4) as __Usual
    return __Usual{(real8)v }

    static operator implicit(s as string) as __Usual
    return __Usual{s}

    static operator implicit(i as Word) as __Usual
    return __Usual{(int)i}

    static operator implicit(v as DWord) as __Usual
    return IIF((v > 0x7fffffff),__Usual{(Long)v },__Usual{(Long)v })

    static operator ++(u as __Usual) as __Usual
		THROW NotImplementedException{}

    static operator !=(leftOperand as __Usual, rightOperand as __Usual) as Logic
		THROW NotImplementedException{}

    static operator <<(leftOperand as __Usual, rightOperand as Long) as __Usual
		THROW NotImplementedException{}
    static operator <(leftOperand as __Usual, rightOperand as __Usual) as Logic
		THROW NotImplementedException{}

    static operator <=(leftOperand as __Usual, rightOperand as __Usual) as Logic
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

    static operator -(u as __Usual) as __Usual
		THROW NotImplementedException{}
    static operator +(u as __Usual) as __Usual
		THROW NotImplementedException{}
	#endregion

    static operator implicit(u as __Usual) as __Array
		THROW NotImplementedException{}

end structure			


[StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit, CharSet:=System.Runtime.InteropServices.CharSet.Auto)];
internal structure Vulcan.UsualData
    // Fields
    [FieldOffset(0)];
    export d as __VoDate
    [FieldOffset(0)];
    export f as real8
    [FieldOffset(0)];
    export i as Long
    [FieldOffset(0)];
    export i64 as Int64
    [FieldOffset(0)];
    export l as Logic
    [FieldOffset(0)];
    export p as System.IntPtr
    [FieldOffset(0)];
    export s as __Symbol

end structure

public enum Vulcan.UsualDataType as byte
    member @@ARRAY:=5
    member @@CODEBLOCK:=9
    member @@DATE:=2
    member @@FLOAT:=3
    member @@INT:=1
    member @@INT64:=0x16
    member @@LOGIC:=8
    member @@NIL:=0
    member @@OBJECT:=6
    member @@PSZ:=0x11
    member @@PTR:=0x12
    member @@STRING:=7
    member @@SYMBOL:=10
end enum

[StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)];
internal structure Vulcan.UsualFlags
    [FieldOffset(0)];
	export usualType as UsualDataType
    [FieldOffset(1)];
	export width as byte
    [FieldOffset(2)];
	export decimals as byte
    [FieldOffset(3)];
	export isByRef as logic
	CONSTRUCTOR(type as UsualDataType)
		usualType := type
		width	  := 0
		decimals  := 0
		isByRef   := false
end structure