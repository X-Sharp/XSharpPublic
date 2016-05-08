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
    private flags as UsualFlags				// 4 bytes
    private value as Object					// 4 bytes
    private valueData as UsualData			// 8 bytes
	#endregion
    #region constructors
	STATIC Constructor()
		_NIL := __Usual{}
	return

	internal property _Width		as Byte			 get flags:width		set flags:width := value
	internal property _UsualType	as UsualDataType get flags:type			set flags:type := value
	internal property _Decimals		as Byte			 get flags:decimals		set flags:decimals := value
	internal property _Byref	    as Logic		 get flags:byref != 0	set flags:byref := (byte)iif(value,1,0)
    private  constructor(u as __Usual)
		self:valueData := UsualData{}
		self:value := u
	    self:flags := u:flags
        self:valueData := u:valueData
        self:_UsualType := u:_UsualType
	return

    private  constructor(l as Logic)
		self:valueData := UsualData{}
        self:valueData:l := l
        self:value := null
	    self:flags := UsualFlags{UsualDataType.LOGIC}
	return

    private  constructor(a as __Array)
		self:valueData := UsualData{}
        self:valueData:i64 := 0
        self:value := a
		self:flags := UsualFlags{UsualDataType.ARRAY}
	return

    private  constructor(dt as System.DateTime)
		self:valueData := UsualData{}
        self:valueData:i64 := 0
        self:value := dt
	    self:flags := UsualFlags{UsualDataType.OBJECT}
	return

    private  constructor(i as Long)
		self:valueData := UsualData{}
        self:valueData:i := i
        self:value := null
	    self:flags := UsualFlags{UsualDataType.INT}
	return

    private  constructor(i as Int64)
		self:valueData := UsualData{}
        self:valueData:i64 := i
        self:value := null
	    self:flags := UsualFlags{UsualDataType.INT64}
	return

    private  constructor(p as System.IntPtr)
		self:valueData := UsualData{}
        self:valueData:p := p
        self:value := null
  	    self:flags := UsualFlags{UsualDataType.PTR}
	return

    public constructor(o as Object)
        local u as __Usual
        local @@type as System.Type
        local typeCode as System.TypeCode
		self:valueData := UsualData{}
        self:value := null
        self:flags := UsualFlags{UsualDataType.PTR}
        if (o == null)
            self:valueData:p := System.IntPtr.Zero
        else
            if (o:GetType() == typeof(__Usual))
                //
                u := (__Usual)o 
                self:value		:= u:value
                self:valueData	:= u:valueData
				self:flags		:= UsualFlags{u:_UsualType}
            else
                //
                @@type := o:GetType()
                typeCode := System.Type.GetTypeCode(@@type)
                switch typeCode
                case  System.TypeCode.DBNull
                    self:_UsualType := UsualDataType.NIL
                    self:value := null
                case System.TypeCode.Boolean
                    self:_UsualType := UsualDataType.LOGIC
                    self:valueData:l := (Logic)o 
                case System.TypeCode.Char
                    self:_UsualType := UsualDataType.INT
                    self:valueData:i := (Char)o 
                case System.TypeCode.SByte
                    self:_UsualType := UsualDataType.INT
                    self:valueData:i := (SByte)o 
                case System.TypeCode.Byte
                    self:_UsualType := UsualDataType.INT
                    self:valueData:i := (Byte)o 
                case System.TypeCode.Int16 
                    self:_UsualType := UsualDataType.INT
                    self:valueData:i := (Short)o 
                case System.TypeCode.UInt16
                    self:_UsualType := UsualDataType.INT
                    self:valueData:i := (Word)o 
                case System.TypeCode.Int32
                    self:_UsualType := UsualDataType.INT
                    self:valueData:i := (Long)o 
                case System.TypeCode.UInt32
                    if ((DWord)o  <= 0x7fffffff)
                        self:_UsualType := UsualDataType.INT
                        self:valueData:i := (Long)(DWord)o  
                    else
						self:_UsualType := UsualDataType.FLOAT
						self:valueData:f := (DWord)o 
                    endif
                case System.TypeCode.Int64 
                    self:_UsualType := UsualDataType.INT64
                    self:valueData:i64 := (Int64)o 
                case System.TypeCode.UInt64 
                    self:_UsualType := UsualDataType.INT64
                    self:valueData:i64 := (Int64)(UInt64)o  
                case System.TypeCode.Single  
                    self:_UsualType := UsualDataType.FLOAT
                    self:valueData:f := (real8)o 
                case System.TypeCode.Double 
                    self:_UsualType := UsualDataType.FLOAT
                    self:valueData:f := (real8)o 
                case System.TypeCode.Decimal 
                    self:_UsualType := UsualDataType.OBJECT
                    self:value := o
                case System.TypeCode.DateTime 
                    self:_UsualType := UsualDataType.DATE
                    self:valueData:d := (System.DateTime)o 
                case System.TypeCode.String 
                    self:_UsualType := UsualDataType.STRING
                    self:value := (string)o 
                end switch
                if ((typeCode == System.TypeCode.Object) .and. (@@type == typeof(__Array)))
                    self:_UsualType := UsualDataType.ARRAY
                    self:value := o
                endif
                if ((typeCode == System.TypeCode.Object) .and. (@@type == typeof(__VODate)))
                    self:_UsualType := UsualDataType.DATE
                    self:value := o
                endif

            endif
        endif
	return

    private  constructor(s as string)
        //
		self:valueData := UsualData{}
        self:valueData:i64 := 0
        self:value := s
        self:flags := UsualFlags{UsualDataType.String}
	return
	#endregion
	#region implementation IComparable
    public method CompareTo(o as Object) as Long
        local typeLHS as UsualDataType
        local typeRHS as UsualDataType
        local u as __Usual
        local iPtrValue as Int64
        local uiDateValue as DWord
        typeLHS := self:_UsualType
        u		:= (__Usual)o 
        typeRHS := u:_UsualType
        if (typeLHS == typeRHS)
			Switch (typeLHS)
            CASE UsualDataType.DATE
                return (int) (self:valueData:d:Ticks -  u:valueData:d:Ticks)
            CASE UsualDataType.FLOAT
                return (int)(self:valueData:f - u:valueData:f)
            CASE UsualDataType.INT
                return self:valueData:i - u:valueData:i
            CASE UsualDataType.INT64
                return (int) (self:valueData:i64 - u:valueData:i64)
            CASE UsualDataType.LOGIC
                return IIF((self:valueData:l != u:valueData:l),IIF(! self:valueData:l,-1,1),0)
            CASE UsualDataType.PTR
                return (int) (self:valueData:p:ToInt64() - u:valueData:p:ToInt64())
            CASE UsualDataType.STRING
                return String.Compare((string)self:value , (string)u:value , System.StringComparison.CurrentCultureIgnoreCase)
            END SWITCH
            return 0
        endif
        if (typeLHS == UsualDataType.NIL)
            return -1
        endif
        if (typeRHS == UsualDataType.NIL)
            return 1
        endif
        if (typeLHS != UsualDataType.DATE)
			uiDateValue := self:DateToUInt(self:valueData:d)    
            SWITCH typeLHS
            CASE UsualDataType.FLOAT 
                SWITCH typeRHS
                CASE UsualDataType.INT 
                    return IIF((self:valueData:f != u:valueData:i),IIF((self:valueData:f >= u:valueData:i),1,-1),0)
                CASE UsualDataType.DATE 
                    return IIF((self:valueData:f != uiDateValue),IIF((self:valueData:f >= uiDateValue),1,-1),0)
                CASE UsualDataType.INT64 
                    return IIF((self:valueData:f != u:valueData:i64),IIF((self:valueData:f >= u:valueData:i64),1,-1),0)
                end SWITCH
            CASE UsualDataType.INT 
                SWITCH typeRHS
                CASE UsualDataType.DATE 
                    return IIF((self:valueData:i != uiDateValue),IIF((self:valueData:i >= uiDateValue),1,-1),0)
                CASE UsualDataType.FLOAT 
                    return IIF((self:valueData:i != u:valueData:f),IIF((self:valueData:i >= u:valueData:f),1,-1),0)
                CASE UsualDataType.INT64 
                    return IIF((self:valueData:i != u:valueData:i64),IIF((self:valueData:i >= u:valueData:i64),1,-1),0)
                end SWITCH
            case UsualDataType.INT64 
                SWITCH typeRHS
                case UsualDataType.INT 
                    return IIF((self:valueData:i64 != u:valueData:i),IIF((self:valueData:i64 >= u:valueData:i),1,-1),0)
                case UsualDataType.DATE 
                    return IIF((self:valueData:i64 != uiDateValue),IIF((self:valueData:i64 >= uiDateValue),1,-1),0)
                case UsualDataType.FLOAT  
                    return IIF((self:valueData:i64 != u:valueData:f),IIF((self:valueData:i64 >= u:valueData:f),1,-1),0)
                end SWITCH
            end SWITCH
			return (Int) typeLHS - (int) typeRHS
        endif
		uiDateValue := self:DateToUInt(self:valueData:d)    
        SWITCH typeRHS
        case UsualDataType.INT 
			RETURN uiDateValue - (DWORD) (u:valueData:i)
        case UsualDataType.FLOAT 
			RETURN uiDateValue - (DWORD) u:valueData:f
        case UsualDataType.INT64 
			RETURN uiDateValue - (DWORD) u:valueData:i64
        end switch
		return (Int) typeLHS - (int) typeRHS
	#endregion
	#region helper methods
    private method DateToUInt(d as System.DateTime) as DWord
        local span as System.TimeSpan
        span := (System.TimeSpan)(d - System.DateTime{0x76d, 1, 1}) 
    return (DWord)(span:Days + 0x24db1a) 

    private method UsualEquals(rightOperand as __Usual, op as string) as Logic
        local usualType as UsualDataType
        usualType := rightOperand:usualType
        if (self:IsNil)
            return rightOperand:IsNil
        endif
        if (rightOperand:IsNil)
            return self:IsNil
        endif
        if ((self:usualType == UsualDataType.OBJECT) .and. (usualType == UsualDataType.OBJECT))
            return (self:value == rightOperand:value)
        endif
        if ((self:usualType == UsualDataType.INT) .and. (usualType == UsualDataType.INT))
            return (self:valueData:i == rightOperand:valueData:i)
        endif
        if ((self:usualType == UsualDataType.INT) .and. (usualType == UsualDataType.INT64))
            return (self:valueData:i == rightOperand:valueData:i64)
        endif
        if ((self:usualType == UsualDataType.INT) .and. (usualType == UsualDataType.FLOAT))
            return (self:valueData:i == rightOperand:valueData:f)
        endif
        if ((self:usualType == UsualDataType.INT64) .and. (usualType == UsualDataType.INT))
            return (self:valueData:i64 == rightOperand:valueData:i)
        endif
        if ((self:usualType == UsualDataType.INT64) .and. (usualType == UsualDataType.INT64))
            return (self:valueData:i64 == rightOperand:valueData:i64)
        endif
        if ((self:usualType == UsualDataType.INT64) .and. (usualType == UsualDataType.FLOAT))
            return (self:valueData:i64 == rightOperand:valueData:f)
        endif
        if ((self:usualType == UsualDataType.FLOAT) .and. (usualType == UsualDataType.INT))
            return (self:valueData:f == rightOperand:valueData:i)
        endif
        if ((self:usualType == UsualDataType.FLOAT) .and. (usualType == UsualDataType.INT64))
            return (self:valueData:f == rightOperand:valueData:i64)
        endif
        if ((self:usualType == UsualDataType.FLOAT) .and. (usualType == UsualDataType.FLOAT))
            return (self:valueData:f == rightOperand:valueData:f)
        endif
        if ((self:usualType == UsualDataType.LOGIC) .and. (usualType == UsualDataType.LOGIC))
            return (self:valueData:l == rightOperand:valueData:l)
        endif
        if ((self:usualType == UsualDataType.LOGIC) .and. (usualType == UsualDataType.INT))
            return (self:valueData:l == (rightOperand:valueData:i != 0))
        endif
        if ((self:usualType == UsualDataType.INT) .and. (usualType == UsualDataType.LOGIC))
            return ((self:valueData:i != 0) == rightOperand:valueData:l)
        endif
        if ((self:usualType == UsualDataType.DATE) .and. (usualType == UsualDataType.DATE))
            return (self:valueData:d == rightOperand:valueData:d)
        endif
        if ((self:usualType == UsualDataType.STRING) .and. (usualType == UsualDataType.STRING))
            return ((string)self:value  == (string)rightOperand:value )
        endif
        if ((self:usualType == UsualDataType.ARRAY) .and. (usualType == UsualDataType.ARRAY))
            return (self:value == rightOperand:value)
        endif
        if ((self:usualType == UsualDataType.CODEBLOCK) .and. (usualType == UsualDataType.CODEBLOCK))
            return (self:value == rightOperand:value)
        endif
        if ((self:usualType != UsualDataType.PTR) .or. (usualType != UsualDataType.PTR))
            throw System.InvalidOperationException{String.Format("Incompatible type {0} {1} {2} ", self, op, rightOperand)}
        endif
    return (self:valueData:p == rightOperand:valueData:p)
	#endregion
	#region properties
	public Property UsualType as UsualDataType
		get
			return SELF:_UsualType
		end get
	end property
    internal property IsArray as Logic
        Get
            return (self:usualType == UsualDataType.ARRAY)
        End Get
    end property

    internal property IsDate as Logic
        Get
            return (self:usualType == UsualDataType.DATE)
        End Get
    end property

    internal property IsNil as Logic
        Get
            return ((self:usualType == UsualDataType.NIL) .or. (((self:usualType == UsualDataType.ARRAY) .or. (self:usualType == UsualDataType.OBJECT)) .and. (self:value == null)))
        End Get
    end property

    internal property IsPtr as Logic
        Get
            return (self:usualType == UsualDataType.PTR)
        End Get
    end property

    internal property IsString as Logic
        Get
            return (self:usualType == UsualDataType.STRING)
        End Get
    end property

	
    internal property IsLong as Logic
        Get
            return (self:usualType == UsualDataType.INT64)
        End Get
    end property

	
    internal property IsFloat as Logic
        Get
            return (self:usualType == UsualDataType.FLOAT)
        End Get
    end property

	#endregion
	#region implementation IConvertable
    public method ToBoolean(provider as System.IFormatProvider) as Logic
    return self:valueData:l

    public method ToByte(provider as System.IFormatProvider) as Byte
    return (Byte)self:valueData:i 

    public method ToChar(provider as System.IFormatProvider) as Char
        local o as Object
        o := __Usual.ToObject(self)
        if (! typeof(System.IConvertible):IsInstanceOfType(o))
            throw System.InvalidCastException{}
        endif
    return ((System.IConvertible)o):ToChar(provider)

    public method ToDateTime(provider as System.IFormatProvider) as System.DateTime
    return self:valueData:d

    public method ToDecimal(provider as System.IFormatProvider) as Decimal
    return (Decimal)self:valueData:f 

    public method ToDouble(provider as System.IFormatProvider) as real8
    return self:valueData:f

    public method ToInt16(provider as System.IFormatProvider) as Short
    return (Short)self:valueData:i 

    public method ToInt32(provider as System.IFormatProvider) as Long
    return self:valueData:i

    public method ToInt64(provider as System.IFormatProvider) as Int64
    return self:valueData:i64

    static method ToObject(u as __Usual) as Object
        local obj2 as Object
        obj2 := null
        do case
        case ( u:usualType == UsualDataType.NIL ) 
            return null
        case ( u:usualType == UsualDataType.INT ) 
            return u:valueData:i
        case ( u:usualType == UsualDataType.DATE ) 
            return u:valueData:d
        case ( u:usualType == UsualDataType.FLOAT ) 
            return u:valueData:f
        case ( u:usualType == (UsualDataType)4  ) 
            return obj2
        case ( u:usualType == UsualDataType.ARRAY ) 
            return u:value
        case ( u:usualType == UsualDataType.OBJECT ) 
            return u:value
        case ( u:usualType == UsualDataType.STRING ) 
            return u:value
        case ( u:usualType == UsualDataType.LOGIC ) 
            return u:valueData:l
        case ( u:usualType == UsualDataType.PTR ) 
            if (u:valueData:p == System.IntPtr.Zero)
                return null
            endif
            return u:valueData:p
        case ( u:usualType == UsualDataType.INT64 ) 
            return u:valueData:i64
        endcase
    return obj2

    public method ToSByte(provider as System.IFormatProvider) as SByte
    return (SByte)self:valueData:i 

    public method ToSingle(provider as System.IFormatProvider) as real4
    return (real4)self:valueData:f 

    public method ToString() as string
        local str as string
        str := ""
        do case
        case ( self:usualType == UsualDataType.NIL ) 
            return "NIL"
        case ( self:usualType == UsualDataType.INT ) 
            return self:valueData:i:ToString()
        case ( self:usualType == UsualDataType.DATE ) 
            return self:value:ToString()
        case ( self:usualType == UsualDataType.FLOAT ) 
            return self:valueData:f:ToString()
        case ( self:usualType == (UsualDataType)4  ) .or. ;
            ( self:usualType == UsualDataType.SYMBOL ) .or. ;
            ( self:usualType == (UsualDataType.SYMBOL | UsualDataType.INT) ) .or. ;
            ( self:usualType == (UsualDataType)12  ) .or. ;
            ( self:usualType == (UsualDataType)13  ) .or. ;
            ( self:usualType == (UsualDataType)14  ) .or. ;
            ( self:usualType == (UsualDataType.SYMBOL | UsualDataType.ARRAY) ) .or. ;
            ( self:usualType == (UsualDataType)0x10  ) .or. ;
            ( self:usualType == UsualDataType.PSZ ) 
            return str
        case ( self:usualType == UsualDataType.ARRAY ) 
            return self:value:ToString()
        case ( self:usualType == UsualDataType.OBJECT ) 
            return self:value:ToString()
        case ( self:usualType == UsualDataType.STRING ) 
            return (string)self:value 
        case ( self:usualType == UsualDataType.LOGIC ) 
            return IIF(! self:valueData:l,".F.",".T.")
        case ( self:usualType == UsualDataType.CODEBLOCK ) 
            return self:value:ToString()
        case ( self:usualType == UsualDataType.PTR ) 
            return "IntPtr"
        case ( self:usualType == UsualDataType.INT64 ) 
            return self:valueData:i64:ToString()
        endcase
    return str

    public method ToString(provider as System.IFormatProvider) as string
    return self:ToString()

    public method ToType(conversionType as System.Type, provider as System.IFormatProvider) as Object
        local o as Object
        if (conversionType:IsPointer)
            if (self:usualType == UsualDataType.PTR)
                return self:valueData:p
            endif
            if (self:usualType == UsualDataType.INT)
                return (System.IntPtr)self:valueData:i 
            endif
            if (self:usualType != UsualDataType.INT64)
                throw System.InvalidCastException{}
            endif
            return (System.IntPtr)self:valueData:i64 
        endif
        o := __Usual.ToObject(self)
        if (conversionType:IsAssignableFrom(o:GetType()))
            return o
        endif
        if (! typeof(System.IConvertible):IsInstanceOfType(o))
            throw System.InvalidCastException{}
        endif
    return ((System.IConvertible)o):ToType(conversionType, provider)

    public method ToUInt16(provider as System.IFormatProvider) as Word
    return (Word)self:valueData:i 

    public method ToUInt32(provider as System.IFormatProvider) as DWord
    return (DWord)self:valueData:i 

    public method ToUInt64(provider as System.IFormatProvider) as UInt64
    return (UInt64)self:valueData:i64 

	public method GetTypeCode() as System.TypeCode
        local boolean as System.TypeCode
        boolean := System.TypeCode.Object
        if (self:usualType == UsualDataType.ARRAY)
            boolean := System.TypeCode.Object
        endif
        if (self:usualType == UsualDataType.CODEBLOCK)
            boolean := System.TypeCode.Object
        endif
        if (self:usualType == UsualDataType.DATE)
            boolean := System.TypeCode.Object
        endif
        if (self:usualType == UsualDataType.FLOAT)
            boolean := System.TypeCode.Object
        endif
        if (self:usualType == UsualDataType.INT)
            boolean := System.TypeCode.Int32
        endif
        if (self:usualType == UsualDataType.INT64)
            boolean := System.TypeCode.Int64
        endif
        if (self:usualType == UsualDataType.LOGIC)
            boolean := System.TypeCode.Boolean
        endif
        if (self:usualType == UsualDataType.OBJECT)
            boolean := System.TypeCode.Object
        endif
        if (self:usualType == UsualDataType.PTR)
            boolean := System.TypeCode.Object
        endif
        if (self:usualType == UsualDataType.STRING)
            boolean := System.TypeCode.String
        endif
        if (self:usualType == UsualDataType.SYMBOL)
            boolean := System.TypeCode.Object
        endif
        if (self:usualType == UsualDataType.NIL)
            boolean := System.TypeCode.Object
        endif
    return boolean
	#endregion
	#region operators
    static operator +(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        do case
        case ( usualType == UsualDataType.INT ) 
           do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i + rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i + rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i + rightOperand:valueData:i64)
            endcase
        case ( usualType == UsualDataType.FLOAT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f + rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f + rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f + rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for addition", leftOperand, rightOperand)}
        case ( usualType == UsualDataType.STRING ) 
            if (typeRHS != UsualDataType.STRING)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for addition", leftOperand, rightOperand)}
            endif
            return String.Concat((string)leftOperand:value , (string)rightOperand:value )
        case ( usualType == UsualDataType.INT64 ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i64 + rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i64 + rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i64 + rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for addition", leftOperand, rightOperand)}
        otherwise
            if (usualType != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for addition", leftOperand, rightOperand)}
            endif
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return leftOperand:valueData:d:Add(System.TimeSpan.FromDays((real8)rightOperand:valueData:i ))
            case ( typeRHS == UsualDataType.FLOAT ) 
                return leftOperand:valueData:d:Add(System.TimeSpan.FromDays(rightOperand:valueData:f))
            case ( typeRHS == UsualDataType.INT64 ) 
                return leftOperand:valueData:d:Add(System.TimeSpan.FromDays((real8)rightOperand:valueData:i64 ))
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for addition", leftOperand, rightOperand)}
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for addition", leftOperand, rightOperand)}

    static operator &(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        local type3 as UsualDataType
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        if (usualType == UsualDataType.INT)
            type3 := typeRHS
            if (type3 != UsualDataType.INT)
                if (type3 != UsualDataType.INT64)
                    throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for bitwise and", leftOperand, rightOperand)}
                endif
            else
                return (leftOperand:valueData:i & rightOperand:valueData:i)
            endif
            return (leftOperand:valueData:i & rightOperand:valueData:i64)
        endif
        if (usualType != UsualDataType.INT64)
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for bitwise and", leftOperand, rightOperand)}
        endif
        type3 := typeRHS
        if (type3 != UsualDataType.INT)
            if (type3 != UsualDataType.INT64)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for bitwise and", leftOperand, rightOperand)}
            endif
        else
            return (leftOperand:valueData:i64 & rightOperand:valueData:i)
        endif
    return (leftOperand:valueData:i64 & rightOperand:valueData:i64)

    static operator |(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        local type3 as UsualDataType
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        if (usualType == UsualDataType.INT)
            type3 := typeRHS
            if (type3 != UsualDataType.INT)
                if (type3 != UsualDataType.INT64)
                    throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for bitwise or", leftOperand, rightOperand)}
                endif
            else
                return (leftOperand:valueData:i | rightOperand:valueData:i)
            endif
            return (leftOperand:valueData:i | (Long)rightOperand:valueData:i64 )
        endif
        if (usualType != UsualDataType.INT64)
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for bitwise or", leftOperand, rightOperand)}
        endif
        type3 := typeRHS
        if (type3 != UsualDataType.INT)
            if (type3 != UsualDataType.INT64)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for bitwise or", leftOperand, rightOperand)}
            endif
        else
            return ((Long)leftOperand:valueData:i64  | rightOperand:valueData:i)
        endif
    return (leftOperand:valueData:i64 | rightOperand:valueData:i64)

    static operator --(u as __Usual) as __Usual
        do case
        case ( u:usualType == UsualDataType.INT ) 
            return (u:valueData:i - 1)
        case ( u:usualType == UsualDataType.DATE ) 
            return u:valueData:d:Subtract(System.TimeSpan.FromDays(1))
        case ( u:usualType == UsualDataType.FLOAT ) 
            return (u:valueData:f - 1)
        case ( u:usualType == UsualDataType.INT64 ) 
            return (u:valueData:i64 - 1)
        endcase
        throw System.InvalidOperationException{String.Format("Error on decrementing {0}", u)}

    static operator /(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        local num as Int64
        local num2 as Int64
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        local type3 as UsualDataType
        local num3 as Long
        local num4 as Long
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        type3 := usualType
        if (type3 != UsualDataType.INT)
            if (type3 == UsualDataType.INT64)
                do case
                case ( typeRHS == UsualDataType.INT ) 
                    num := System.Math.DivRem(leftOperand:valueData:i64, (Int64)rightOperand:valueData:i , out num2)
                    if (num2 != 0)
                        return (leftOperand:valueData:i64 / (Int64)rightOperand:valueData:i )
                    endif
                    return num
                case ( typeRHS == UsualDataType.FLOAT ) 
                    return ((real8)leftOperand:valueData:i64  / rightOperand:valueData:f)
                case ( typeRHS == UsualDataType.INT64 ) 
                    num := System.Math.DivRem(leftOperand:valueData:i64, rightOperand:valueData:i64, out num2)
                    if (num2 == 0)
                        return num
                    endif
                    return (leftOperand:valueData:i64 / rightOperand:valueData:i64)
                endcase
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}
            endif
            if (usualType != UsualDataType.FLOAT)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}
            endif
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f / (real8)rightOperand:valueData:i )
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f / rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f / (real8)rightOperand:valueData:i64 )
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}
        endif
        do case
        case ( typeRHS == UsualDataType.INT ) 
            num4 := System.Math.DivRem(leftOperand:valueData:i, rightOperand:valueData:i, out num3)
            if (num3 != 0)
                return (leftOperand:valueData:i / rightOperand:valueData:i)
            endif
            return num4
        case ( typeRHS == UsualDataType.FLOAT ) 
            return ((real8)leftOperand:valueData:i  / rightOperand:valueData:f)
        case ( typeRHS == UsualDataType.INT64 ) 
            num := System.Math.DivRem((Int64)leftOperand:valueData:i , rightOperand:valueData:i64, out num2)
            if (num2 == 0)
                return num
            endif
            return ((Int64)leftOperand:valueData:i  / rightOperand:valueData:i64)
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}

	public method Equals(obj as object) as logic
	return super:Equals(obj)

	public method GetHashCode() as int
	return super:GetHashCode()

    static operator ==(leftOperand as __Usual, rightOperand as __Usual) as Logic
    return leftOperand:UsualEquals(rightOperand, "==")

    static operator >(leftOperand as __Usual, rightOperand as __Usual) as Logic
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        do case
        case ( usualType == UsualDataType.INT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i > rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i > rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i > rightOperand:valueData:i64)
            endcase

        case ( usualType == UsualDataType.FLOAT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f > rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f > rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f > rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for > comparison", leftOperand, rightOperand)}
        case ( usualType == UsualDataType.STRING ) 
            if (typeRHS != UsualDataType.STRING)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for > comparison", leftOperand, rightOperand)}
            endif
            return (String.CompareOrdinal((string)leftOperand:value , (string)rightOperand:value ) > 0)
        case ( usualType == UsualDataType.INT64 ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i64 > rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i64 > rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i64 > rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for > comparison", leftOperand, rightOperand)}
        otherwise
            if (usualType != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for > comparison", leftOperand, rightOperand)}
            endif
            if (typeRHS != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for > comparison", leftOperand, rightOperand)}
            endif
            return (leftOperand:valueData:d > rightOperand:valueData:d)
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for > comparison", leftOperand, rightOperand)}

    static operator >=(leftOperand as __Usual, rightOperand as __Usual) as Logic
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        do case
        case ( usualType == UsualDataType.INT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i >= rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i >= rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i >= rightOperand:valueData:i64)
            endcase

        case ( usualType == UsualDataType.FLOAT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f >= rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f >= rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f >= rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for >= comparison", leftOperand, rightOperand)}
        case ( usualType == UsualDataType.STRING ) 
            if (typeRHS != UsualDataType.STRING)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for >= comparison", leftOperand, rightOperand)}
            endif
            return (String.CompareOrdinal((string)leftOperand:value , (string)rightOperand:value ) >= 0)
        case ( usualType == UsualDataType.INT64 ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i64 >= rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i64 >= rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i64 >= rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for >= comparison", leftOperand, rightOperand)}
        otherwise
            if (usualType != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for >= comparison", leftOperand, rightOperand)}
            endif
            if (typeRHS != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for >= comparison", leftOperand, rightOperand)}
            endif
            return (leftOperand:valueData:d >= rightOperand:valueData:d)
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for >= comparison", leftOperand, rightOperand)}

    static operator implicit(u as __Usual) as Logic
        if (u:usualType == UsualDataType.LOGIC)
            return u:valueData:l
        endif
        if (u:usualType == UsualDataType.INT)
            return (u:valueData:i != 0)
        endif
        if (u:usualType == UsualDataType.INT64)
            return (u:valueData:i64 != 0)
        endif
        if (u:usualType != UsualDataType.NIL)
            throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to logic", u)}
        endif
    return false

    static operator implicit(u as __Usual) as Byte
        try
            if (u:usualType == UsualDataType.FLOAT)
                return System.Convert.ToByte(u:valueData:f)
            endif
            if (u:usualType == UsualDataType.INT)
                return (Byte)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return (Byte)u:valueData:i64 
            endif
            if (u:usualType == UsualDataType.LOGIC)
                return IIF(! u:valueData:l,(Byte)0 ,(Byte)1 )
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to byte", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to byte", u), exception}
        end try
    return 0

    static operator implicit(u as __Usual) as System.Collections.ArrayList
        if (u:IsNil)
            return null
        endif
        if (IIF(u:IsArray,0,1) != 0)
            throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to ArrayList", u)}
        endif
    return (System.Collections.ArrayList)u:value 

    static operator implicit(u as __Usual) as System.DateTime
        if (u:usualType == UsualDataType.NIL)
            return System.DateTime{0}
        endif
        if (IIF(u:IsDate,0,1) != 0)
            throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to DateTime", u)}
        endif
    return u:valueData:d

    static operator implicit(u as __Usual) as real8
        try
            if (u:usualType == UsualDataType.FLOAT)
                return u:valueData:f
            endif
            if (u:usualType == UsualDataType.INT)
                return (real8)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return (real8)u:valueData:i64 
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to double", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to double", u), exception}
        end try
    return 0

    static operator implicit(u as __Usual) as Short
        try
            if (u:usualType == UsualDataType.FLOAT)
                return System.Convert.ToInt16(u:valueData:f)
            endif
            if (u:usualType == UsualDataType.INT)
                return (Short)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return (Short)u:valueData:i64 
            endif
            if (u:usualType == UsualDataType.LOGIC)
                return IIF(! u:valueData:l,(Short)0 ,(Short)1 )
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to short", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to short", u), exception}
        end try
	return 0

    static operator implicit(u as __Usual) as Long
        try
            if (u:usualType == UsualDataType.FLOAT)
                return System.Convert.ToInt32(u:valueData:f)
            endif
            if (u:usualType == UsualDataType.INT)
                return u:valueData:i
            endif
            if (u:usualType == UsualDataType.INT64)
                return (Long)u:valueData:i64 
            endif
            if (u:usualType == UsualDataType.LOGIC)
                return IIF(! u:valueData:l,0,1)
            endif
            if (u:usualType == UsualDataType.PTR)
                return u:valueData:p:ToInt32()
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to Int32", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to Int32", u), exception}
        end try
        return 0

    static operator implicit(u as __Usual) as Int64
        try
            if (u:usualType == UsualDataType.FLOAT)
                return System.Convert.ToInt64(u:valueData:f)
            endif
            if (u:usualType == UsualDataType.INT)
                return (Int64)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return u:valueData:i64
            endif
            if (u:usualType == UsualDataType.LOGIC)
                return IIF(! u:valueData:l,0,1)
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to long", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to long", u), exception}
        end try
	return 0

    static operator implicit(u as __Usual) as System.IntPtr
        if (u:usualType == UsualDataType.NIL)
            return System.IntPtr.Zero
        endif
        if (IIF(u:IsPtr,0,1) != 0)
            throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to PTR", u)}
        endif
    return u:valueData:p

    static operator implicit(u as __Usual) as SByte
        try
            if (u:usualType == UsualDataType.FLOAT)
                return (SByte)u:valueData:f 
            endif
            if (u:usualType == UsualDataType.INT)
                return (SByte)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return (SByte)u:valueData:i64 
            endif
            if (u:usualType == UsualDataType.LOGIC)
                return IIF(! u:valueData:l,(SByte)0 ,(SByte)1 )
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to sbyte", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to sbyte", u), exception}
        end try
    return 0

    static operator implicit(u as __Usual) as real4
        try
            if (u:usualType == UsualDataType.FLOAT)
                return (real4)u:valueData:f 
            endif
            if (u:usualType == UsualDataType.INT)
                return (real4)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return (real4)u:valueData:i64 
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to Single", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to Single", u), exception}
        end try
    return 0

    static operator implicit(u as __Usual) as string
        if (u:usualType == UsualDataType.NIL)
            return ""
        endif
        if (IIF(u:IsString,0,1) != 0)
            throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to String", u)}
        endif
    return u:ToString()

    static operator implicit(u as __Usual) as Word
        local i as Word
        i := 0
        try
            if (u:usualType == UsualDataType.FLOAT)
                i := System.Convert.ToUInt16(u:valueData:f)
            else
                if (u:usualType == UsualDataType.INT)
                    i := (Word)u:valueData:i 
                else
                    if (u:usualType == UsualDataType.INT64)
                        i := (Word)u:valueData:i64 
                    else
                        if (u:usualType == UsualDataType.LOGIC)
                            i := IIF(! u:valueData:l,(Word)0 ,(Word)1 )
                        endif
                    endif
                endif
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to ushort", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to ushort", u), exception}
        end try
    return i

    static operator implicit(u as __Usual) as DWord
        try
            if (u:usualType == UsualDataType.FLOAT)
                return (DWord)u:valueData:f 
            endif
            if (u:usualType == UsualDataType.INT)
                return (DWord)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return (DWord)u:valueData:i64 
            endif
            if (u:usualType == UsualDataType.LOGIC)
                return (dword)IIF(! u:valueData:l,0,1)
            endif
            if (u:usualType == UsualDataType.PTR)
                return (DWord)u:valueData:p:ToInt32() 
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to uint", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to uint", u), exception}
        end try
    return 0

    static operator implicit(u as __Usual) as UInt64
        try
            if (u:usualType == UsualDataType.FLOAT)
                return (UInt64)u:valueData:f 
            endif
            if (u:usualType == UsualDataType.INT)
                return (UInt64)u:valueData:i 
            endif
            if (u:usualType == UsualDataType.INT64)
                return (UInt64)u:valueData:i64 
            endif
            if (u:usualType == UsualDataType.LOGIC)
                return IIF(! u:valueData:l,(UInt64)0 ,(UInt64)1 )
            endif
            if (u:usualType != UsualDataType.NIL)
                throw System.InvalidOperationException{String.Format("Can"+chr(39)+"t convert {0} to ulong", u)}
            endif
        catch exception as System.OverflowException
            throw System.InvalidOperationException{String.Format("Overflow exception converting {0} to ulong", u), exception}
        end try
    return 0

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
        do case
        case ( u:usualType == UsualDataType.INT ) 
            return (u:valueData:i + 1)
        case ( u:usualType == UsualDataType.DATE ) 
            return u:valueData:d:Add(System.TimeSpan.FromDays(1))
        case ( u:usualType == UsualDataType.FLOAT ) 
            return (u:valueData:f + 1)
        case ( u:usualType == UsualDataType.INT64 ) 
            return (u:valueData:i64 + 1)
        endcase
        throw System.InvalidOperationException{String.Format("Error on decrementing {0}", u)}

    static operator !=(leftOperand as __Usual, rightOperand as __Usual) as Logic
        return ! leftOperand:UsualEquals(rightOperand, "!=")

    static operator <<(leftOperand as __Usual, rightOperand as Long) as __Usual
        local usualType as UsualDataType
        usualType := leftOperand:usualType
        if (usualType == UsualDataType.INT)
            return (leftOperand:valueData:i << rightOperand)
        endif
        if (usualType != UsualDataType.INT64)
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for << operator", leftOperand, rightOperand)}
        endif
    return (leftOperand:valueData:i64 << rightOperand)

    static operator <(leftOperand as __Usual, rightOperand as __Usual) as Logic
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        do case
        case ( usualType == UsualDataType.INT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i < rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i < rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i < rightOperand:valueData:i64)
            endcase

        case ( usualType == UsualDataType.FLOAT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f < rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f < rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f < rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for < comparison", leftOperand, rightOperand)}
        case ( usualType == UsualDataType.STRING ) 
            if (typeRHS != UsualDataType.STRING)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for < comparison", leftOperand, rightOperand)}
            endif
            return (String.CompareOrdinal((string)leftOperand:value , (string)rightOperand:value ) < 0)
        case ( usualType == UsualDataType.INT64 ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i64 < rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i64 < rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i64 < rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for < comparison", leftOperand, rightOperand)}
        otherwise
            if (usualType != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for < comparison", leftOperand, rightOperand)}
            endif
            if (typeRHS != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for < comparison", leftOperand, rightOperand)}
            endif
            return (leftOperand:valueData:d < rightOperand:valueData:d)
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for < comparison", leftOperand, rightOperand)}

    static operator <=(leftOperand as __Usual, rightOperand as __Usual) as Logic
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        do case
        case ( usualType == UsualDataType.INT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i <= rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i <= rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i <= rightOperand:valueData:i64)
            endcase
        case ( usualType == UsualDataType.FLOAT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f <= rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f <= rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f <= rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for <= comparison", leftOperand, rightOperand)}
        case ( usualType == UsualDataType.STRING ) 
            if (typeRHS != UsualDataType.STRING)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for <= comparison", leftOperand, rightOperand)}
	        endif
            return (String.CompareOrdinal((string)leftOperand:value , (string)rightOperand:value ) <= 0)
        case ( usualType == UsualDataType.INT64 ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i64 <= rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i64 <= rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i64 <= rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for <= comparison", leftOperand, rightOperand)}
        otherwise
            if (usualType != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for <= comparison", leftOperand, rightOperand)}
            endif
            if (typeRHS != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for <= comparison", leftOperand, rightOperand)}
            endif
            return (leftOperand:valueData:d <= rightOperand:valueData:d)
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for <= comparison", leftOperand, rightOperand)}

    static method op_LogicalNot(u as __Usual) as Logic
        local u2 as __Usual
        if (u:usualType != UsualDataType.LOGIC)
            //
            throw System.InvalidOperationException{String.Format("Error negating {0}", u)}
        endif
        u2 := IIF(u:valueData:l,0,1)
    return (Logic)u2 

    static operator %(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        local type3 as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        type3 := usualType
        if (type3 != UsualDataType.INT)
            if (type3 == UsualDataType.INT64)
                do case
                case ( typeRHS == UsualDataType.INT ) 
                    return (leftOperand:valueData:i64 % (Int64)rightOperand:valueData:i )
                case ( typeRHS == UsualDataType.FLOAT ) 
                    return ((real8)leftOperand:valueData:i64  % rightOperand:valueData:f)
                case ( typeRHS == UsualDataType.INT64 ) 
                    return (leftOperand:valueData:i64 % rightOperand:valueData:i64)
                endcase
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for % operator", leftOperand, rightOperand)}
            endif
            if (usualType != UsualDataType.FLOAT)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for % operator", leftOperand, rightOperand)}
            endif
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f % (real8)rightOperand:valueData:i )
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f % rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f % (real8)rightOperand:valueData:i64 )
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for % operator", leftOperand, rightOperand)}
        endif
        do case
        case ( typeRHS == UsualDataType.INT ) 
            return (leftOperand:valueData:i % rightOperand:valueData:i)
        case ( typeRHS == UsualDataType.FLOAT ) 
            return ((real8)leftOperand:valueData:i  % rightOperand:valueData:f)
        case ( typeRHS == UsualDataType.INT64 ) 
            return ((Int64)leftOperand:valueData:i  % rightOperand:valueData:i64)
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for % operator", leftOperand, rightOperand)}

    static operator *(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        local type3 as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        type3 := usualType
        if (type3 != UsualDataType.INT)
            if (type3 == UsualDataType.INT64)
                do case
                case ( typeRHS == UsualDataType.INT ) 
                    return (leftOperand:valueData:i64 * rightOperand:valueData:i)
                case ( typeRHS == UsualDataType.FLOAT ) 
                    return (leftOperand:valueData:i64 * rightOperand:valueData:f)
                case ( typeRHS == UsualDataType.INT64 ) 
                    return (leftOperand:valueData:i64 * rightOperand:valueData:i64)
                endcase
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}
            endif
            if (usualType != UsualDataType.FLOAT)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}
            endif
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f * rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f * rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f * rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}
        endif
        do case
        case ( typeRHS == UsualDataType.INT ) 
            return (leftOperand:valueData:i * rightOperand:valueData:i)
        case ( typeRHS == UsualDataType.FLOAT ) 
            return (leftOperand:valueData:i * rightOperand:valueData:f)
        case ( typeRHS == UsualDataType.INT64 ) 
            return (leftOperand:valueData:i * rightOperand:valueData:i64)
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for multiplication", leftOperand, rightOperand)}

    static operator >>(leftOperand as __Usual, rightOperand as Long) as __Usual
        local usualType as UsualDataType
        usualType := leftOperand:usualType
        if (usualType == UsualDataType.INT)
            return (leftOperand:valueData:i >> rightOperand)
        endif
        if (usualType != UsualDataType.INT64)
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for >> operator", leftOperand, rightOperand)}
        endif
    return (leftOperand:valueData:i64 >> rightOperand)

    static operator -(leftOperand as __Usual, rightOperand as __Usual) as __Usual
        local usualType as UsualDataType
        local typeRHS as UsualDataType
        usualType := leftOperand:usualType
        typeRHS := rightOperand:usualType
        do case
        case ( usualType == UsualDataType.INT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i - rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i - rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i - rightOperand:valueData:i64)
            endcase
        case ( usualType == UsualDataType.FLOAT ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:f - rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:f - rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:f - rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for substraction", leftOperand, rightOperand)}
        case ( usualType == UsualDataType.INT64 ) 
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return (leftOperand:valueData:i64 - rightOperand:valueData:i)
            case ( typeRHS == UsualDataType.FLOAT ) 
                return (leftOperand:valueData:i64 - rightOperand:valueData:f)
            case ( typeRHS == UsualDataType.INT64 ) 
                return (leftOperand:valueData:i64 - rightOperand:valueData:i64)
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for substraction", leftOperand, rightOperand)}
        otherwise
            if (usualType != UsualDataType.DATE)
                throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for substraction", leftOperand, rightOperand)}
            endif
            do case
            case ( typeRHS == UsualDataType.INT ) 
                return leftOperand:valueData:d:Subtract(System.TimeSpan.FromDays((real8)rightOperand:valueData:i ))
            case ( typeRHS == UsualDataType.FLOAT ) 
                return leftOperand:valueData:d:Subtract(System.TimeSpan.FromDays(rightOperand:valueData:f))
            case ( typeRHS == UsualDataType.INT64 ) 
                return leftOperand:valueData:d:Subtract(System.TimeSpan.FromDays((real8)rightOperand:valueData:i64 ))
            endcase
            throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for substraction", leftOperand, rightOperand)}
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} {1} for substraction", leftOperand, rightOperand)}

    static operator -(u as __Usual) as __Usual
        do case
        case ( u:usualType == UsualDataType.INT ) 
            return -u:valueData:i
        case ( u:usualType == UsualDataType.FLOAT ) 
            return -u:valueData:f
        case ( u:usualType == UsualDataType.INT64 ) 
            return -u:valueData:i64
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} unary minus op", u)}

    static operator +(u as __Usual) as __Usual
        do case
        case ( u:usualType == UsualDataType.INT ) 
            return u
        case ( u:usualType == UsualDataType.FLOAT ) 
            return u
        case ( u:usualType == UsualDataType.INT64 ) 
            return u
        endcase
        throw System.InvalidOperationException{String.Format("Arguments not compatible {0} unary plus op", u)}
	#endregion
	#region todo

        static operator implicit(u as __Usual) as __Array
            //
            if (u:IsNil)
                //
                return null
            endif
            if (IIF(u:IsArray,0,1) != 0)
                //
                throw System.InvalidOperationException{String.Format("Cannot convert {0} to ArrayList", u)}
            endif
            return (__Array)u:value 

	#endregion

end structure			


[StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit, CharSet:=System.Runtime.InteropServices.CharSet.Auto)];
internal structure Vulcan.UsualData
    // Fields
    [FieldOffset(0)];
    export d as System.DateTime
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

end structure

[StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit, CharSet:=System.Runtime.InteropServices.CharSet.Auto)];
internal structure UsualFlags
    [FieldOffset(0)];
    export type as UsualDataType
    [FieldOffset(1)];
    export byref as Byte
    [FieldOffset(2)];
    export width as Byte
    [FieldOffset(3)];
    export decimals as Byte
	constructor(t as UsualDataType)
		type := t
		decimals := 0
		width := 0
		byref := 0
end structure

public enum UsualDataType as Byte
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

