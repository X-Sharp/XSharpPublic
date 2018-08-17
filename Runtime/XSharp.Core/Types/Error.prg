
using System
using System.Collections.Generic
using System.Text
using System.Reflection
begin namespace XSharp
  /// <summary>XSharp Runtime base Error class</summary>
  class Error inherit Exception
    /// <summary>A string representing the name of the subsystem generating the error.</summary>
    property SubSystem as string auto
    /// <summary>An integer numeric value representing a Visual Objects generic error code.</summary>
    /// <Seealso cref="T:XSharp.Gencode"/>
    property Gencode as dword auto
    /// <summary>An integer numeric value representing a subsystem-specific error code.</summary>
    property SubCode as dword auto
    /// <summary>A string representing the name of the function or method in which the error occurred.</summary>
    property FuncSym as string auto
    /// <summary>A string representing the name used to open the file associated with the error condition.</summary>
    property FileName as string auto
    /// <summary>A constant indicating the severity of the error condition.</summary>
    /// <Seealso cref="T:XSharp.Severity"/>
    property Severity as dword auto
    /// <summary>A string that describes the error condition.</summary>
    property Description		as string auto
    property Arg				as string auto
    private  _ArgType			as dword
    property ArgType			as dword 
      get 
        return _ArgType 
      end get
      set 
        _ArgType := value
        _ArgTypeType := UsualTypeToType(value)
      end set
    end property
    private  _ArgTypeType 		as System.Type
    property ArgTypeType		as System.Type 
      get 
        return _ArgTypeType 
      end get
      set 
        _ArgTypeType := value
        _ArgType 	 := TypeToUsualType(value)
      end set
    end property
    
    private _ArgTypeReq			as dword
    property ArgTypeReq			as dword 
      get 
        return _ArgTypeReq 
      end get
      set 
        _ArgTypeReq := value
        _ArgTypeReqType := UsualTypeToType(value)
      end set
    end property
    private  _ArgTypeReqType 		as System.Type
    property ArgTypeReqType		as System.Type 
      get 
        return _ArgTypeReqType 
      end get
      set 
        _ArgTypeReqType := value
        _ArgTypeReq 	 := TypeToUsualType(value)
      end set
    end property
    
    property SubstituteType     as dword auto
    property ArgNum				as dword auto
    property MethodSelf			as object auto
    property CallFuncSym		as string auto
    property Args				as object[] auto
    property Tries				as int auto
    property CanDefault         as logic auto
    property CanRetry           as logic auto
    property CanSubstitute      as logic auto
    property Operation          as string auto
    property SubCodeText        as string auto
    property OSCode				as dword auto
    property FileHandle         as dword auto
    property MaxSize			as dword auto
    
    private method setDefaultValues() as void
    self:Gencode		:= EG_UNKNOWN
    self:Subcode		:= 0
    self:Subsystem		:= "BASE"
    self:Severity		:= ES_ERROR
    self:CanDefault		:= false
    self:CanRetry		:= false
    self:CanSubstitute	:= false
    self:Tries			:= 0
    self:FuncSym		:= ProcName(2)
    self:OSCode			:= 0
    self:Description    := self:Message
    
    constructor()
    self:setDefaultValues()
    return
    
    constructor (ex as Exception)
    super(ex.Message,ex)
    self:setDefaultValues()
    self:Description := ex:Message
    self:GenCode     := EG_EXCEPTION
    
    constructor (ex as Exception, cFuncName as string, cArgName as string, iArgNum as dword, aArgs params object[])
    super(ex.Message,ex)
    self:setDefaultValues()
    self:GenCode     := EG_EXCEPTION
    self:FuncSym     := cFuncName
    self:Arg         := cArgName
    self:ArgNum      := iArgNum
    self:Args		 := aArgs
    
    
    
    constructor (dwgencode as dword, cArg as string)
    super(ErrString( dwGenCode ))
    self:setDefaultValues()
    self:Gencode := dwGenCode
    self:Arg	 := cArg
    
    constructor (dwgencode as dword, cArg as string, cDescription as string)
    super(cDescription)
    self:setDefaultValues()
    self:Gencode		:= dwGenCode
    self:Arg			:= cArg
    self:Description	:= cDescription
    
    
    constructor (dwgencode as dword, dwSubCode as dword, cFuncName as string, cArgName as string, iArgNum as dword)
    super(ErrString( dwGenCode ))
    self:setDefaultValues()
    self:Gencode := dwgencode
    self:SubCode := dwSubcode
    self:FuncSym     := cFuncName
    self:Arg         := cArgName
    self:ArgNum      := iArgNum
    
    constructor (dwgencode as dword, dwSubCode := 0 as dword)
    self:setDefaultValues()
    self:Gencode := dwgencode
    self:SubCode := dwSubcode
    
    
    override method ToString() as string
      local sb as StringBuilder
      sb := StringBuilder{}
      sb:AppendLine("Description     :" + self:Description)
      sb:AppendLine("SubSystem       :" + self:SubSystem )
      sb:AppendLine("SubCode         :" + self:SubCode   )
      sb:AppendLine("GenCode         :" + ErrString(self:GenCode)  )
      sb:AppendLine("OsCode          :" + self:OsCode:ToString() )
      sb:AppendLine("ArgType         :" + TypeString(self:ArgType    ) )
      sb:AppendLine("ArgNum          :" + self:ArgNum:ToString()    )
      sb:AppendLine("FuncSym         :" + self:FuncSym   )
      sb:AppendLine("Severity        :" + self:Severity )
      sb:AppendLine("CanDefault      :" + self:CanDefault)
      sb:AppendLine("CanRetry        :" + self:CanRetry )
      sb:AppendLine("CanSubstitute   :" + self:CanSubstitute)
      sb:AppendLine("Operation       :" + self:Operation)
      sb:AppendLine("FileName        :" + self:FileName )
      sb:AppendLine("Tries           :" + self:Tries    )
      sb:AppendLine("SubCodeText     :" + self:SubCodeText)
      sb:AppendLine("Arg             :" + self:Arg)
      if self:ArgTypeReqType == null
        sb:AppendLine("ArgTypeReq      :")
      else
        sb:AppendLine("ArgTypeReq      :" + self:ArgTypeReqType:FullName)
      end if
      sb:AppendLine("CallFuncSym     :" + self:CallFuncSym  )
      return sb:ToString()
      
      
    method @@Throw as void strict
    // must override in subclass
    return
    
    #region STATIC methods TO construct an error
    static method ArgumentError(cFuncName as string, name as string, description as string) as Error
    return ArgumentError(cFuncName, name, description, 0)
    
    static method ArgumentError(cFuncName as string, name as string, iArgnum  as dword, aArgs params object[]) as Error
    var err			:= Error{Gencode.EG_ARG, name, ErrString( EG_ARG )}
    err:FuncSym     := cFuncName
    err:Description := err:Message
    err:Argnum		:= iArgNum
    err:args			:= aArgs
    return err
    
    
    static method ArgumentError(cFuncName as string, name as string, description as string, iArgnum as dword) as Error
    var err			:= Error{Gencode.EG_ARG, name, description}
    err:FuncSym     := cFuncName
    err:Description := err:Message
    err:Argnum		:= iArgNum
    return err
    
    static method WrapRawException( ex as Exception ) as Error
    local e as Error
    e			  := Error{ ex }
    e:Description := ErrString( EG_EXCEPTION )
    return e
    
    static method VOError( dwGenCode as dword, cFuncName as string, cArgName as string, iArgNum as dword, aArgs as object[] ) as Error
    return VOError( null , dwGenCode, cFuncName, cArgName, iArgNum, aArgs )
    
    static method VOError( ex as Exception, dwGenCode as dword, cFuncName as string, cArgName as string, iArgNum as dword, aArgs as object[]  ) as Error
    local e as Error
    e			  := Error{ ex, cFuncName, cArgName, iArgNum, aArgs }
    e:GenCode     := dwGenCode
    e:Description := ErrString( dwGenCode )
    return e
    
    static method VODBError( dwGenCode as dword, dwSubCode as dword, cFuncName as string ) as Error
    local e as Error
    e := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:FuncSym     := cFuncName
    e:Description := ErrString( dwGenCode )
    return e
    
    static method VODBError( dwGenCode as dword, dwSubCode as dword, aArgs params object[] ) as Error
    local e as Error
    e			  := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:Description := ErrString( dwGenCode )
    e:Args        := aArgs
    return e
    
    static method VODBError( dwGenCode as dword, dwSubCode as dword, cFuncName as string, cArgName as string, iArgNum as dword, aArgs params object[] ) as Error
    local e as Error
    e := Error{dwGenCode, dwSubCode, cFuncName, cArgName, iArgNum}
    e:SubSystem   := "DBCMD"
    e:Description := ErrString( dwGenCode )
    e:Args        := aArgs
    return e
    
    static method DataTypeError( cFuncName as string, cArgName as string, iArgNum as dword, aArgs params object[] ) as Error
    local e as Error
    e				:= Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
    e:GenCode     := EG_DATATYPE
    e:Description := __CavoStr( VOErrors.DATATYPEERROR )
    return e
    
    static method ArgumentError( cFuncName as string, cArgName as string, iArgNum as dword, cDescription as string, aArgs params object[]) as Error
    local e as Error
    e				:= Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
    e:GenCode     := EG_ARG
    e:Description := cDescription
    return e
    
    static method NullArgumentError( cFuncName as string, cArgName as string, iArgNum as dword ) as Error
    local e as Error
    e := Error{ ArgumentNullException{} ,cFuncName, cArgName, iArgNum}
    e:GenCode     := EG_ARG
    e:Description := __CavoStr( VOErrors.ARGISNULL )
    return e
    
    static method BoundError( cFuncName as string, cArgName as string, iArgNum as dword, aArgs params object[] ) as Error
    local e as Error
    e := Error{ ArgumentException{ErrString( EG_BOUND) } }
    e:Severity    := ES_ERROR
    e:GenCode     := EG_BOUND
    e:SubSystem   := "BASE"
    e:FuncSym     := cFuncName
    e:Arg         := cArgName
    e:ArgNum      := iArgNum
    e:Args        := aArgs
    return e
    
    
    #endregion
    
    static method TypeToUsualType(oType as System.Type) as dword
    switch Type.GetTypeCode(oType)
    case TypeCode.Boolean
      return __UsualType.logic
    case TypeCode.Byte
      return __UsualType.byte
    case TypeCode.Char
      return __UsualType.Char
    case TypeCode.DateTime
      return __UsualType.DATETIME
    case TypeCode.DBNull
      return __UsualType.object
    case TypeCode.Decimal
      return __UsualType.DECIMAL
    case TypeCode.Double
      return __UsualType.real8
    case TypeCode.Empty
      return __UsualType.void
    case TypeCode.Int16
      return __UsualType.shortint
    case TypeCode.Int32
      return __UsualType.long
    case TypeCode.Int64
      return __UsualType.int64
    case TypeCode.SByte
      return __UsualType.byte
    case TypeCode.Single
      return __UsualType.real4
    case TypeCode.UInt16
      return __UsualType.word
    case TypeCode.UInt32
      return __UsualType.dword
    case TypeCode.UInt64
      return __UsualType.uint64
    otherwise
        switch oType:FullName:ToLower()
        case "xsharp.__array"
          return __UsualType.array
        case "xsharp.__codeblock"
          return __UsualType.codeblock
        case "xsharp.__vodate"
          return __UsualType.date
        case "xsharp.__vofloat"
          return __UsualType.float
        case "xsharp.__psz"
          return __UsualType.psz
        case "xsharp.__symbol"
          return __UsualType.symbol
        case "xsharp.__usual"
          return __UsualType.usual
        case "system.intptr"
          return __UsualType.ptr
        
      end switch
    end switch
    return __UsualType.void
    static method UsualTypeTotype(dwType as dword) as System.Type
    local typename := null as string
    switch dwType
    case __UsualType.array
      typeName := "XSharp.__Array"
    case __UsualType.byte
      return typeof(System.Byte)
    case __UsualType.char
      return typeof(System.Char)
    case __UsualType.codeblock
      typeName := "XSharp.__CodeBlock"
    case __UsualType.date
      typeName := "XSharp.__VODate"
    case __UsualType.dword
      return typeof(System.UInt32)
    case __UsualType.int64
      return typeof(System.Int64)
    case __UsualType.float
      typeName := "XSharp.__VOFloat"
    case __UsualType.logic
      return typeof(System.Boolean)
    case __UsualType.long
      return typeof(System.Int32)
    case __UsualType.object
      return typeof(System.Object)
    case __UsualType.psz
      typeName := "XSharp.__Psz"
    case __UsualType.ptr
      return typeof(System.IntPtr)
    case __UsualType.real4
      return typeof(System.Single)
    case __UsualType.real8
      return typeof(System.Double)
    case __UsualType.shortint
      return typeof(System.Int16)
    case __UsualType.string
      return typeof(System.String)
    case __UsualType.symbol
      typeName := "XSharp.__Symbol"
    case __UsualType.uint64
      return typeof(System.Int64)
    case __UsualType.usual
      typeName := "XSharp.__Usual"
    case __UsualType.void
      return typeof(System.Void)
    case __UsualType.word
      return typeof(System.Uint16)
    end switch
    // lookup types in XSharp.VO
    if typename != null
        foreach asm as Assembly in AppDomain.CurrentDomain:GetAssemblies()
          if asm:Getname():Name:ToLower() == "xsharp.vo"
            var type := asm:GetType(typeName, false, true)
            if type != null
              return type
            endif
          endif
        next
    endif
    return null
  end class

  enum __UsualType as byte
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

end namespace




