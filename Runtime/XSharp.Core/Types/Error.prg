
USING System
USING System.Collections.Generic
USING System.Text
USING System.Reflection
BEGIN NAMESPACE XSharp
  /// <summary>XSharp Runtime base Error class</summary>
  CLASS Error INHERIT Exception
    /// <summary>A string representing the name of the subsystem generating the error.</summary>
    PROPERTY SubSystem AS STRING AUTO
    /// <summary>An integer numeric value representing a Visual Objects generic error code.</summary>
    /// <Seealso cref="T:XSharp.Gencode"/>
    PROPERTY Gencode AS DWORD AUTO
    /// <summary>An integer numeric value representing a subsystem-specific error code.</summary>
    PROPERTY SubCode AS DWORD AUTO
    /// <summary>A string representing the name of the function or method in which the error occurred.</summary>
    PROPERTY FuncSym AS STRING AUTO
    /// <summary>A string representing the name used to open the file associated with the error condition.</summary>
    PROPERTY FileName AS STRING AUTO
    /// <summary>A constant indicating the severity of the error condition.</summary>
    /// <Seealso cref="T:XSharp.Severity"/>
    PROPERTY Severity AS DWORD AUTO
    /// <summary>A string that describes the error condition.</summary>
    PROPERTY Description		AS STRING AUTO
    PROPERTY Arg				AS STRING AUTO
    PRIVATE  _ArgType			AS DWORD
    PROPERTY ArgType			AS DWORD 
      GET 
        RETURN _ArgType 
      END GET
      SET 
        _ArgType := VALUE
        _ArgTypeType := UsualTypeToType(VALUE)
      END SET
    END PROPERTY
    PRIVATE  _ArgTypeType 		AS System.Type
    PROPERTY ArgTypeType		AS System.Type 
      GET 
        RETURN _ArgTypeType 
      END GET
      SET 
        _ArgTypeType := VALUE
        _ArgType 	 := TypeToUsualType(VALUE)
      END SET
    END PROPERTY
    
    PRIVATE _ArgTypeReq			AS DWORD
    PROPERTY ArgTypeReq			AS DWORD 
      GET 
        RETURN _ArgTypeReq 
      END GET
      SET 
        _ArgTypeReq := VALUE
        _ArgTypeReqType := UsualTypeToType(VALUE)
      END SET
    END PROPERTY
    PRIVATE  _ArgTypeReqType 		AS System.Type
    PROPERTY ArgTypeReqType		AS System.Type 
      GET 
        RETURN _ArgTypeReqType 
      END GET
      SET 
        _ArgTypeReqType := VALUE
        _ArgTypeReq 	 := TypeToUsualType(VALUE)
      END SET
    END PROPERTY
    
    PROPERTY SubstituteType     AS DWORD AUTO
    PROPERTY ArgNum				AS DWORD AUTO
    PROPERTY MethodSelf			AS OBJECT AUTO
    PROPERTY CallFuncSym		AS STRING AUTO
    PROPERTY Args				AS OBJECT[] AUTO
    PROPERTY Tries				AS INT AUTO
    PROPERTY CanDefault         AS LOGIC AUTO
    PROPERTY CanRetry           AS LOGIC AUTO
    PROPERTY CanSubstitute      AS LOGIC AUTO
    PROPERTY Operation          AS STRING AUTO
    PROPERTY SubCodeText        AS STRING AUTO
    PROPERTY OSCode				AS DWORD AUTO
    PROPERTY FileHandle         AS DWORD AUTO
    PROPERTY MaxSize			AS DWORD AUTO
    
    PRIVATE METHOD setDefaultValues() AS VOID
    SELF:Gencode		:= EG_UNKNOWN
    SELF:Subcode		:= 0
    SELF:Subsystem		:= "BASE"
    SELF:Severity		:= ES_ERROR
    SELF:CanDefault		:= FALSE
    SELF:CanRetry		:= FALSE
    SELF:CanSubstitute	:= FALSE
    SELF:Tries			:= 0
    SELF:FuncSym		:= ProcName(2)
    SELF:OSCode			:= 0
    SELF:Description    := SELF:Message
    
    CONSTRUCTOR()
    SELF:setDefaultValues()
    RETURN
    
    CONSTRUCTOR (ex AS Exception)
    SUPER(ex.Message,ex)
    SELF:setDefaultValues()
    SELF:Description := ex:Message
    SELF:GenCode     := EG_EXCEPTION
    
    CONSTRUCTOR (ex AS Exception, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[])
    SUPER(ex.Message,ex)
    SELF:setDefaultValues()
    SELF:GenCode     := EG_EXCEPTION
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    SELF:Args		 := aArgs
    
    
    
    CONSTRUCTOR (dwgencode AS DWORD, cArg AS STRING)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwGenCode
    SELF:Arg	 := cArg
    
    CONSTRUCTOR (dwgencode AS DWORD, cArg AS STRING, cDescription AS STRING)
    SUPER(cDescription)
    SELF:setDefaultValues()
    SELF:Gencode		:= dwGenCode
    SELF:Arg			:= cArg
    SELF:Description	:= cDescription
    
    
    CONSTRUCTOR (dwgencode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwgencode
    SELF:SubCode := dwSubcode
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    
    CONSTRUCTOR (dwgencode AS DWORD, dwSubCode := 0 AS DWORD)
    SELF:setDefaultValues()
    SELF:Gencode := dwgencode
    SELF:SubCode := dwSubcode
    
    
    OVERRIDE METHOD ToString() AS STRING
      LOCAL sb AS StringBuilder
      sb := StringBuilder{}
      sb:AppendLine("Description     :" + SELF:Description)
      sb:AppendLine("SubSystem       :" + SELF:SubSystem )
      sb:AppendLine("SubCode         :" + SELF:SubCode   )
      sb:AppendLine("GenCode         :" + ErrString(SELF:GenCode)  )
      sb:AppendLine("OsCode          :" + SELF:OsCode:ToString() )
      sb:AppendLine("ArgType         :" + TypeString(SELF:ArgType    ) )
      sb:AppendLine("ArgNum          :" + SELF:ArgNum:ToString()    )
      sb:AppendLine("FuncSym         :" + SELF:FuncSym   )
      sb:AppendLine("Severity        :" + SELF:Severity )
      sb:AppendLine("CanDefault      :" + SELF:CanDefault)
      sb:AppendLine("CanRetry        :" + SELF:CanRetry )
      sb:AppendLine("CanSubstitute   :" + SELF:CanSubstitute)
      sb:AppendLine("Operation       :" + SELF:Operation)
      sb:AppendLine("FileName        :" + SELF:FileName )
      sb:AppendLine("Tries           :" + SELF:Tries    )
      sb:AppendLine("SubCodeText     :" + SELF:SubCodeText)
      sb:AppendLine("Arg             :" + SELF:Arg)
      IF SELF:ArgTypeReqType == NULL
        sb:AppendLine("ArgTypeReq      :")
      ELSE
        sb:AppendLine("ArgTypeReq      :" + SELF:ArgTypeReqType:FullName)
      END IF
      sb:AppendLine("CallFuncSym     :" + SELF:CallFuncSym  )
      RETURN sb:ToString()
      
      
    METHOD @@Throw AS VOID STRICT
    // must override in subclass
    RETURN
    
    #region STATIC methods TO construct an error
    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, description AS STRING) AS Error
    RETURN ArgumentError(cFuncName, name, description, 0)
    
    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, iArgnum  AS DWORD, aArgs PARAMS OBJECT[]) AS Error
    VAR err			:= Error{Gencode.EG_ARG, name, ErrString( EG_ARG )}
    err:FuncSym     := cFuncName
    err:Description := err:Message
    err:Argnum		:= iArgNum
    err:args			:= aArgs
    RETURN err
    
    
    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, description AS STRING, iArgnum AS DWORD) AS Error
    VAR err			:= Error{Gencode.EG_ARG, name, description}
    err:FuncSym     := cFuncName
    err:Description := err:Message
    err:Argnum		:= iArgNum
    RETURN err
    
    STATIC METHOD WrapRawException( ex AS Exception ) AS Error
    LOCAL e AS Error
    e			  := Error{ ex }
    e:Description := ErrString( EG_EXCEPTION )
    RETURN e
    
    STATIC METHOD VOError( dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs AS OBJECT[] ) AS Error
    RETURN VOError( NULL , dwGenCode, cFuncName, cArgName, iArgNum, aArgs )
    
    STATIC METHOD VOError( ex AS Exception, dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs AS OBJECT[]  ) AS Error
    LOCAL e AS Error
    e			  := Error{ ex, cFuncName, cArgName, iArgNum, aArgs }
    e:GenCode     := dwGenCode
    e:Description := ErrString( dwGenCode )
    RETURN e
    
    STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING ) AS Error
    LOCAL e AS Error
    e := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:FuncSym     := cFuncName
    e:Description := ErrString( dwGenCode )
    RETURN e
    
    STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e			  := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:Description := ErrString( dwGenCode )
    e:Args        := aArgs
    RETURN e
    
    STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e := Error{dwGenCode, dwSubCode, cFuncName, cArgName, iArgNum}
    e:SubSystem   := "DBCMD"
    e:Description := ErrString( dwGenCode )
    e:Args        := aArgs
    RETURN e
    
    STATIC METHOD DataTypeError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e				:= Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
    e:GenCode     := EG_DATATYPE
    e:Description := __CavoStr( VOErrors.DATATYPEERROR )
    RETURN e
    
    STATIC METHOD ArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, cDescription AS STRING, aArgs PARAMS OBJECT[]) AS Error
    LOCAL e AS Error
    e				:= Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
    e:GenCode     := EG_ARG
    e:Description := cDescription
    RETURN e
    
    STATIC METHOD NullArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD ) AS Error
    LOCAL e AS Error
    e := Error{ ArgumentNullException{} ,cFuncName, cArgName, iArgNum}
    e:GenCode     := EG_ARG
    e:Description := __CavoStr( VOErrors.ARGISNULL )
    RETURN e
    
    STATIC METHOD BoundError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e := Error{ ArgumentException{ErrString( EG_BOUND) } }
    e:Severity    := ES_ERROR
    e:GenCode     := EG_BOUND
    e:SubSystem   := "BASE"
    e:FuncSym     := cFuncName
    e:Arg         := cArgName
    e:ArgNum      := iArgNum
    e:Args        := aArgs
    RETURN e
    
    
    #endregion
    
    STATIC METHOD TypeToUsualType(oType AS System.Type) AS DWORD
    SWITCH Type.GetTypeCode(oType)
    CASE TypeCode.Boolean
      RETURN __UsualType.logic
    CASE TypeCode.Byte
      RETURN __UsualType.byte
    CASE TypeCode.Char
      RETURN __UsualType.Char
    CASE TypeCode.DateTime
      RETURN __UsualType.DATETIME
    CASE TypeCode.DBNull
      RETURN __UsualType.object
    CASE TypeCode.Decimal
      RETURN __UsualType.DECIMAL
    CASE TypeCode.Double
      RETURN __UsualType.real8
    CASE TypeCode.Empty
      RETURN __UsualType.void
    CASE TypeCode.Int16
      RETURN __UsualType.shortint
    CASE TypeCode.Int32
      RETURN __UsualType.long
    CASE TypeCode.Int64
      RETURN __UsualType.int64
    CASE TypeCode.SByte
      RETURN __UsualType.byte
    CASE TypeCode.Single
      RETURN __UsualType.real4
    CASE TypeCode.UInt16
      RETURN __UsualType.word
    CASE TypeCode.UInt32
      RETURN __UsualType.dword
    CASE TypeCode.UInt64
      RETURN __UsualType.uint64
    OTHERWISE
        SWITCH oType:FullName:ToLower()
        CASE "xsharp.__array"
          RETURN __UsualType.array
        CASE "xsharp.__codeblock"
          RETURN __UsualType.codeblock
        CASE "xsharp.__vodate"
          RETURN __UsualType.date
        CASE "xsharp.__vofloat"
          RETURN __UsualType.float
        CASE "xsharp.__psz"
          RETURN __UsualType.psz
        CASE "xsharp.__symbol"
          RETURN __UsualType.symbol
        CASE "xsharp.__usual"
          RETURN __UsualType.usual
        CASE "system.intptr"
          RETURN __UsualType.ptr
        
      END SWITCH
    END SWITCH
    RETURN __UsualType.void
    STATIC METHOD UsualTypeTotype(dwType AS DWORD) AS System.Type
    LOCAL typename := NULL AS STRING
    SWITCH dwType
    CASE __UsualType.array
      typeName := "XSharp.__Array"
    CASE __UsualType.byte
      RETURN typeof(System.Byte)
    CASE __UsualType.char
      RETURN typeof(System.Char)
    CASE __UsualType.codeblock
      typeName := "XSharp.__CodeBlock"
    CASE __UsualType.date
      typeName := "XSharp.__VODate"
    CASE __UsualType.dword
      RETURN typeof(System.UInt32)
    CASE __UsualType.int64
      RETURN typeof(System.Int64)
    CASE __UsualType.float
      typeName := "XSharp.__VOFloat"
    CASE __UsualType.logic
      RETURN typeof(System.Boolean)
    CASE __UsualType.long
      RETURN typeof(System.Int32)
    CASE __UsualType.object
      RETURN typeof(System.Object)
    CASE __UsualType.psz
      typeName := "XSharp.__Psz"
    CASE __UsualType.ptr
      RETURN typeof(System.IntPtr)
    CASE __UsualType.real4
      RETURN typeof(System.Single)
    CASE __UsualType.real8
      RETURN typeof(System.Double)
    CASE __UsualType.shortint
      RETURN typeof(System.Int16)
    CASE __UsualType.string
      RETURN typeof(System.String)
    CASE __UsualType.symbol
      typeName := "XSharp.__Symbol"
    CASE __UsualType.uint64
      RETURN typeof(System.Int64)
    CASE __UsualType.usual
      typeName := "XSharp.__Usual"
    CASE __UsualType.void
      RETURN typeof(System.Void)
    CASE __UsualType.word
      RETURN typeof(System.Uint16)
    END SWITCH
    // lookup types in XSharp.VO
    IF typename != NULL
        FOREACH asm AS Assembly IN AppDomain.CurrentDomain:GetAssemblies()
          IF asm:Getname():Name:ToLower() == "xsharp.vo"
            VAR type := asm:GetType(typeName, FALSE, TRUE)
            IF type != NULL
              RETURN type
            ENDIF
          ENDIF
        NEXT
    ENDIF
    RETURN NULL
  END CLASS

  ENUM __UsualType AS BYTE
        // These numbers must match with the types defined in the compiler
        // They also match with the USUAL types in VO (BaseType.h)
        MEMBER @@Void		:=0
        MEMBER @@Long		:=1
        MEMBER @@Date		:=2
        MEMBER @@Float		:=3
        MEMBER @@Fixed      := 4 // Note # 4 (FIXED) was defined but never used in VO
        MEMBER @@Array		:=5
        MEMBER @@Object		:=6
        MEMBER @@String		:=7
        MEMBER @@Logic		:=8
        MEMBER @@CodeBlock	:=9
        MEMBER @@Symbol		:=10
        // see below for missing values
        // The follow numbers are defined but never stored inside a USUAL in VO and Vulcan
        MEMBER @@Byte		:=11
        MEMBER @@ShortInt	:=12
        MEMBER @@Word		:=13
        MEMBER @@DWord		:=14
        MEMBER @@Real4		:=15
        MEMBER @@Real8		:=16
        MEMBER @@Psz		:=17
        MEMBER @@Ptr		:=18
        MEMBER @@Usual		:=19	// USUAL by Ref, not implemented in Vulcan
        // 20 and 21 not used
        MEMBER @@Int64		:=22
        MEMBER @@Uint64     :=23
        MEMBER @@Char		:=24    // not stored in a usual
        MEMBER @@Dynamic    :=25
        MEMBER @@DateTime	:=26
        MEMBER @@Decimal	:=27
        MEMBER @@Memo		:=32	// Used in RDD system in VO
        MEMBER @@Invalid    :=99
    END ENUM

END NAMESPACE




