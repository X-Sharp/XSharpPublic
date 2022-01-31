//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Reflection
USING System.Diagnostics
BEGIN NAMESPACE XSharp
  /// <summary>XSharp Runtime base Error class</summary>
  CLASS Error INHERIT Exception
    /// <summary>A string representing the name of the subsystem generating the error.</summary>
    VIRTUAL PROPERTY SubSystem AS STRING AUTO  := "BASE"
    /// <summary>An integer numeric value representing a Visual Objects generic error code.</summary>
    /// <Seealso cref="Gencode"/>
    VIRTUAL PROPERTY Gencode AS DWORD AUTO    := EG_UNKNOWN
    /// <summary>An string containing the description of the Gencode.</summary>
    VIRTUAL PROPERTY GenCodeText AS STRING GET IIF (Gencode != 0, ErrString(Gencode), "Unknown GenCode")
    /// <summary>An integer numeric value representing a subsystem-specific error code.</summary>
    VIRTUAL PROPERTY SubCode AS DWORD AUTO    := 0
    /// <summary>An string containing the description of the SubCode.</summary>
    VIRTUAL PROPERTY SubCodeText AS STRING GET IIF(!String.IsNullOrEmpty(_Subcode), _Subcode, IIF (SubCode != 0, __CavoStr(SubCode), "Unknown SubCode")) SET _Subcode := value
    PRIVATE _Subcode as STRING
    /// <summary>A string representing the name of the function or method in which the error occurred.</summary>
    VIRTUAL PROPERTY FuncSym AS STRING AUTO   := ""
    /// <summary>A string representing the name used to open the file associated with the error condition.</summary>
    VIRTUAL PROPERTY FileName AS STRING AUTO  := ""
    /// <summary>A constant indicating the severity of the error condition.</summary>
    /// <Seealso cref="Severity"/>
    VIRTUAL PROPERTY Severity AS DWORD AUTO   := ES_ERROR
    /// <summary>A string that describes the error condition.</summary>
    VIRTUAL PROPERTY Description		AS STRING AUTO := ""
    /// <summary>A string representing the argument supplied to an operator or function when an argument error occurs.</summary>
    VIRTUAL PROPERTY Arg				AS STRING AUTO := ""
    PRIVATE  _ArgType			AS DWORD
    /// <summary>A numeric value representing the data type of the argument that raised the error.</summary>
    VIRTUAL PROPERTY ArgType			AS DWORD
      GET
        RETURN _ArgType
      END GET
      SET
        _ArgType := value
        _ArgTypeType := UsualTypeToType(value)
      END SET
    END PROPERTY
    PRIVATE  _ArgTypeType 		:= NULL AS System.Type
    /// <summary>The system type representing the data type of the argument that raised the error.</summary>
    VIRTUAL PROPERTY ArgTypeType		AS System.Type
      GET
        RETURN _ArgTypeType
      END GET
      SET
        _ArgTypeType := value
        _ArgType 	 := TypeToUsualType(value)
      END SET
    END PROPERTY

    PRIVATE _ArgTypeReq		AS DWORD
    /// <summary>A numeric value representing the expected type of the argument that raised the error.</summary>
    VIRTUAL PROPERTY ArgTypeReq			AS DWORD
      GET
        RETURN _ArgTypeReq
      END GET
      SET
        _ArgTypeReq := value
        _ArgTypeReqType := UsualTypeToType(value)
      END SET
    END PROPERTY
    PRIVATE  _ArgTypeReqType := NULL	AS System.Type
    /// <summary>The system type representing the expected type of the argument that raised the error.</summary>
    VIRTUAL PROPERTY ArgTypeReqType		AS System.Type
      GET
        RETURN _ArgTypeReqType
      END GET
      SET
        _ArgTypeReqType := value
        _ArgTypeReq 	 := TypeToUsualType(value)
      END SET
    END PROPERTY
    /// <summary>A numeric value representing the type of the new result that the error handler substitutes for the operation that produced the error condition.</summary>
    VIRTUAL PROPERTY SubstituteType     AS DWORD AUTO
    /// <summary>A numeric value representing the number of the argument supplied to an operator or function when an argument error occurs.</summary>
    VIRTUAL PROPERTY ArgNum				AS DWORD AUTO
    /// <summary>An object representing the SELF of the method in which the error occurred.</summary>
    VIRTUAL PROPERTY MethodSelf			AS OBJECT AUTO
    /// <summary>A symbol representing the calling function of the function in which the error occurred.</summary>
    VIRTUAL PROPERTY CallFuncSym		AS STRING AUTO
    /// <summary>An array of the arguments supplied to an operator or function when an argument error occurs.</summary>
    VIRTUAL PROPERTY Args				AS OBJECT[] AUTO
    /// <summary>An integer numeric value representing the number of times the failed operation has been attempted.</summary>
    VIRTUAL PROPERTY Tries				AS INT AUTO := 0
    /// <summary>A logical value indicating whether the subsystem can perform default error recovery for the error condition.</summary>
    VIRTUAL PROPERTY CanDefault         AS LOGIC AUTO
    /// <summary>A logical value indicating whether the subsystem can retry the operation that caused the error condition.</summary>
    VIRTUAL PROPERTY CanRetry           AS LOGIC AUTO
    /// <summary>A logical value indicating whether a new result can be substituted for the operation that produced the error condition.</summary>
    VIRTUAL PROPERTY CanSubstitute      AS LOGIC AUTO
    /// <summary>A string that describes the operation being attempted when the error occurred.</summary>
    VIRTUAL PROPERTY Operation          AS STRING AUTO := ""
    /// <summary>A value of 0 indicates that the error condition was not caused by an error from the operating system.</summary>
    VIRTUAL PROPERTY OSCode				AS DWORD AUTO := 0
    /// <summary>Descripion of the OSCode</summary>
    VIRTUAL PROPERTY OSCodeText			AS STRING GET IIF(OSCode == 0, "", DosErrString(OSCode))
    /// <summary>A numeric value representing the file handle supplied to a function when an file error occurs.</summary>
    VIRTUAL PROPERTY FileHandle         AS DWORD AUTO
    /// <summary>A numeric value representing a boundary condition for an operation (such as string overflow or array bound error).</summary>
    VIRTUAL PROPERTY MaxSize			AS DWORD AUTO
    /// <summary>A pointer to the function in which the error occurred.</summary>
    /// <remarks><em>Note</em> This property is for compatibility only. It is not being used in the X# runtime.</remarks>
    VIRTUAL PROPERTY FuncPtr            AS IntPtr AUTO := IntPtr.Zero


    /// <summary>A value of any data type unused by the Error system.  It is provided as a user-definable slot, allowing arbitrary information to be attached to an Error object and retrieved later</summary>
    VIRTUAL PROPERTY Cargo              AS OBJECT AUTO
    /// <summary>Call stack from the moment where the error object was created</summary>
    VIRTUAL PROPERTY Stack              AS STRING GET SELF:StackTrace SET SELF:StackTrace := Value

	PRIVATE _StackTrace AS STRING
    OVERRIDE PROPERTY StackTrace         AS STRING
    	GET
    		IF String.IsNullOrEmpty(SELF:_StackTrace)
    			RETURN SUPER:StackTrace
    		END IF
    		RETURN SELF:_StackTrace
    	END GET
    	SET
    		SELF:_StackTrace := value
    	END SET
    END PROPERTY


    PRIVATE METHOD setDefaultValues() AS VOID
        SELF:FuncSym := ProcName(2)
        SELF:Args    := <OBJECT>{}
        IF String.IsNullOrEmpty(SELF:StackTrace)
            SELF:_StackTrace  := ErrorStack(2)
        ENDIF
        RETURN

    /// <summary>Create an Error Object</summary>
    CONSTRUCTOR()
    SELF:setDefaultValues()
    RETURN

    /// <summary>Create an Error Object with the specified Description</summary>
    CONSTRUCTOR (msg AS STRING)
    SUPER(msg)
    SELF:setDefaultValues()
    SELF:Description := msg
    SELF:Gencode     := EG_EXCEPTION
    RETURN

    /// <summary>Create an Error Object with the Innner Exception</summary>
    CONSTRUCTOR (ex AS Exception)
    SUPER(ex.Message,ex)
    SELF:setDefaultValues()
    IF ex IS Error
        // Clone Error properties from Inner Exception
        LOCAL e := (Error) ex AS Error
        VAR props := typeof(Error):GetProperties()
        FOREACH oProp AS PropertyInfo IN props
            IF oProp:CanWrite
                oProp:SetValue(SELF, oProp:GetValue(e))
            ENDIF
         NEXT
         IF SELF:SubCode == 0
            SELF:SubCodeText := ""
         ENDIF
    ELSE
        SELF:Description := ex:Message
        SELF:Gencode     := EG_EXCEPTION
        IF ! String.IsNullOrEmpty(ex:Source)
            SELF:SubSystem := ex:Source
        ENDIF
        VAR sStack       := ErrorStack( StackTrace{ex,TRUE},UInt32.MaxValue)
        IF !sStack:StartsWith("*EmptyCallStack*")
            SELF:_StackTrace := sStack + SELF:_StackTrace
        ENDIF
    ENDIF
    IF String.IsNullOrEmpty(SELF:StackTrace)
        SELF:StackTrace  := ErrorStack(2)
    ENDIF

    /// <summary>Create an Error Object with the Innner Exception and other parameters</summary>
    CONSTRUCTOR (ex AS Exception, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[])
    SUPER(ex.Message,ex)
    SELF:setDefaultValues()
    SELF:Gencode     := EG_EXCEPTION
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    SELF:Args		 := aArgs


    /// <summary>Create an Error Object for a Gencode and Argument Name.</summary>
    CONSTRUCTOR (dwGenCode AS DWORD, cArg AS STRING)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwGenCode
    SELF:Arg	 := cArg
    SELF:Description := ErrString( dwGenCode )

    /// <summary>Create an Error Object for a Gencode, Argument Name and Description.</summary>
    CONSTRUCTOR (dwGenCode AS DWORD, cArg AS STRING, cDescription AS STRING)
    SUPER(cDescription)
    SELF:setDefaultValues()
    SELF:Gencode		:= dwGenCode
    SELF:Arg			:= cArg
    SELF:Description	:= cDescription


    /// <summary>Create an Error Object.</summary>
    CONSTRUCTOR (dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwGenCode
    SELF:SubCode := dwSubCode
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    SELF:Description := ErrString( dwGenCode )

    /// <summary>Create an Error Object.</summary>
    CONSTRUCTOR (dwGenCode AS DWORD, dwSubCode := 0 AS DWORD)
    SELF:setDefaultValues()
    SELF:Gencode := dwGenCode
    SELF:SubCode := dwSubCode
    SELF:Description := ErrString( dwGenCode )

    PRIVATE STATIC METHOD LangString(e as VOErrors) AS STRING
        local cString := __CavoStr(e):Trim() as string
        IF cString:EndsWith(":")
            cString := cString:Substring(0, cString:Length-1):Trim()
        endif
        return cString+e" :\t"

    /// <inheritdoc />
    OVERRIDE METHOD ToString() AS STRING
      LOCAL sb AS StringBuilder
      LOCAL nGenCode AS Gencode
      nGenCode := (Gencode) SELF:Gencode
      sb := StringBuilder{}
      sb:AppendLine( LangString(VOErrors.ERROR_DESCRIPTION) + SELF:Description)
      sb:AppendLine( LangString(VOErrors.ERROR_SUBSYSTEM) + SELF:SubSystem )
      sb:AppendLine( LangString(VOErrors.ERROR_GENCODE) + nGenCode:ToString()  +" " +SELF:GenCodeText  )
      IF SELF:SubCode != 0
        sb:AppendLine( LangString(VOErrors.ERROR_SUBCODE) + SELF:SubCode:ToString() +" "+SELF:SubCodeText)
      ENDIF
      IF SELF:OSCode != 0
        sb:AppendLine( LangString(VOErrors.ERROR_OSCODE)  + SELF:OSCode:ToString() +" " +SELF:OSCodeText )
      ENDIF
      IF !String.IsNullOrEmpty(SELF:FuncSym)
        sb:AppendLine(LangString(VOErrors.ERROR_FUNCSYM) + SELF:FuncSym   )
      ENDIF
      LOCAL sev := (Severity) SELF:Severity AS Severity
      sb:AppendLine(LangString(VOErrors.ERROR_SEVERITY)     + sev:ToString() )
      sb:AppendLine(LangString(VOErrors.ERROR_CANDEFAULT)   + SELF:CanDefault:ToString())
      sb:AppendLine(LangString(VOErrors.ERROR_CANRETRY)     +SELF:CanRetry:ToString() )
      sb:AppendLine(LangString(VOErrors.ERROR_CANSUBSTITUTE) +SELF:CanSubstitute:ToString())
      IF ! String.IsNullOrEmpty(SELF:Operation)
        sb:AppendLine(LangString(VOErrors.ERROR_OPERATION) + SELF:Operation)
      ENDIF
      IF ! String.IsNullOrEmpty(SELF:FileName)
        sb:AppendLine(LangString(VOErrors.ERROR_FILENAME) + SELF:FileName )
      ENDIF
      IF SELF:Tries != 0
          sb:AppendLine(LangString(VOErrors.ERROR_TRIES) + SELF:Tries:ToString()    )
     ENDIF
      IF SELF:ArgType != 0
        sb:AppendLine(LangString(VOErrors.ERROR_ARGTYPE) 	+ TypeString(SELF:ArgType    ) )
      ENDIF
      IF SELF:ArgNum != 0
        sb:AppendLine(LangString(VOErrors.ERROR_ARGNUM) 	+ SELF:ArgNum:ToString()    )
      ENDIF
      IF ! String.IsNullOrEmpty(SELF:Arg)
        sb:AppendLine(LangString(VOErrors.ERROR_ARG)	+ SELF:Arg)
      ENDIF
      LOCAL cArgs AS STRING
      IF SELF:Args != NULL .AND. SELF:Args:Length > 0
            cArgs := "{"
            LOCAL lFirst := TRUE AS LOGIC
            FOREACH VAR oArg IN SELF:Args
                IF ! lFirst
                    cArgs += ","
                ENDIF
                IF oArg == NULL
                    cArgs += "(NULL)"
                ELSE
                    cArgs += oArg:ToString()
                ENDIF
                lFirst := FALSE
            NEXT
            cArgs += "}"
            sb:AppendLine(LangString(VOErrors.ERROR_ARGS)+ cArgs)
      ENDIF
      IF SELF:ArgTypeReqType != NULL
        sb:AppendLine(LangString(VOErrors.ERROR_ARGTYPE_REQ) + SELF:ArgTypeReqType:FullName)
      ENDIF
      IF ! String.IsNullOrEmpty(SELF:CallFuncSym)
        sb:AppendLine(LangString(VOErrors.ERROR_CALLEDFROM) + SELF:CallFuncSym  )
      ENDIF
      sb:AppendLine(LangString(VOErrors.ERROR_STACK))
      sb:AppendLine(SELF:StackTrace  )
      RETURN sb:ToString()


    /// <summary>Throw the error.</summary>
    VIRTUAL METHOD Throw AS VOID STRICT
        THROW SELF

    #region STATIC methods TO construct an error
    /// <exclude/>
    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, description AS STRING) AS Error
    RETURN ArgumentError(cFuncName, name, description, 0)

    /// <exclude/>
    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, iArgnum  AS DWORD, aArgs PARAMS OBJECT[]) AS Error
    VAR err			:= Error{XSharp.Gencode.EG_ARG, name, ErrString( EG_ARG )}
    err:FuncSym     := cFuncName
    err:Description := err:Message
    err:ArgNum		:= iArgnum
    err:Args		:= aArgs
    RETURN err


    /// <exclude/>
    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, description AS STRING, iArgnum AS DWORD) AS Error
    VAR err			:= Error{XSharp.Gencode.EG_ARG, name, description}
    err:FuncSym     := cFuncName
    err:Description := err:Message
    err:ArgNum		:= iArgnum
    RETURN err


    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, description AS STRING, iArgnum AS DWORD, aArgs AS OBJECT[]) AS Error
        VAR err := ArgumentError(cFuncName, name, description, iArgnum)
        err:Args := aArgs
    RETURN err

    /// <exclude/>
    STATIC METHOD WrapRawException( ex AS Exception ) AS Error
    LOCAL e AS Error
    if ex != null
        e			  := Error{ ex }
        e:Description := ErrString( EG_EXCEPTION ) + ": " + ex:Message
    else
        e			  := Error{  }
        e:Description := ErrString( EG_SEQUENCE )
    endif
    RETURN e

    /// <exclude/>
    STATIC METHOD VOError( dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs AS OBJECT[] ) AS Error
        LOCAL e AS Error
        e:= Error{dwGenCode,cArgName}
        e:FuncSym := cFuncName
        e:ArgNum := iArgNum
        e:Args := aArgs
        e:Description := ErrString( dwGenCode )
        RETURN e

    /// <exclude/>
    STATIC METHOD VOError( ex AS Exception, dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs AS OBJECT[]  ) AS Error
    LOCAL e AS Error
    e			  := Error{ ex, cFuncName, cArgName, iArgNum, aArgs }
    e:Gencode     := dwGenCode
    e:Description := ErrString( dwGenCode )
    RETURN e

    /// <exclude/>
    STATIC METHOD VoDbError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING ) AS Error
    LOCAL e AS Error
    e := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:FuncSym     := cFuncName
    e:Description := ErrString( dwGenCode )
    RETURN e

    /// <exclude/>
    STATIC METHOD VoDbError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:FuncSym     := cFuncName
    e:Description := ErrString( dwGenCode )
    e:Args        := aArgs
    IF aArgs:Length > 0
        e:Arg := aArgs[0]:ToString()
    ENDIF
    RETURN e


    /// <exclude/>
    STATIC METHOD VoDbError( dwGenCode AS DWORD, dwSubCode AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e			  := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:Description := __CavoStr( dwSubCode)
    e:Args        := aArgs
    IF aArgs:Length > 0
        e:Arg := aArgs[0]:ToString()
    ENDIF
    RETURN e

    /// <exclude/>
    STATIC METHOD VoDbError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e := Error{dwGenCode, dwSubCode, cFuncName, cArgName, iArgNum}
    e:SubSystem   := "DBCMD"
    IF dwSubCode != 0
        e:Description := __CavoStr( dwSubCode)
    ELSE
        e:Description := ErrString(dwGenCode)
    ENDIF
    e:Args        := aArgs
    RETURN e

    /// <exclude/>
    STATIC METHOD DataTypeError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e			  := Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
    e:Gencode     := EG_DATATYPE
    e:Description := __CavoStr( VOErrors.DATATYPEERROR )
    RETURN e

    /// <exclude/>
    STATIC METHOD ArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, cDescription AS STRING, aArgs PARAMS OBJECT[]) AS Error
    LOCAL e AS Error
    e				:= Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
    e:Gencode     := EG_ARG
    e:Description := cDescription
    RETURN e

    /// <exclude/>
    STATIC METHOD NullArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD ) AS Error
    LOCAL e AS Error
    e := Error{ ArgumentNullException{} ,cFuncName, cArgName, iArgNum}
    e:Gencode     := EG_ARG
    e:Description := __CavoStr( VOErrors.ARGISNULL )
    RETURN e

    /// <exclude/>
    STATIC METHOD BoundError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
    LOCAL e AS Error
    e := Error{ ArgumentException{ErrString( EG_BOUND) } }
    e:Severity    := ES_ERROR
    e:Gencode     := EG_BOUND
    e:SubSystem   := "BASE"
    e:FuncSym     := cFuncName
    e:Arg         := cArgName
    e:ArgNum      := iArgNum
    e:Args        := aArgs
    RETURN e


    #endregion
    /// <exclude />
    STATIC METHOD TypeToUsualType(oType AS System.Type) AS DWORD
    SWITCH Type.GetTypeCode(oType)
    CASE TypeCode.Boolean
      RETURN __UsualType.Logic
    CASE TypeCode.Byte
      RETURN __UsualType.Byte
    CASE TypeCode.Char
      RETURN __UsualType.Char
    CASE TypeCode.DateTime
      RETURN __UsualType.DateTime
    CASE TypeCode.DBNull
      RETURN __UsualType.Object
    CASE TypeCode.Decimal
      RETURN __UsualType.Decimal
    CASE TypeCode.Double
      RETURN __UsualType.Real8
    CASE TypeCode.Empty
      RETURN __UsualType.Void
    CASE TypeCode.Int16
      RETURN __UsualType.ShortInt
    CASE TypeCode.Int32
      RETURN __UsualType.Long
    CASE TypeCode.Int64
      RETURN __UsualType.Int64
    CASE TypeCode.SByte
      RETURN __UsualType.Byte
    CASE TypeCode.Single
      RETURN __UsualType.Real4
    CASE TypeCode.UInt16
      RETURN __UsualType.Word
    CASE TypeCode.UInt32
      RETURN __UsualType.DWord
    CASE TypeCode.UInt64
      RETURN __UsualType.UInt64
    OTHERWISE
        SWITCH oType:FullName:ToLower()
        CASE "xsharp.__array"
          RETURN __UsualType.Array
        CASE "xsharp.__binary"
          RETURN __UsualType.Binary
        CASE "xsharp.__codeblock"
          RETURN __UsualType.Codeblock
        CASE "xsharp.__currency"
          RETURN __UsualType.Currency
        CASE "xsharp.__date"
          RETURN __UsualType.Date
        CASE "xsharp.__float"
          RETURN __UsualType.Float
        CASE "xsharp.__psz"
          RETURN __UsualType.Psz
        CASE "xsharp.__symbol"
          RETURN __UsualType.Symbol
        CASE "xsharp.__usual"
          RETURN __UsualType.Usual
        CASE "system.intptr"
          RETURN __UsualType.Ptr

      END SWITCH
    END SWITCH
    RETURN __UsualType.Void

    /// <exclude />
    STATIC METHOD UsualTypeToType(dwType AS DWORD) AS System.Type
    LOCAL typeName := NULL AS STRING
    SWITCH dwType
    CASE __UsualType.Array
      typeName := "XSharp.__Array"
    CASE __UsualType.Byte
      RETURN typeof(System.Byte)
    CASE __UsualType.Binary
       typeName := "XSharp.__Binary"
    CASE __UsualType.Char
      RETURN typeof(System.Char)
    CASE __UsualType.Codeblock
      typeName := "XSharp.__CodeBlock"
    CASE __UsualType.Currency
      typeName := "XSharp.__Currency"
    CASE __UsualType.Date
      typeName := "XSharp.__Date"
    CASE __UsualType.DWord
      RETURN typeof(System.UInt32)
    CASE __UsualType.Int64
      RETURN typeof(System.Int64)
    CASE __UsualType.Float
      typeName := "XSharp.__Float"
    CASE __UsualType.Logic
      RETURN typeof(System.Boolean)
    CASE __UsualType.Long
      RETURN typeof(System.Int32)
    CASE __UsualType.Object
      RETURN typeof(System.Object)
    CASE __UsualType.Psz
      typeName := "XSharp.__Psz"
    CASE __UsualType.Ptr
      RETURN typeof(System.IntPtr)
    CASE __UsualType.Real4
      RETURN typeof(System.Single)
    CASE __UsualType.Real8
      RETURN typeof(System.Double)
    CASE __UsualType.ShortInt
      RETURN typeof(System.Int16)
    CASE __UsualType.String
      RETURN typeof(System.String)
    CASE __UsualType.Symbol
      typeName := "XSharp.__Symbol"
    CASE __UsualType.UInt64
      RETURN typeof(System.Int64)
    CASE __UsualType.Usual
      typeName := "XSharp.__Usual"
    CASE __UsualType.Void
      RETURN typeof(System.Void)
    CASE __UsualType.Word
      RETURN typeof(System.UInt16)
    END SWITCH
    // lookup types in XSharp.VO
    IF typeName != NULL
        FOREACH asm AS Assembly IN AppDomain.CurrentDomain:GetAssemblies()
          IF asm:GetName():Name:ToLower() == "xsharp.vo"
            VAR type := asm:GetType(typeName, FALSE, TRUE)
            IF type != NULL
              RETURN type
            ENDIF
          ENDIF
        NEXT
    ENDIF
    RETURN NULL


  STATIC METHOD GetInnerException( SELF e as Exception) AS Exception
     IF e:InnerException != NULL
        DO WHILE e:InnerException != NULL
            e := e:InnerException
            IF e IS XSharp.Error
                EXIT
            ENDIF
        ENDDO
     ENDIF
     RETURN e
END CLASS
END NAMESPACE

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorbuild/*" />
FUNCTION ErrorBuild(pErrInfo AS Exception) AS XSharp.Error
	RETURN  XSharp.Error{pErrInfo}



