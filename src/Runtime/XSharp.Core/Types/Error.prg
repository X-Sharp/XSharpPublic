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
  /// <include file="XSharp.CoreDocs.xml" path="doc/Error/*" />
  CLASS Error INHERIT Exception
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.SubSystem/*" />
    VIRTUAL PROPERTY SubSystem AS STRING AUTO  := "BASE"
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Gencode/*" />
    VIRTUAL PROPERTY Gencode AS DWORD AUTO    := EG_UNKNOWN
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.GenCodeText/*" />
    VIRTUAL PROPERTY GenCodeText AS STRING GET IIF (Gencode != 0, ErrString(Gencode), "Unknown GenCode")
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.SubCode/*" />
    VIRTUAL PROPERTY SubCode AS DWORD AUTO    := 0
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.SubCodeText/*" />
    VIRTUAL PROPERTY SubCodeText AS STRING GET IIF(!String.IsNullOrEmpty(_Subcode), _Subcode, IIF (SubCode != 0, __CavoStr(SubCode), "Unknown SubCode")) SET _Subcode := value
    PRIVATE _Subcode as STRING
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.FuncSym/*" />
    VIRTUAL PROPERTY FuncSym AS STRING AUTO   := ""
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.FileName/*" />
    VIRTUAL PROPERTY FileName AS STRING AUTO  := ""
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Severity/*" />
    VIRTUAL PROPERTY Severity AS DWORD AUTO   := ES_ERROR
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Description/*" />
    VIRTUAL PROPERTY Description		AS STRING AUTO := ""
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Arg/*" />
    VIRTUAL PROPERTY Arg				AS STRING AUTO := ""
    PRIVATE  _ArgType			AS DWORD
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ArgType/*" />
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
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ArgTypeType/*" />
    VIRTUAL PROPERTY ArgTypeType		AS System.Type
      GET
        RETURN _ArgTypeType
      END GET
      SET
        _ArgTypeType := value
        _ArgType 	 := (DWORD) TypeToUsualType(value)
      END SET
    END PROPERTY

    PRIVATE _ArgTypeReq		AS DWORD
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ArgTypeReq/*" />
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
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ArgTypeReqType/*" />
    VIRTUAL PROPERTY ArgTypeReqType		AS System.Type
      GET
        RETURN _ArgTypeReqType
      END GET
      SET
        _ArgTypeReqType := value
        _ArgTypeReq 	 := (DWORD) TypeToUsualType(value)
      END SET
    END PROPERTY
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.SubstituteType/*" />
    VIRTUAL PROPERTY SubstituteType     AS DWORD AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ArgNum/*" />
    VIRTUAL PROPERTY ArgNum				AS DWORD AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.MethodSelf/*" />
    VIRTUAL PROPERTY MethodSelf			AS OBJECT AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.CallFuncSym/*" />
    VIRTUAL PROPERTY CallFuncSym		AS STRING AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Args/*" />
    VIRTUAL PROPERTY Args				AS OBJECT[] AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Tries/*" />
    VIRTUAL PROPERTY Tries				AS INT AUTO := 0
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.CanDefault/*" />
    VIRTUAL PROPERTY CanDefault         AS LOGIC AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.CanRetry/*" />
    VIRTUAL PROPERTY CanRetry           AS LOGIC AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.CanSubstitute/*" />
    VIRTUAL PROPERTY CanSubstitute      AS LOGIC AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Operation/*" />
    VIRTUAL PROPERTY Operation          AS STRING AUTO := ""
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.OSCode/*" />
    VIRTUAL PROPERTY OSCode				AS DWORD AUTO := 0
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.OSCodeText/*" />
    VIRTUAL PROPERTY OSCodeText			AS STRING GET IIF(OSCode == 0, "", DosErrString(OSCode))
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.FileHandle/*" />
    VIRTUAL PROPERTY FileHandle         AS DWORD AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.MaxSize/*" />
    VIRTUAL PROPERTY MaxSize			AS DWORD AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.FuncPtr/*" />
    VIRTUAL PROPERTY FuncPtr            AS IntPtr AUTO := IntPtr.Zero


    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Cargo/*" />
    VIRTUAL PROPERTY Cargo              AS OBJECT AUTO
    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Stack/*" />
    VIRTUAL PROPERTY Stack              AS STRING GET SELF:StackTrace SET SELF:SetStackTrace(VALUE)

	PRIVATE _StackTrace AS STRING
    OVERRIDE PROPERTY StackTrace         AS STRING
    	GET
    		IF String.IsNullOrEmpty(SELF:_StackTrace)
    			RETURN SUPER:StackTrace
    		END IF
    		RETURN SELF:_StackTrace
    	END GET
    END PROPERTY
    METHOD SetStackTrace(cTrace AS STRING) AS VOID
 		SELF:_StackTrace := cTrace


    PRIVATE METHOD setDefaultValues() AS VOID
        SELF:FuncSym := ProcName(2)
        SELF:Args    := <OBJECT>{}
        IF String.IsNullOrEmpty(SELF:StackTrace)
            SELF:_StackTrace  := ErrorStack(2)
        ENDIF
        RETURN

    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor/*" />
    CONSTRUCTOR()
    SELF:setDefaultValues()
    RETURN

    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor_2/*" />
    CONSTRUCTOR (msg AS STRING)
    SUPER(msg)
    SELF:setDefaultValues()
    SELF:Description := msg
    SELF:Gencode     := EG_EXCEPTION
    RETURN

    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor_3/*" />
    CONSTRUCTOR (ex AS Exception)
    SUPER(ex:Message,ex)
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
        SELF:GetGenCodeFromException(ex)
        IF ! String.IsNullOrEmpty(ex:Source)
            SELF:SubSystem := ex:Source
        ENDIF
        VAR sStack       := ErrorStack( StackTrace{ex,TRUE},UInt32.MaxValue)
        IF !sStack:StartsWith(EMPTY_ERRORSTACK)
            SELF:_StackTrace := sStack + SELF:_StackTrace
        ENDIF
    ENDIF

    IF String.IsNullOrEmpty(SELF:StackTrace)
        SELF:SetStackTrace(ErrorStack(2))
    ENDIF
    PRIVATE METHOD GetGenCodeFromException(ex as Exception) AS VOID
        if ex IS DivideByZeroException
            SELF:Gencode := EG_ZERODIV
        ELSEIF ex IS OverflowException
            SELF:Gencode := EG_NUMOVERFLOW
        ELSEIF ex IS OutOfMemoryException
            SELF:Gencode := EG_MEM
        ELSEIF ex IS RankException
            SELF:Gencode := EG_BOUND
        ELSEIF ex IS IndexOutOfRangeException
            SELF:Gencode := EG_BOUND
        ELSEIF ex IS ArgumentException
            SELF:Gencode := EG_ARG
        ELSEIF ex IS InvalidCastException
            SELF:Gencode := EG_WRONGCLASS
        ELSEIF ex IS NullReferenceException
            SELF:Gencode := EG_NULLVAR
        ELSEIF ex IS StackOverflowException
            SELF:Gencode := EG_STACK
        ELSEIF ex IS AccessViolationException
            SELF:Gencode := EG_CORRUPTION
        ELSE
            SELF:Gencode     := EG_EXCEPTION
        ENDIF

    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor_4/*" />
    CONSTRUCTOR (ex AS Exception, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[])
    SUPER(ex:Message,ex)
    SELF:setDefaultValues()
    SELF:GetGenCodeFromException(ex)
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    SELF:Args		 := aArgs


    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor_5/*" />
    CONSTRUCTOR (dwGenCode AS DWORD, cArg AS STRING)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwGenCode
    SELF:Arg	 := cArg
    SELF:Description := ErrString( dwGenCode )

    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor_6/*" />
    CONSTRUCTOR (dwGenCode AS DWORD, cArg AS STRING, cDescription AS STRING)
    SUPER(cDescription)
    SELF:setDefaultValues()
    SELF:Gencode		:= dwGenCode
    SELF:Arg			:= cArg
    SELF:Description	:= cDescription


    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor_7/*" />
    CONSTRUCTOR (dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwGenCode
    SELF:SubCode := dwSubCode
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    SELF:Description := ErrString( dwGenCode )

    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ctor_8/*" />
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

    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.ToString/*" />
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


    /// <include file="XSharp.CoreDocs.xml" path="doc/Error.Throw/*" />
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
    return e


    #endregion
    /// <exclude />
    static method TypeToUsualType(oType as System.Type) as dword
        Return (DWORD) SystemTypeToUsualType(oType)

    /// <exclude />
    static method UsualTypeToType(dwType as dword) as System.Type
    local typeName := null as string
    switch dwType
    case __UsualType.Array
      typeName := "XSharp.__Array"
    case __UsualType.Byte
      return typeof(System.Byte)
    case __UsualType.Binary
       typeName := "XSharp.__Binary"
    case __UsualType.Char
      return typeof(System.Char)
    case __UsualType.Codeblock
      typeName := "XSharp.__CodeBlock"
    case __UsualType.Currency
      typeName := "XSharp.__Currency"
    case __UsualType.Date
      typeName := "XSharp.__Date"
    case __UsualType.DWord
      return typeof(System.UInt32)
    case __UsualType.Int64
      return typeof(System.Int64)
    case __UsualType.Float
      typeName := "XSharp.__Float"
    case __UsualType.Logic
      return typeof(System.Boolean)
    case __UsualType.Long
      return typeof(System.Int32)
    case __UsualType.Object
      return typeof(System.Object)
    case __UsualType.Psz
      typeName := "XSharp.__Psz"
    case __UsualType.Ptr
      return typeof(System.IntPtr)
    case __UsualType.Real4
      return typeof(System.Single)
    case __UsualType.Real8
      return typeof(System.Double)
    case __UsualType.ShortInt
      return typeof(System.Int16)
    case __UsualType.String
      return typeof(System.String)
    case __UsualType.Symbol
      typeName := "XSharp.__Symbol"
    case __UsualType.UInt64
      return typeof(System.Int64)
    case __UsualType.Usual
      typeName := "XSharp.__Usual"
    case __UsualType.Void
      return typeof(System.Void)
    case __UsualType.Word
      return typeof(System.UInt16)
    case __UsualType.Null
      return typeof(System.DBNull)
    end switch
    // lookup types in XSharp.VO
    if typeName != null
        foreach asm as Assembly in AppDomain.CurrentDomain:GetAssemblies()
          if asm:GetName():Name:ToLower() == "xsharp.vo"
            var type := asm:GetType(typeName, false, true)
            if type != null
              return type
            endif
          endif
        next
    endif
    return null


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



