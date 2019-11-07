
USING System
USING System.Collections.Generic
USING System.Text
USING System.Reflection
BEGIN NAMESPACE XSharp
  /// <summary>XSharp Runtime base Error class</summary>
  CLASS Error INHERIT Exception
    /// <summary>A string representing the name of the subsystem generating the error.</summary>
    VIRTUAL PROPERTY SubSystem AS STRING AUTO  := "BASE"
    /// <summary>An integer numeric value representing a Visual Objects generic error code.</summary>
    /// <Seealso cref="T:XSharp.Gencode"/>
    VIRTUAL PROPERTY Gencode AS DWORD AUTO    := EG_UNKNOWN
    /// <summary>An string containing the description of the Gencode.</summary>
    VIRTUAL PROPERTY GenCodeText AS STRING GET IIF (Gencode != 0, ErrString(Gencode), "Unknown GenCode")
    /// <summary>An integer numeric value representing a subsystem-specific error code.</summary>
    VIRTUAL PROPERTY SubCode AS DWORD AUTO    := 0
    /// <summary>An string containing the description of the SubCode.</summary>
    VIRTUAL PROPERTY SubCodeText AS STRING GET IIF (SubCode != 0, __CavoStr(SubCode), "Unknown SubCode")
    /// <summary>A string representing the name of the function or method in which the error occurred.</summary>
    VIRTUAL PROPERTY FuncSym AS STRING AUTO   := ""
    /// <summary>A string representing the name used to open the file associated with the error condition.</summary>
    VIRTUAL PROPERTY FileName AS STRING AUTO  := ""
    /// <summary>A constant indicating the severity of the error condition.</summary>
    /// <Seealso cref="T:XSharp.Severity"/>
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
        _ArgType := VALUE
        _ArgTypeType := UsualTypeToType(VALUE)
      END SET
    END PROPERTY
    PRIVATE  _ArgTypeType 		:= NULL AS System.Type 
    /// <summary>The system type representing the data type of the argument that raised the error.</summary>
    VIRTUAL PROPERTY ArgTypeType		AS System.Type 
      GET 
        RETURN _ArgTypeType 
      END GET
      SET 
        _ArgTypeType := VALUE
        _ArgType 	 := TypeToUsualType(VALUE)
      END SET
    END PROPERTY
    
    PRIVATE _ArgTypeReq		AS DWORD 
    /// <summary>A numeric value representing the expected type of the argument that raised the error.</summary>
    VIRTUAL PROPERTY ArgTypeReq			AS DWORD 
      GET 
        RETURN _ArgTypeReq 
      END GET
      SET 
        _ArgTypeReq := VALUE
        _ArgTypeReqType := UsualTypeToType(VALUE)
      END SET
    END PROPERTY
    PRIVATE  _ArgTypeReqType := NULL	AS System.Type 
    /// <summary>The system type representing the expected type of the argument that raised the error.</summary>
    VIRTUAL PROPERTY ArgTypeReqType		AS System.Type 
      GET 
        RETURN _ArgTypeReqType 
      END GET
      SET 
        _ArgTypeReqType := VALUE
        _ArgTypeReq 	 := TypeToUsualType(VALUE)
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
    /// <summary>A value of 0 indicates that the error condition was not caused by an error from the operating system.  When Error:OsCode is set to a value other </summary>
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
    
	PRIVATE _StackTrace AS STRING
    VIRTUAL PROPERTY StackTrace         AS STRING
    	GET
    		IF String.IsNullOrEmpty(SELF:_StackTrace)
    			RETURN SUPER:StackTrace
    		END IF
    		RETURN SELF:_StackTrace
    	END GET
    	SET
    		SELF:_StackTrace := VALUE
    	END SET
    END PROPERTY


    PRIVATE METHOD setDefaultValues() AS VOID
        SELF:FuncSym := ProcName(2)
        SELF:Args    := <OBJECT>{}
        IF String.IsNullOrEmpty(SELF:StackTrace)
            SELF:_StackTrace  := System.Diagnostics.StackTrace{2,TRUE}:ToString()
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
    SELF:GenCode     := EG_EXCEPTION
    RETURN 

    /// <summary>Create an Error Object with the Innner Exception</summary>
    CONSTRUCTOR (ex AS Exception)
    SUPER(ex.Message,ex)
    SELF:setDefaultValues()
    IF ex IS Error
        // Clone Error properties from Inner Exception
        LOCAL e := (Error) ex AS Error
        VAR props := typeof(error):GetProperties()
        FOREACH oProp AS PropertyInfo IN props
            IF oProp:CanWrite
                oProp:SetValue(SELF, oProp:GetValue(e))
            ENDIF
        NEXT
    ELSE
        SELF:Description := ex:Message
        SELF:GenCode     := EG_EXCEPTION
        SELF:_StackTrace := ex:StackTrace
    ENDIF
    IF String.IsNullOrEmpty(SELF:StackTrace)
        SELF:StackTrace  := System.Diagnostics.StackTrace{2,TRUE}:ToString()
    ENDIF

    /// <summary>Create an Error Object with the Innner Exception and other parameters</summary>
    CONSTRUCTOR (ex AS Exception, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[])
    SUPER(ex.Message,ex)
    SELF:setDefaultValues()
    SELF:GenCode     := EG_EXCEPTION
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    SELF:Args		 := aArgs
    
    
    /// <summary>Create an Error Object for a Gencode and Argument Name.</summary>
    CONSTRUCTOR (dwgencode AS DWORD, cArg AS STRING)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwGenCode
    SELF:Arg	 := cArg

    /// <summary>Create an Error Object for a Gencode, Argument Name and Description.</summary>
    CONSTRUCTOR (dwgencode AS DWORD, cArg AS STRING, cDescription AS STRING)
    SUPER(cDescription)
    SELF:setDefaultValues()
    SELF:Gencode		:= dwGenCode
    SELF:Arg			:= cArg
    SELF:Description	:= cDescription
    
    
    /// <summary>Create an Error Object.</summary>
    CONSTRUCTOR (dwgencode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD)
    SUPER(ErrString( dwGenCode ))
    SELF:setDefaultValues()
    SELF:Gencode := dwgencode
    SELF:SubCode := dwSubcode
    SELF:FuncSym     := cFuncName
    SELF:Arg         := cArgName
    SELF:ArgNum      := iArgNum
    
    /// <summary>Create an Error Object.</summary>
    CONSTRUCTOR (dwgencode AS DWORD, dwSubCode := 0 AS DWORD)
    SELF:setDefaultValues()
    SELF:Gencode := dwgencode
    SELF:SubCode := dwSubcode
    

    PRIVATE METHOD LangString(e as VOErrors) AS STRING
        local cString := __CavoStr(e):Trim() as string
        if cString:Endswith(":")
            cString := cString:Substring(0, cString:Length-1):Trim()
        endif
        return cString+e" :\t"

    /// <inheritdoc />
    OVERRIDE METHOD ToString() AS STRING
      LOCAL sb AS StringBuilder
      LOCAL nGenCode AS GenCode
      nGenCode := (GenCode) SELF:GenCode
      sb := StringBuilder{}
      
      sb:AppendLine( LangString(VOErrors.ERROR_DESCRIPTION) + SELF:Description)
      sb:AppendLine( LangString(VOErrors.ERROR_SUBSYSTEM) + SELF:SubSystem )
      sb:AppendLine( LangString(VOErrors.ERROR_GENCODE) + nGenCode:ToString()  +" " +SELF:GenCodeText  )
      IF SELF:SubCode != 0
        sb:AppendLine( LangString(VOErrors.ERROR_SUBCODE) + SELF:SubCode:ToString() +" "+SELF:SubCodeText)
      ENDIF
      IF SELF:OsCode != 0
        sb:AppendLine( LangString(VOErrors.ERROR_OSCODE)  + SELF:OsCode:ToString() +" " +SELF:OsCodeText )
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
    err:Argnum		:= iArgNum
    err:args		:= aArgs
    RETURN err
    
    
    /// <exclude/>	
    STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, description AS STRING, iArgnum AS DWORD) AS Error
    VAR err			:= Error{XSharp.Gencode.EG_ARG, name, description}
    err:FuncSym     := cFuncName
    err:Description := err:Message
    err:Argnum		:= iArgNum
    RETURN err
    
    /// <exclude/>	
    STATIC METHOD WrapRawException( ex AS Exception ) AS Error
    LOCAL e AS Error
    if ex != null
        e			  := Error{ ex }
        e:Description := ErrString( EG_EXCEPTION ) + ":" + ex:Message
    else
        e			  := Error{  }
        e:Description := ErrString( EG_SEQUENCE ) + ":" + ex:Message
    endif
    RETURN e
    
    /// <exclude/>	
    STATIC METHOD VOError( dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs AS OBJECT[] ) AS Error
        LOCAL e AS Error
        e:= Error{dwGencode,cArgName}
        e:FuncSym := cFuncName
        e:ArgNum := iArgNum
        e:Args := aArgs
        RETURN e 
    
    /// <exclude/>	
    STATIC METHOD VOError( ex AS Exception, dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs AS OBJECT[]  ) AS Error
    LOCAL e AS Error
    e			  := Error{ ex, cFuncName, cArgName, iArgNum, aArgs }
    e:GenCode     := dwGenCode
    e:Description := ErrString( dwGenCode )
    RETURN e
    
    /// <exclude/>	
    STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING ) AS Error
    LOCAL e AS Error
    e := Error{dwGenCode, dwSubCode}
    e:SubSystem   := "DBCMD"
    e:FuncSym     := cFuncName
    e:Description := ErrString( dwGenCode )
    RETURN e

    /// <exclude/>	
    STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, aArgs PARAMS OBJECT[] ) AS Error
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
    STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
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
    STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs PARAMS OBJECT[] ) AS Error
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
    e:GenCode     := EG_DATATYPE
    e:Description := __CavoStr( VOErrors.DATATYPEERROR )
    RETURN e
    
    /// <exclude/>	
    STATIC METHOD ArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, cDescription AS STRING, aArgs PARAMS OBJECT[]) AS Error
    LOCAL e AS Error
    e				:= Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
    e:GenCode     := EG_ARG
    e:Description := cDescription
    RETURN e
    
    /// <exclude/>	
    STATIC METHOD NullArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD ) AS Error
    LOCAL e AS Error
    e := Error{ ArgumentNullException{} ,cFuncName, cArgName, iArgNum}
    e:GenCode     := EG_ARG
    e:Description := __CavoStr( VOErrors.ARGISNULL )
    RETURN e
    
    /// <exclude/>	
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
    /// <exclude />
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
        CASE "xsharp.__date"
          RETURN __UsualType.date
        CASE "xsharp.__float"
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
    
    /// <exclude />
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
      typeName := "XSharp.__Date"
    CASE __UsualType.dword
      RETURN typeof(System.UInt32)
    CASE __UsualType.int64
      RETURN typeof(System.Int64)
    CASE __UsualType.float
      typeName := "XSharp.__Float"
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

  /// <summary>This enum is used to describe the type of USUAL values in the X# Runtime. It is based on the original USUAL type values in the VO runtime.</summary>
  ENUM __UsualType AS BYTE
        // These numbers must match with the types defined in the compiler
        // They also match with the USUAL types in VO (BaseType.h)
        /// <summary>The usual contains a NIL.</summary>
        MEMBER Void		:=0
        /// <summary>The usual contains a LONG value</summary>
        MEMBER Long		:=1
        /// <summary>The usual contains a DATE value</summary>
        MEMBER Date		:=2
        /// <summary>The usual contains a FLOAT value</summary>
        MEMBER Float	:=3
        /// <summary>This value is NEVER used for USUALs (this was also defined in VO but never used).</summary>
        MEMBER Fixed    := 4 
        /// <summary>The usual contains an ARRAY value</summary>
        MEMBER Array	:=5
        /// <summary>The usual contains an OBJECT value</summary>
        MEMBER Object	:=6
        /// <summary>The usual contains an STRING value</summary>
        MEMBER String	:=7
        /// <summary>The usual contains an LOGIC value</summary>
        MEMBER Logic	:=8
        /// <summary>The usual contains an CODEBLOCK value</summary>
        MEMBER Codeblock:=9
        /// <summary>The usual contains an SYMBOL value</summary>
        MEMBER Symbol	:=10
        // see below for missing values
        // The follow numbers are defined but never stored inside a USUAL in VO and Vulcan
        /// <summary>This value is in the enum for completeness but never used inside a usual. Byte values are stored as LONG.</summary>
        MEMBER Byte		:=11
        /// <summary>This value is in the enum for completeness but never used inside a usual. Short values are stored as LONG.</summary>
        MEMBER ShortInt	:=12
        /// <summary>This value is in the enum for completeness but never used inside a usual. Word values are stored as LONG.</summary>
        MEMBER Word		:=13
        /// <summary>This value is in the enum for completeness but never used inside a usual. DWord values are stored as LONG or FLOAT.</summary>
        MEMBER DWord	:=14
        /// <summary>This value is in the enum for completeness but never used inside a usual. Real4 values are stored as FLOAT</summary>
        MEMBER Real4	:=15
        /// <summary>This value is in the enum for completeness but never used inside a usual. Real8 values are stored as FLOAT.</summary>
        MEMBER Real8	:=16
        /// <summary>This value is in the enum for completeness but never used inside a usual.</summary>
        /// <summary>The usual contains an PSZ value</summary>
        MEMBER Psz		:=17
        /// <summary>The usual contains an PTR value</summary>
        MEMBER Ptr		:=18
        /// <exclude/>	
        MEMBER Usual	:=19	// USUAL by Ref, not implemented in Vulcan

        // 20 and 21 not used

        /// <summary>The usual contains an INT64 value (new in Vulcan and X#).</summary>
        MEMBER Int64		:=22
        /// <summary>The usual contains an UINT64 value (new in Vulcan and X#).</summary>
        MEMBER Uint64     :=23
        /// <summary>This value is in the enum for completeness but never used inside a usual. Char values are stored as LONG</summary>
        MEMBER Char		:=24    // not stored in a usual
        /// <summary>This value is in the enum for completeness but never used inside a usual. Dynamic values are stored as OBJECT</summary>
        MEMBER Dynamic    :=25
        /// <summary>The usual contains an DateTime value (new in X#).</summary>
        MEMBER DateTime	:=26
        /// <summary>The usual contains an Decimal value (new in X#).</summary>
        MEMBER Decimal	:=27
        /// <summary>The usual contains an Memo value. This value is there for compatibility with VO but never used.</summary>
        MEMBER Memo		:=32	// Used in RDD system in VO
        /// <summary>Invalid Usual Type.</summary>
        MEMBER Invalid    :=99
    END ENUM

END NAMESPACE




/// <exclude/>
FUNCTION ErrorBuild(pErrInfo AS exception) AS XSharp.ERROR
	RETURN  XSharp.Error{pErrInfo}



