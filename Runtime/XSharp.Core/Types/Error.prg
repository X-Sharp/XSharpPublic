
USING System
USING System.Collections.Generic
USING System.Text

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
		PROPERTY ArgType			AS DWORD AUTO
		PROPERTY ArgTypeReq			AS System.Type AUTO
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
		PROPERTY OSCode				as DWORD AUTO
		
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

		CONSTRUCTOR (ex AS Exception, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs params OBJECT[])
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
			sb:AppendLine("ArgTypeReq      :" + SELF:ArgTypeReq:Name )
			sb:AppendLine("CallFuncSym     :" + SELF:CallFuncSym  )
			return sb:ToString()

			
		METHOD @@Throw AS VOID STRICT
			// must override in subclass
			RETURN		
			
			#region Static methods to construct an error			
			STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, description AS STRING) AS Error
				RETURN ArgumentError(cFuncName, name, description, 0)

			STATIC METHOD ArgumentError(cFuncName AS STRING, name AS STRING, iArgnum  AS DWORD, aArgs params OBJECT[]) AS Error
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
				
			STATIC METHOD VOError( dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs as OBJECT[] ) AS Error
				RETURN VOError( NULL , dwGenCode, cFuncName, cArgName, iArgNum, aArgs )
				
			STATIC METHOD VOError( ex AS Exception, dwGenCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs as OBJECT[]  ) AS Error
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
				
			STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, aArgs Params object[] ) AS Error
				LOCAL e AS Error
				e			  := Error{dwGenCode, dwSubCode}
				e:SubSystem   := "DBCMD"
				e:Description := ErrString( dwGenCode )
				e:Args        := aArgs
				RETURN e
				
			STATIC METHOD VODBError( dwGenCode AS DWORD, dwSubCode AS DWORD, cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs params object[] ) AS Error
				LOCAL e AS Error
				e := Error{dwGenCode, dwSubCode, cFuncName, cArgName, iArgNum}
				e:SubSystem   := "DBCMD"
				e:Description := ErrString( dwGenCode )
				e:Args        := aArgs
				RETURN e	
				
		   STATIC METHOD DataTypeError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, aArgs params Object[] ) AS Error
			  LOCAL e AS Error
			  e				:= Error{ ArgumentException{} , cFuncName, cArgName, iArgNum, aArgs}
			  e:GenCode     := EG_DATATYPE
			  e:Description := __CavoStr( VOErrors.DATATYPEERROR )
			  RETURN e

		  STATIC METHOD ArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS DWORD, cDescription AS STRING, aArgs params OBJECT[]) AS Error
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
	END CLASS
END NAMESPACE 




