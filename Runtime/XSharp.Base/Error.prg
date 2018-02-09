
USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp
	/// <Summary>XSharp Runtime base Error class</Summary>
	CLASS Error INHERIT Exception
		/// <Summary>A string representing the name of the subsystem generating the error.</Summary>
        PROPERTY SubSystem as STRING AUTO
		/// <Summary>An integer numeric value representing a Visual Objects generic error code.</Summary>
		/// <Seealso cref="T:XSharp.Gencode"/>
        PROPERTY Gencode as INT AUTO
		/// <Summary>An integer numeric value representing a subsystem-specific error code.</Summary>
        PROPERTY SubCode as INT AUTO
		/// <Summary>A string representing the name of the function or method in which the error occurred.</Summary>
        PROPERTY FuncSym as STRING AUTO
		/// <Summary>A string representing the name used to open the file associated with the error condition.</Summary>
        PROPERTY FileName as STRING AUTO
		/// <Summary>A constant indicating the severity of the error condition.</Summary>
		/// <Seealso cref="T:XSharp.Severity"/>
        PROPERTY Severity as INT AUTO
		/// <Summary>A string that describes the error condition.</Summary>
        PROPERTY Description AS STRING AUTO
		PROPERTY Arg AS STRING AUTO
		PROPERTY ArgType AS STRING AUTO
		PROPERTY ArgTypeReq AS System.Type AUTO
		PROPERTY ArgNum AS LONG AUTO
		
	PRIVATE METHOD setDefaultValues() as VOID
		SELF:Gencode := 0
		SELF:Subcode := 0
		SELF:Subsystem := "BASE"
		SELF:Severity    := Severity.Error
		//TODO 
		//SELF:FuncSym   := 

    CONSTRUCTOR()
         RETURN

	CONSTRUCTOR (ex AS Exception)
		SUPER(ex.Message,ex)
		SELF:setDefaultValues()
		SELF:Description := ex:Message

	CONSTRUCTOR (igencode AS INT)
		SELF:setDefaultValues()
		SELF:Gencode := iGenCode

	CONSTRUCTOR (igencode AS Gencode)
		SELF:setDefaultValues()
		SELF:Gencode := iGenCode

	CONSTRUCTOR (igencode AS Gencode, iSubCode as INT)
		SELF:setDefaultValues()
		SELF:Gencode := igencode
		SELF:SubCode := iSubcode

	CONSTRUCTOR (igencode AS INT, iSubCode as INT)
		SELF:setDefaultValues()
		SELF:Gencode := igencode
		SELF:SubCode := iSubcode

	STATIC METHOD ArgumentError(name AS STRING, description AS STRING) AS Error
		VAR err := Error{Gencode.ARG}
		err:Arg  := name
		err:Description := Description
		return err

	STATIC METHOD NullArgumentError( cFuncName AS STRING, cArgName AS STRING, iArgNum AS INT ) AS Error
		  LOCAL e AS Error
		  e := Error{ ArgumentNullException{} }
		  e:Severity    := ES_ERROR
		  e:GenCode     := EG_ARG
		  e:SubSystem   := "BASE"
		  e:FuncSym     := cFuncName
		  e:Arg         := cArgName
		  e:ArgNum      := iArgNum
		  //e:Description := SR.GetString( SR.ArgIsNULL )
		  RETURN e

	END CLASS
END NAMESPACE // XSharp.Rdd