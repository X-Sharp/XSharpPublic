
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
		
    CONSTRUCTOR()
         RETURN

	CONSTRUCTOR (igencode AS INT)
		SELF:Gencode := iGenCode

	CONSTRUCTOR (igencode AS Gencode)
		SELF:Gencode := iGenCode

	CONSTRUCTOR (igencode AS Gencode, iSubCode as INT)
		SELF:Gencode := igencode
		SELF:SubCode := iSubcode

	CONSTRUCTOR (igencode AS INT, iSubCode as INT)
		SELF:Gencode := igencode
		SELF:SubCode := iSubcode
	STATIC METHOD ArgumentError(name AS STRING, description AS STRING) as Error
		VAR err := Error{Gencode.ARG}
		err:Arg  := name
		err:Description := Description
		return err
	END CLASS
END NAMESPACE // XSharp.Rdd