
USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp
	/// <Summary>XSharp Runtime base Error class</Summary>
	CLASS XError INHERIT Exception
		/// <Summary>A string representing the name of the subsystem generating the error.</Summary>
        PROPERTY SubSystem as STRING AUTO
		/// <Summary>An integer numeric value representing a Visual Objects generic error code.</Summary>
		/// <Seealso cref="T:XSharp.Gencode"/>
        PROPERTY Gencode as DWORD AUTO
		/// <Summary>An integer numeric value representing a subsystem-specific error code.</Summary>
        PROPERTY SubCode as DWORD AUTO
		/// <Summary>A string representing the name of the function or method in which the error occurred.</Summary>
        PROPERTY FuncSym as STRING AUTO
		/// <Summary>A string representing the name used to open the file associated with the error condition.</Summary>
        PROPERTY FileName as STRING AUTO
		/// <Summary>A constant indicating the severity of the error condition.</Summary>
		/// <Seealso cref="T:XSharp.Severity"/>
        PROPERTY Severity as DWORD AUTO
		/// <Summary>A string that describes the error condition.</Summary>
        PROPERTY Description as STRING AUTO
    CONSTRUCTOR()
         RETURN

	END CLASS
END NAMESPACE // XSharp.Rdd