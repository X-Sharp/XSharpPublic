
using System
using System.Collections.Generic
using System.Text

begin namespace XSharp
	/// <Summary>XSharp Runtime base Error class</Summary>
	class Error inherit Exception
		/// <Summary>A string representing the name of the subsystem generating the error.</Summary>
		property SubSystem as string auto
		/// <Summary>An integer numeric value representing a Visual Objects generic error code.</Summary>
		/// <Seealso cref="T:XSharp.Gencode"/>
		property Gencode as dword auto
		/// <Summary>An integer numeric value representing a subsystem-specific error code.</Summary>
		property SubCode as dword auto
		/// <Summary>A string representing the name of the function or method in which the error occurred.</Summary>
		property FuncSym as string auto
		/// <Summary>A string representing the name used to open the file associated with the error condition.</Summary>
		property FileName as string auto
		/// <Summary>A constant indicating the severity of the error condition.</Summary>
		/// <Seealso cref="T:XSharp.Severity"/>
		property Severity as dword auto
		/// <Summary>A string that describes the error condition.</Summary>
		property Description as string auto
		property Arg as string auto
		property ArgType as string auto
		property ArgTypeReq as System.Type auto
		property ArgNum as long auto
		
		private method setDefaultValues() as void
			self:Gencode := 0
			self:Subcode := 0
			self:Subsystem := "BASE"
			self:Severity    := Severity.ES_Error
		//TODO 
		//SELF:FuncSym   := 
		
		constructor()
			return
		
		constructor (ex as Exception)
			super(ex.Message,ex)
			self:setDefaultValues()
			self:Description := ex:Message
		
		constructor (dwgencode as dword)
			self:setDefaultValues()
			self:Gencode := dwGenCode
		
		constructor (eGencode as Gencode)
			self:setDefaultValues()
			self:Gencode := eGenCode
		
		constructor (eGencode as Gencode, dwSubCode as dword)
			self:setDefaultValues()
			self:Gencode := eGencode
			self:SubCode := dwSubcode
		
		constructor (dwgencode as dword, dwSubCode as dword)
			self:setDefaultValues()
			self:Gencode := dwgencode
			self:SubCode := dwSubcode
		
		static method ArgumentError(name as string, description as string) as Error
			var err := Error{Gencode.EG_ARG}
			err:Arg  := name
			err:Description := Description
			return err
		
		static method NullArgumentError( cFuncName as string, cArgName as string, iArgNum as int ) as Error
			local e as Error
			e := Error{ ArgumentNullException{} }
			e:Severity    := ES_ERROR
			e:GenCode     := EG_ARG
			e:SubSystem   := "BASE"
			e:FuncSym     := cFuncName
			e:Arg         := cArgName
			e:ArgNum      := iArgNum
			//e:Description := SR.GetString( SR.ArgIsNULL )
			return e
		
	end class
end namespace 