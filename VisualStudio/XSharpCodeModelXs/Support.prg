using System
begin namespace XSharpModel
	class Support
		static method Debug(msg as string,  o :=  null as object[]) as void
			if System.Diagnostics.Debugger.IsAttached
				if o == null
					o := Array.Empty<object>() 
				endif
				System.Diagnostics.Debug.WriteLine(String.Format("XModel: "+  msg, o))
			endif
		
		
	end class
	
end namespace 

