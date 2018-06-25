//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp

begin namespace XSharp.RDD
	static class RDDHelpers
		
		static method WAS as WorkAreas
			return WorkAreas.GetInstance()
		
		static method CWA(cFunction as string) as IRDD 
			local oResult as IRDD
			oResult := CWA()
			if oResult != null_object
				return oResult
			endif
			throw NoTableError(cFunction)
		
		static method CWA as IRDD 
			return WorkAreas.GetInstance().CurrentWorkArea
		
		static method CWANum as long
			return WorkAreas.GetInstance().CurrentWorkAreaNO
		
		static method CWANum(cFunction as string)  as long
			var oWA := WorkAreas.GetInstance().CurrentWorkArea
			if oWA != null
				return oWA:Area
			endif
			throw NoTableError(cFunction)
		
		
		
		static method NoTableError(cFunction as string) as RddError
			local oError as RddError
			oError := RddError{}
			oError:SubSystem := "XSharp.RDD"
			oError:Gencode  := EG_NOTABLE
			oError:SubCode  := 1050
			oError:Severity := ES_ERROR
			oError:FuncSym := cFunction
			return oError
	end class
end namespace