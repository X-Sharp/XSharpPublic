//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp

BEGIN NAMESPACE XSharp.RDD
	STATIC CLASS RDDHelpers
		
		STATIC METHOD WAS AS WorkAreas
			RETURN WorkAreas.GetInstance()
		
		STATIC METHOD CWA(cFunction AS STRING) AS IRDD 
			LOCAL oResult AS IRDD
			oResult := CWA()
			IF oResult != NULL_OBJECT
				RETURN oResult
			ENDIF
			THROW NoTableError(cFunction)
		
		STATIC METHOD CWA AS IRDD 
			RETURN WorkAreas.GetInstance().CurrentWorkArea
		
		STATIC METHOD CWANum AS LONG
			RETURN WorkAreas.GetInstance().CurrentWorkAreaNO
		
		STATIC METHOD CWANum(cFunction AS STRING)  AS LONG
			VAR oWA := WorkAreas.GetInstance().CurrentWorkArea
			IF oWA != NULL
				RETURN oWA:Area
			ENDIF
			THROW NoTableError(cFunction)
		
		
		
		STATIC METHOD NoTableError(cFunction AS STRING) AS RddError
			LOCAL oError AS RddError
			oError := RddError{}
			oError:SubSystem := "XSharp.RDD"
			oError:Gencode  := EG_NOTABLE
			oError:SubCode  := 1050
			oError:Severity := ES_ERROR
			oError:FuncSym := cFunction
			RETURN oError
	END CLASS
END NAMESPACE
