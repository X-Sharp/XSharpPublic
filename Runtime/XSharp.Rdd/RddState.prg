USING System.Collections.Generic
USING System.Threading
USING XSharp.Runtime
USING System

BEGIN NAMESPACE XSharp.RDD
CLASS RDDState	IMPLEMENTS IDisposable
	// Static Fields
	PRIVATE STATIC mainState  AS RDDState
	PRIVATE STATIC currentState := ThreadLocal<RDDState>{ {=>  mainState:Clone()} }  AS ThreadLocal<RDDState> 

	// Normal Fields
	PRIVATE oWorkareas	AS WorkAreas
	PRIVATE oThread		AS Thread
	PRIVATE oLastError	AS Exception
	PRIVATE oSettings	AS Dictionary<INT, OBJECT>

	
	// Static Methods and Constructor
	STATIC CONSTRUCTOR()
		mainState       := RDDState{}
	
	PUBLIC STATIC METHOD GetInstance() AS RDDState
		RETURN currentState:Value

	// Private methods
	PRIVATE CONSTRUCTOR()       
		oThread		:= Thread.CurrentThread
		SELF:Name	:= "RDDState for Thread "+oThread:ManagedThreadId:ToString()
		oSettings	:= Dictionary<INT, OBJECT>{}
		oWorkareas  := WorkAreas{}
		RETURN

	DESTRUCTOR()
		// Close all
		oWorkAreas:CloseAll()
		oSettings:Clear()
		RETURN

	PRIVATE METHOD Clone() AS RDDState
		LOCAL oNew AS RDDState
		oNew := RDDState{}		
		BEGIN LOCK oSettings
			// Copy all values from Current State to New state
			FOREACH VAR element IN oSettings     
				oNew:oSettings:Add(element:Key, element:Value)
			NEXT
		END LOCK
		RETURN oNew     
		
	// Public Fields and Methods		
	PUBLIC PROPERTY Name			 AS STRING AUTO
	PUBLIC VIRTUAL METHOD ToString() AS STRING
		RETURN SELF:Name
	
	PUBLIC METHOD GetValue<T> (nSetting AS INT) AS T
		BEGIN LOCK oSettings
			IF oSettings.ContainsKey(nSetting)
				RETURN (T) oSettings[nSetting]
			ENDIF
		END LOCK
		RETURN Default(T)

	PUBLIC METHOD SetValue<T>(nSetting AS INT, oValue AS OBJECT) AS T
		LOCAL result AS T
		BEGIN LOCK oSettings
			IF oSettings.ContainsKey(nSetting)
				result :=  (T) oSettings[nSetting]
			ELSE
				result := Default(T)
			ENDIF
			oSettings[nSetting] := oValue
		END LOCK
		RETURN result

	PUBLIC PROPERTY WorkAreas AS WorkAreas GET oWorkAreas

	PUBLIC STATIC METHOD SetLastError(oError AS Exception) AS VOID
		VAR oInstance := GetInstance()
		oInstance:oLastError := oError
		RETURN

	#region IDisposable
	PUBLIC VIRTUAL METHOD Dispose AS VOID
		IF oWorkAreas != NULL
			oWorkareas:CloseAll()
			oWorkareas := NULL
		ENDIF
		RETURN
	#endregion 
	
END CLASS	
END NAMESPACE
