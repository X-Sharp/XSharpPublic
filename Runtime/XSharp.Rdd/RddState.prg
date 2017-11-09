USING System.Collections.Generic
USING System.Threading
USING XSharp.Runtime
USING System

BEGIN NAMESPACE XSharp.RDD
//Todo: Use ThreadLocal<T>
CLASS RDDState	IMPLEMENTS IDisposable
	// Static Fields
	PRIVATE STATIC stateTable AS Dictionary<Thread, RDDState>
	PRIVATE STATIC mainThread AS Thread   
	PRIVATE STATIC mainState  AS RDDState

	// Normal Fields
	PRIVATE oWorkareas	AS WorkAreas
	PRIVATE oThread		AS Thread
	PRIVATE oLastError	AS Exception
	PRIVATE oSettings	AS Dictionary<INT, OBJECT>

	
	// Static Methods and Constructor
	STATIC CONSTRUCTOR()
		stateTable		:= Dictionary<Thread, RDDState>{}
		mainThread		:= Thread.CurrentThread
		mainState       := RDDState{}
	
	PUBLIC STATIC METHOD GetInstance() AS RDDState
		VAR oThread := Thread.CurrentThread
		LOCAL oState AS RDDState   
		BEGIN LOCK stateTable
			IF ! stateTable.ContainsKey(oThread)   
				oState := MainState:Clone()
			ELSE
				oState := stateTable[oThread]
			ENDIF                
		END LOCK            
		RETURN oState

	// Private methods
	PRIVATE CONSTRUCTOR()       
		oThread		:= Thread.CurrentThread
		SELF:Name	:= "RDDState for Thread "+oThread:ManagedThreadId:ToString()
		oSettings	:= Dictionary<INT, OBJECT>{}
		oWorkareas  := WorkAreas{}
		BEGIN LOCK stateTable
			stateTable:Add(oThread, SELF)
		END LOCK
		RETURN

	PRIVATE METHOD Clone() AS RDDState
		LOCAL oNew AS RDDState
		oNew := RDDState{}		
		BEGIN LOCK oSettings
			// Copy all values from Current State to New state
			FOREACH VAR element IN oSettings     
				oNew:SetValue(element:Key, element:Value)
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
		RETURN DEFAULT(T)

	PUBLIC METHOD SetValue(nSetting AS INT, oValue AS OBJECT) AS VOID
		BEGIN LOCK oSettings
			oSettings[nSetting] := oValue
		END LOCK
		RETURN			

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
