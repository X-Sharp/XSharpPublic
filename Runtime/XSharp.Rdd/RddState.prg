USING System.Collections.Generic
USING System.Threading
USING XSharp.Runtime

BEGIN NAMESPACE XSharp.Runtime
CLASS RDDState
	// Static Fields
	PRIVATE STATIC StateTable AS Dictionary<Thread, RDDState>
	PRIVATE STATIC MainThread AS Thread   
	// Static Properties	
	PRIVATE STATIC PROPERTY MainState AS RDDState GET StateTable[MainThread]
	// Static Methods and Constructor
	STATIC CONSTRUCTOR
		StateTable := Dictionary<Thread, RDDState>{}
		VAR oCurrent := RDDState{}
		MainThread := Thread.CurrentThread
		StateTable:Add(MainThread, oCurrent)				
	
	PUBLIC STATIC METHOD GetInstance() AS RDDState
		VAR oThread := Thread.CurrentThread
		LOCAL oState AS RDDState   
		BEGIN LOCK StateTable
			IF ! StateTable.ContainsKey(oThread)   
				oState := MainState:Clone()
				StateTable:Add(oThread, oState)
			ELSE
				oState := StateTable[oThread]
			ENDIF                
		END LOCK            
		RETURN oState

	// Private Fields		
	PRIVATE Settings AS Dictionary<INT, OBJECT>

	// Private methods
	PRIVATE CONSTRUCTOR()       
		SELF:Name := "State for Thread "+Thread.CurrentThread.ManagedThreadId.ToString()
		Settings := Dictionary<INT, OBJECT>{}
		RETURN
	PRIVATE METHOD Clone() AS RDDState
		LOCAL oNew AS RDDState
		oNew := RDDState{}		
		BEGIN LOCK Settings
			// Copy all values from Current State to New state
			FOREACH VAR element IN Settings     
				oNew:Settings[element:Key] := element:Value
			NEXT
		END LOCK
		RETURN oNew     
		
	// Public Fields and Methods		
	PUBLIC PROPERTY Name AS STRING AUTO
	PUBLIC VIRTUAL METHOD ToString() AS STRING
		RETURN SELF:Name
	PUBLIC METHOD GetValue<T> (nSetting AS INT) AS T
		IF Settings.ContainsKey(nSetting)
			RETURN (T) Settings[nSetting]
		ENDIF
		RETURN Default(T)
	PUBLIC METHOD SetValue(nSetting AS INT, oValue AS OBJECT) AS VOID
		Settings[nSetting] := oValue
		RETURN			

END CLASS	
END NAMESPACE
