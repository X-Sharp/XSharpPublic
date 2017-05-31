USING System.Collections.Generic
USING System.Threading
USING XSharp.Runtime
USING XSharp

CLASS XSharp.Runtime.State
	// Static Fields
	PRIVATE STATIC StateTable AS Dictionary<Thread, XSharp.Runtime.State>
	PRIVATE STATIC MainThread AS Thread   
	// Static Properties	
	PRIVATE STATIC PROPERTY MainState AS State GET StateTable[MainThread]
	// Static Methods and Constructor
	STATIC CONSTRUCTOR
		StateTable := Dictionary<Thread, XSharp.Runtime.State>{}
		VAR oCurrent := State{}
		MainThread := Thread.CurrentThread
		StateTable:Add(MainThread, oCurrent)				
	
	PUBLIC STATIC METHOD GetInstance() AS State
		VAR oThread := Thread.CurrentThread
		LOCAL oState AS State   
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
	PRIVATE METHOD Clone() AS State
		LOCAL oNew AS State
		oNew := State{}		
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


STATIC CLASS XSharp.RuntimeState
    STATIC PROPERTY Optimize AS LOGIC ;
        GET XSharp.Runtime.State.GetInstance():GetValue<LOGIC>(Set.OPTIMIZE);
        SET XSharp.Runtime.State.GetInstance():SetValue(Set.OPTIMIZE, value)
    STATIC PROPERTY Deleted AS LOGIC ;
        GET XSharp.Runtime.State.GetInstance():GetValue<LOGIC>(Set.DELETED);
        SET XSharp.Runtime.State.GetInstance():SetValue(Set.DELETED, value)
    STATIC PROPERTY Exact AS LOGIC ;
        GET XSharp.Runtime.State.GetInstance():GetValue<LOGIC>(Set.EXACT);
        SET XSharp.Runtime.State.GetInstance():SetValue(Set.EXACT, value)
    STATIC PROPERTY Epoch AS DWORD ;
        GET XSharp.Runtime.State.GetInstance():GetValue<DWORD>(Set.EPOCH);
        SET XSharp.Runtime.State.GetInstance():SetValue(Set.EPOCH, value)
    STATIC PROPERTY Decimals AS DWORD ;
        GET XSharp.Runtime.State.GetInstance():GetValue<DWORD>(Set.DECIMALS);
        SET XSharp.Runtime.State.GetInstance():SetValue(Set.DECIMALS, value)

    STATIC PROPERTY DateFormat AS STRING ;
        GET XSharp.Runtime.State.GetInstance():GetValue<STRING>(Set.DateFormat);
        SET XSharp.Runtime.State.GetInstance():SetValue(Set.DateFormat, value)


END CLASS