//USING System.Collections.Generic
//USING System.Threading
//USING XSharp.Runtime
//USING XSharp
//
/// <Summary>
/// Container Class that holds the XSharp Runtime state
/// </Summary>
/// <Remarks>
/// Please note that unlike in Visual Objects and Vulcan.NET every thread has its own copy of the runtime state.</br>
/// The runtime state from a new thread is a copy of the state of the main thread at that moment.
/// </Remarks>
//CLASS XSharp.Runtime.State
	// Static Fields
//	PRIVATE STATIC stateTable AS Dictionary<Thread, State>
//	PRIVATE STATIC mainThread AS Thread   
	// Static Properties	
//	PRIVATE STATIC mainState  AS State 
	// Static Methods and Constructor
//
//	STATIC CONSTRUCTOR
//		stateTable := Dictionary<Thread, State>{}
//		mainState  := State{}
//	
	/// <Summary>Retrieve the runtime state for the current thread</Summary>
//	PUBLIC STATIC METHOD GetInstance() AS State
//		VAR oThread := Thread.CurrentThread
//		LOCAL oState AS State   
//		BEGIN LOCK stateTable
//			IF ! stateTable.ContainsKey(oThread)   
//				oState := mainState:Clone()
//				stateTable:Add(oThread, oState)
//			ELSE
//				oState := stateTable[oThread]
//			ENDIF                
//		END LOCK            
//		RETURN oState
//
	/// <Summary>List of number - value pairs </Summary>
//	PRIVATE oSettings AS Dictionary<INT, OBJECT>
//
	// Private methods
//	PRIVATE CONSTRUCTOR()       
//		VAR oThread := Thread.CurrentThread
//		SELF:Name := "State for Thread "+oThread:ManagedThreadId:ToString()
//		oSettings  := Dictionary<INT, OBJECT>{}
//		stateTable:Add(oThread, SELF)			
//
//		RETURN
//
//	PRIVATE METHOD Clone() AS State
//		LOCAL oNew AS State
//		oNew := State{}		
//		BEGIN LOCK oSettings
			// Copy all values from Current State to New state
//			FOREACH VAR element IN oSettings     
//				oNew:oSettings[element:Key] := element:Value
//			NEXT
//		END LOCK
//		RETURN oNew     
//		
	/// <Summary>Retrieve state name</Summary>
	/// <Returns>String value, such as "State for Thread 123"</Returns>
//	PUBLIC PROPERTY Name AS STRING AUTO
//
	/// <Summary>ToString() override</Summary>
	/// <Returns>String value, such as "State for Thread 123"</Returns>
//	PUBLIC VIRTUAL METHOD ToString() AS STRING
//		RETURN SELF:Name
//
//	PUBLIC STATIC METHOD GetValue<T> (nSetting AS INT) AS T
//		VAR oState := GetInstance()
//		RETURN oState:GetInstanceValue<T>(nSetting);
//
//	PUBLIC STATIC METHOD SetValue (nSetting AS INT, oValue AS OBJECT) AS VOID
//		VAR oState := GetInstance()
//		oState:SetInstanceValue(nSetting, oValue)
//
//
	/// <Summary>Get the value for a certain setting</Summary>
	/// <param Name="nSetting"> The number of the setting to change</param>
	/// <typeparam Name="T"> The expected return type of the value</typeparam>
	/// <Returns>The new value</Returns>
//	PRIVATE METHOD GetInstanceValue<T> (nSetting AS INT) AS T
//		BEGIN LOCK oSettings
//			IF oSettings.ContainsKey(nSetting)
//				RETURN (T) oSettings[nSetting]
//			ENDIF
//		END LOCK
//		RETURN Default(T)
	/// <Summary>Set the value for a certain setting</Summary>
	/// <param Name="nSetting"> The number of the setting to change</param>
	/// <param Name="oValue"> The new value of the setting.</param>
//	PRIVATE METHOD SetInstanceValue(nSetting AS INT, oValue AS OBJECT) AS VOID
//		BEGIN LOCK oSettings
//			oSettings[nSetting] := oValue
//		END LOCK
//		RETURN			
//
//END CLASS	
//
/// <Summary> Static class that has "simple" properties that return the common state for some well known  oSettings</Summary>
//STATIC CLASS XSharp.RuntimeState
	/// <Summary>RDD Optimize Flag</Summary>
	/// <Returns>Logic value</Returns>
//    STATIC PROPERTY Optimize AS LOGIC ;
//        GET XSharp.Runtime.State.GetValue<LOGIC>(Set.OPTIMIZE);
//        SET XSharp.Runtime.State.SetValue(Set.OPTIMIZE, VALUE)
	/// <Summary>RDD Deleted Flag that determines whether to ignore or include records that are marked for deletion.</Summary>
	/// <Returns>Logic value</Returns>
//    STATIC PROPERTY Deleted AS LOGIC ;
//        GET XSharp.Runtime.State.GetValue<LOGIC>(Set.DELETED);
//        SET XSharp.Runtime.State.SetValue(Set.DELETED, VALUE)
	/// <Summary>String comparison Exact flag that determines how comparisons with the single '=' characters should be done.</Summary>
	/// <Returns>Logic value</Returns>
//    STATIC PROPERTY Exact AS LOGIC ;
//        GET XSharp.Runtime.State.GetValue<LOGIC>(Set.EXACT);
//        SET XSharp.Runtime.State.SetValue(Set.EXACT, VALUE)
	/// <Summary>Date Epoch value that determines how dates without century digits are interpreted.</Summary>
	/// <Returns>DWORD value</Returns>
//    STATIC PROPERTY Epoch AS DWORD ;
//        GET XSharp.Runtime.State.GetValue<DWORD>(Set.EPOCH);
//        SET XSharp.Runtime.State.SetValue(Set.EPOCH, VALUE)
	/// <Summary>The default number of decimals for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
//    STATIC PROPERTY Decimals AS DWORD ;
//        GET XSharp.Runtime.State.GetValue<DWORD>(Set.DECIMALS);
//        SET XSharp.Runtime.State.SetValue(Set.DECIMALS, VALUE)
	/// <Summary>The current Date format</Summary>
	/// <Remarks>This string should contain a combination of DD MM and either YY or YYYY characters.<br>
	/// For example DD-MM-YYYY for italian date format,r MM/DD/YYYY for American date format or DD/MM/YYYY for British Date format.
	/// Note that all other characters except the four groups mentioned above are copied to the output string verbatim.
	/// </Remarks>
	/// <Returns>String value</Returns>
//    STATIC PROPERTY DateFormat AS STRING ;
//        GET XSharp.Runtime.State.GetValue<STRING>(Set.DateFormat);
//        SET XSharp.Runtime.State.SetValue(Set.DateFormat, VALUE)
//
//END CLASS
