// IEditors.prg
// Created by    : robert
// Creation Date : 8/2/2017 3:23:20 PM
// Created for   : 
// WorkStation   : ZEUS
using System.Windows.Forms

BEGIN NAMESPACE XSharp.VOEditors
	INTERFACE IVOWEDControl
	// properties
	PROPERTY IsGridEnabled as LOGIC GET
	PROPERTY ReadOnly as LOGIC GET SET 
	PROPERTY IsDirty as LOGIC GET 

	PROPERTY IWin32Window as IWin32Window GET 

	METHOD Action(nType as Actions) AS VOID
	METHOD CanDoAction(nType as Actions) AS LOGIC
	
	METHOD Dispose() as VOID
	METHOD ShowTabOrder() as VOID
	METHOD TestForm() as VOID
	METHOD ToggleGrid() AS VOID

	// Recorder
	METHOD StopRecorder() as VOID
	METHOD RecordCommand(sCommand as STRING) as VOID

	// IO
	METHOD Save(strFile as STRING) AS LOGIC
	METHOD Save(strFile as STRING, lFormOnly as LOGIC) AS LOGIC
	METHOD OpenWindow(cFileName AS STRING) AS LOGIC
	METHOD OpenMenu(cFileName AS STRING) AS LOGIC
	METHOD OpenDBServer(cFileName AS STRING) AS LOGIC
	METHOD OpenFieldSpec(cFileName AS STRING) AS LOGIC

	// Delegates
	PROPERTY IsDirtyChanged as EventHandler SET
	PROPERTY TriggerSave as EventHandler SET
	PROPERTY StatusMessage as StatusMessageDelegate SET
	END INTERFACE

	DELEGATE StatusMessageDelegate(cMsg as STRING) AS VOID
	
END NAMESPACE


