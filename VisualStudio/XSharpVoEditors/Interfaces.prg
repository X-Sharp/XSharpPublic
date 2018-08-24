// IEditors.prg
// Created by    : robert
// Creation Date : 8/2/2017 3:23:20 PM
// Created for   : 
// WorkStation   : ZEUS
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VOEditors
	INTERFACE IVOWEDControl
	// properties
	PROPERTY IsGridEnabled AS LOGIC GET
	PROPERTY ReadOnly AS LOGIC GET SET 
	PROPERTY IsDirty AS LOGIC GET 

	PROPERTY IWin32Window AS IWin32Window GET 

	METHOD Action(nType AS Actions) AS VOID
	METHOD CanDoAction(nType AS Actions) AS LOGIC
	
	METHOD Dispose() AS VOID
	METHOD ShowTabOrder() AS VOID
	METHOD TestForm() AS VOID
	METHOD ToggleGrid() AS VOID

	// Recorder
	METHOD StopRecorder() AS VOID
	METHOD RecordCommand(sCommand AS STRING) AS VOID

	// IO
	METHOD Save(strFile AS STRING) AS LOGIC
	METHOD Save(strFile AS STRING, lFormOnly AS LOGIC) AS LOGIC
	METHOD OpenWindow(cFileName AS STRING) AS LOGIC
	METHOD OpenMenu(cFileName AS STRING) AS LOGIC
	METHOD OpenDBServer(cFileName AS STRING) AS LOGIC
	METHOD OpenFieldSpec(cFileName AS STRING) AS LOGIC

	// Delegates
	PROPERTY IsDirtyChanged AS EventHandler SET
	PROPERTY TriggerSave AS EventHandler SET
	PROPERTY StatusMessage AS StatusMessageDelegate SET
	END INTERFACE

	DELEGATE StatusMessageDelegate(cMsg AS STRING) AS VOID
	
END NAMESPACE


