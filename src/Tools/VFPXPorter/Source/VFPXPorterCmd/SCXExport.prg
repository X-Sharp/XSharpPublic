// SCXExport.prg

USING System
USING System.Collections.Generic
USING System.Text
USING VFPXPorterLib

/// <summary>
/// Export a single SCX (form) or VCX (class library) file.
/// Returns TRUE on success.
/// </summary>
FUNCTION SCXExport( inputFile AS STRING, outputFolder AS STRING, doBackup AS LOGIC, settings AS XPorterSettings ) AS LOGIC
	VAR xPorter := XPorterSCXVCX{}
	xPorter:Initialize(inputFile, outputFolder, settings)
	xPorter:Export(doBackup)
	RETURN TRUE


/// <summary>
/// Export a full VFP project (.pjx) — forms, libraries, menus, programs.
/// Returns TRUE on success.
/// </summary>
FUNCTION PJXExport( pjxFile AS STRING, outputFolder AS STRING, doBackup AS LOGIC, settings AS XPorterSettings ) AS LOGIC
	LOCAL success AS LOGIC
	Console.WriteLine("Processing project: " + pjxFile)
	VAR xPorter := XPorterProject{pjxFile, outputFolder}
	xPorter:Settings := settings
	success := xPorter:ProcessPJX()
	IF !success
		Console.ForegroundColor := ConsoleColor.Red
		Console.WriteLine("Error: failed to load project file.")
		Console.ResetColor()
		RETURN FALSE
	ENDIF
	Console.WriteLine("Exporting to: " + outputFolder)
	success := xPorter:ExportProject(doBackup, NULL)
	IF !success
		Console.ForegroundColor := ConsoleColor.Red
		Console.WriteLine("Error: export failed.")
		Console.ResetColor()
	ENDIF
	RETURN success
