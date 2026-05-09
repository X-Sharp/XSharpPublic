// SCXExport.prg

USING System
USING System.Collections.Generic
USING System.Text
USING VFPXPorterLib

/// <summary>
/// Export a single SCX (form) or VCX (class library) file.
/// Returns TRUE on success.
/// </summary>
FUNCTION SCXExport( inputFile AS STRING, outputFolder AS STRING, doBackup AS LOGIC ) AS LOGIC
	VAR xPorter := XPorterSCXVCX{}
	xPorter:Initialize(inputFile, outputFolder, XPorterSettings{})
	xPorter:Export(doBackup)
	RETURN TRUE


/// <summary>
/// Export a full VFP project (.pjx) — forms, libraries, menus, programs.
/// Returns TRUE on success.
/// </summary>
FUNCTION PJXExport( pjxFile AS STRING, outputFolder AS STRING, doBackup AS LOGIC ) AS LOGIC
	LOCAL success AS LOGIC
	VAR xPorter := XPorterProject{pjxFile, outputFolder}
	xPorter:Settings := XPorterSettings{}
	success := xPorter:ProcessPJX()
	IF success
		success := xPorter:ExportProject(doBackup, NULL)
	ENDIF
	RETURN success
