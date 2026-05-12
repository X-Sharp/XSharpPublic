// SCXExport.prg
// Created by    : fabri
// Creation Date : 9/26/2020 2:25:52 PM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING VFPXPorterLib

FUNCTION SCXExport( inputFile AS STRING, outputFolder AS STRING, doBackup AS LOGIC, logFile AS STRING ) AS VOID
	//
	VAR xPorter := XPorterSCXVCX{}
	IF ( !String.IsNullOrEmpty(logFile ) )
		XPorterLogger.SetLoggerToFile( logFile )
		//xPorter:SetLoggerToFile( logFile )
	ENDIF
	xPorter:Initialize( inputFile, outputFolder, XPorterSettings{} )
	xPorter:Export( doBackup )
	//
	RETURN