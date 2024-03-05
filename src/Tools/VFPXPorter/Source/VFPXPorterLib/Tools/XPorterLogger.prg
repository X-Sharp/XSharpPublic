// XPorterLogger.prg
// Created by    : fabri
// Creation Date : 6/11/2023 5:37:48 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING Serilog
USING Serilog.Core

BEGIN NAMESPACE VFPXPorterLib

/// <summary>
/// The XPorterLogger class.
/// </summary>
STATIC CLASS XPorterLogger

	STATIC CONSTRUCTOR()
		XPorterLogger.SetLoggerToFile( NULL )
		RETURN

	STATIC PRIVATE logger AS ILogger

	PUBLIC STATIC PROPERTY Instance AS ILogger
		GET
			IF XPorterLogger.logger == NULL
				XPorterLogger.SetLoggerToFile( NULL )
			ENDIF
			RETURN XPorterLogger.logger
		END GET
	END PROPERTY

	STATIC METHOD SetLoggerToFile( fileLog AS STRING ) AS VOID
		VAR log := LoggerConfiguration{}
		IF String.IsNullOrEmpty( fileLog )
			XPorterLogger.logger := log:WriteTo:BufferedLog():CreateLogger()
		ELSE
			XPorterLogger.logger := log:WriteTo:File( fileLog ):CreateLogger()
		ENDIF

END CLASS
END NAMESPACE // VFPXPorterLib.Tools