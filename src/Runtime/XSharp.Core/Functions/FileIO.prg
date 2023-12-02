//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// File and Disk IO Functions

USING System
USING System.Collections
USING System.IO
USING System.Linq
USING System.Runtime.InteropServices
USING System.Security
USING System.Runtime
USING System.Runtime.ConstrainedExecution


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/frename/*" />
FUNCTION FRename( cOldFile AS STRING , cNewFile AS STRING) AS LOGIC
	LOCAL renamed := FALSE AS LOGIC
	TRY
        IF String.IsNullOrEmpty(cOldFile)
            BadFileParam(__FUNCTION__, nameof(cOldFile), 1)
            RETURN FALSE
        ENDIF
        IF String.IsNullOrEmpty(cNewFile)
            BadFileParam(__FUNCTION__, nameof(cNewFile), 2)
            RETURN FALSE
        ENDIF
        XSharp.IO.File.ClearErrorState()
        IF System.IO.File.Exists(cOldFile)
		    System.IO.File.Move(cOldFile, cNewFile)
	        renamed := TRUE
        ELSE
            THROW FileNotFoundException { cOldFile }
        ENDIF
	CATCH e AS Exception
		XSharp.IO.File.SetErrorState(e)
	END TRY
	RETURN renamed


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ferase/*" />
FUNCTION FErase(cFileName AS STRING) AS LOGIC
	LOCAL isDeleted := FALSE AS LOGIC
	TRY
        IF String.IsNullOrEmpty(cFileName)
            BadFileParam(__FUNCTION__, nameof(cFileName), 1)
            RETURN FALSE
        ENDIF
        XSharp.IO.File.ClearErrorState()
        IF System.IO.File.Exists(cFileName)
		    System.IO.File.Delete(cFileName)
		    isDeleted := TRUE
        ELSE
            THROW FileNotFoundException { cFileName }
        ENDIF
	CATCH e AS Exception
		XSharp.IO.File.SetErrorState(e)
	END TRY
	RETURN isDeleted

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcopy/*" />
FUNCTION FCopy(cSourceFile AS STRING,cTargetFile AS STRING) AS LOGIC
	RETURN FCopy(cSourceFile, cTargetFile, TRUE)


/// <inheritdoc cref="FCopy" />
/// <param name="lOverWrite">Should the target file be overwritten.</param>
FUNCTION FCopy(cSourceFile AS STRING,cTargetFile AS STRING, lOverWrite AS LOGIC) AS LOGIC
	LOCAL IsCopied := FALSE AS LOGIC
	TRY
	        XSharp.IO.File.ClearErrorState()
	        IF String.IsNullOrEmpty(cSourceFile)
	            BadFileParam(__FUNCTION__, nameof(cSourceFile), 1)
	            RETURN FALSE
	        ENDIF
	        IF String.IsNullOrEmpty(cTargetFile)
	            BadFileParam(__FUNCTION__, nameof(cTargetFile), 2)
	            RETURN FALSE
	        ENDIF

		System.IO.File.Copy(cSourceFile,cTargetFile,lOverWrite)
		IsCopied := TRUE
	CATCH e AS Exception
		XSharp.IO.File.SetErrorState(e)
	END TRY
	RETURN IsCopied



/// <summary><include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/summary" /></summary>
/// <returns><include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/returns" /></returns>
/// <remarks><include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/remarks" /></remarks>
/// <param name="cPath">The path name to break.</param>
/// <param name="cDrive">The drive letter followed by a colon.  </param>
/// <param name="cDir">The directories, including the trailing slash.  Forward slashes and backslashes both may be present in &lt;cPath&gt;.  Forward slashes (/) are converted to backslashes (\). </param>
/// <param name="cName">The file name, without the extension.  </param>
/// <param name="cExt">The extension, including the leading period.  </param>
FUNCTION _SplitPath(cPath AS STRING, cDrive OUT STRING,cDir OUT STRING,cName OUT STRING,cExt OUT STRING) AS VOID
	LOCAL nPos AS LONG
	LOCAL cSeparators AS STRING
	LOCAL lDotAfterDirSep := FALSE AS LOGIC
	cDrive	:= ""
	cDir	:= ""
	cName	:= ""
	cExt	:= ""
	IF String.IsNullOrEmpty(cPath)
		RETURN
	ENDIF
	cSeparators := "\/"
	IF cSeparators:IndexOf(Path.DirectorySeparatorChar) == -1
		cSeparators += Path.DirectorySeparatorChar:ToString()
	END IF
	IF cPath:Length >= 2 .and. cPath[1] == Path.VolumeSeparatorChar // What VO does
		cDrive := cPath:Substring(0, 2)
		cPath  := cPath:Substring(2)
	ENDIF

	nPos := -1
	FOREACH cSep AS Char IN cSeparators
		nPos := Math.Max(nPos, cPath:LastIndexOf(cSep))
	NEXT
	IF cPath:LastIndexOf('.') > nPos
		lDotAfterDirSep := TRUE
	END IF
	IF nPos != -1
		cDir := cPath:Substring(0, nPos+1)
		cName  := cPath:Substring(nPos + 1)
	ELSE
		cName := cPath
	ENDIF
	
	IF lDotAfterDirSep
		nPos := cName:LastIndexOf('.')
		IF nPos != -1 // should be always true
			cExt  := cName:Substring(nPos)
			cName := cName:Substring(0, nPos)
		END IF
	ENDIF

	RETURN

