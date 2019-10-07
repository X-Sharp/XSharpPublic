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
USING Microsoft.Win32.SafeHandles
USING System.Runtime
USING System.Runtime.ConstrainedExecution


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/frename/*" />	
FUNCTION FRename( cOldFile AS STRING , cNewFile AS STRING) AS LOGIC
	LOCAL renamed := FALSE AS LOGIC
	TRY
        XSharp.IO.File.clearErrorState()
        IF System.IO.File.Exists(cOldFile)
		    System.IO.File.Move(cOldFile, cNewFile)
	        renamed := TRUE
        ELSE
            THROW FileNotfoundException { cOldFile }
        ENDIF
	CATCH e AS Exception
		XSharp.IO.File.setErrorState(e)
	END TRY
	RETURN renamed
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ferase/*" />	
FUNCTION FErase(cFileName AS STRING) AS LOGIC
	LOCAL isDeleted := FALSE AS LOGIC
	TRY
        XSharp.IO.File.clearErrorState()
        IF System.IO.File.Exists(cFileName)
		    System.IO.File.Delete(cFileName)
		    isDeleted := TRUE
        ELSE
            THROW FileNotfoundException { cFileName }
        ENDIF
	CATCH e AS Exception
		XSharp.IO.File.setErrorState(e)
	END TRY
	RETURN isDeleted
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fcopy/*" />	
FUNCTION FCopy(cSourceFile AS STRING,cTargetFile AS STRING) AS LOGIC
	RETURN FCopy(cSourceFile, cTargetFile, TRUE)


/// <inheritdoc cref="M:XSharp.Core.Functions.FCopy(System.String,System.String)" />	
/// <param name="lOverWrite">Should the target file be overwritten.</param>
FUNCTION FCopy(cSourceFile AS STRING,cTargetFile AS STRING, lOverWrite AS LOGIC) AS LOGIC
	LOCAL IsCopied := FALSE AS LOGIC
	TRY
        XSharp.IO.File.clearErrorState()
		System.IO.File.Copy(cSourceFile,cTargetFile,lOverWrite)
		IsCopied := TRUE
	CATCH e AS Exception
		XSharp.IO.File.setErrorState(e)
	END TRY
	RETURN IsCopied
	
	
	
/// <summary><include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/summary" /></summary>
/// <returns><include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/returns" /></returns>
/// <remarks><include file="VoFunctionDocs.xml" path="Runtimefunctions/splitpath/remarks" /></remarks>
/// <param name="cPath">The path name to break.</param>
/// <param name="cDrive">The drive letter followed by a colon.  </param>
/// <param name="cDir">The directories, including the trailing slash.  Forward slashes and backslashes both may be present in &lt;cPath&gt;.  Forward slashes (/) are converted to backslashes (\). </param>
/// <param name="cName">The file name, without the extension.  </param>
/// <param name="cExt">The extension, including the leading period.  </param>
FUNCTION _SplitPath(cPath AS STRING, cDrive OUT STRING,cDir OUT STRING,cName OUT STRING,cExt OUT STRING) AS VOID
	LOCAL nPos AS LONG
	LOCAL cSep AS STRING
	cDrive	:= ""
	cDir	:= ""
	cName	:= ""
	cExt	:= ""
	IF String.IsNullOrEmpty(cPath)
		RETURN
	ENDIF
	cSep := Path.DirectorySeparatorChar:ToString()
	nPos := cPath:IndexOf(Path.VolumeSeparatorChar)
	IF nPos > 0
		cDrive := cPath:Substring(0, nPos+1)
		cPath  := cPath:SubString(nPos + 1)
	ENDIF
	
	IF cPath:Trim() != ""
		cDir := Path.GetDirectoryName(cPath)
	ENDIF
	
	IF String.IsNullOrEmpty( cDir )
		IF cPath:StartsWith(cSep)
			cDir := cSep
		ELSE
			cDir := ""
		ENDIF
	ELSEIF ! cDir:EndsWith(cSep)
		cDir += cSep
	ENDIF
	
	cName := Path.GetFileNameWithoutExtension(cPath)
	cExt  := Path.GetExtension(cPath)
	
	RETURN
	
