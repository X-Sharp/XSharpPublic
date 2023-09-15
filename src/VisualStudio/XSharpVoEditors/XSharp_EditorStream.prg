//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.IO
USING System.Text
USING Xide
USING XSharpModel

// inherits from vulcan's EditorStream
// represents contents of file that can be read/saved directly on disk or
// through an open editor buffer in VS
CLASS XSharp_EditorStream INHERIT EditorStream
	PROTECT oXSharpEditor AS XSharpBuffer
	PROTECT _fileName     AS STRING
	CONSTRUCTOR()
		SUPER()
	RETURN

	NEW ACCESS Editor AS XSharpBuffer
		RETURN SELF:oXSharpEditor

	// called by the editor's GetSaveFileStreams()
	VIRTUAL METHOD Load(aLines AS List<STRING>) AS VOID
		SELF:oXSharpEditor := XSharpBuffer.Create(aLines)

	VIRTUAL METHOD Load(cFileName AS STRING) AS VOID
		LOCAL aLines := NULL AS List<STRING>
		LOCAL oFile AS XSharpModel.XFile
		LOCAL oProject AS XSharpModel.XProject
		LOCAL cSource AS STRING
		_fileName := cFileName
		TRY
			LOCAL lOpenInVS := FALSE AS LOGIC
			// Note this will not work yet for RC or .VNFrm files. These are not in the codemodel
			oFile := XSharpModel.XSolution.FindFile(cFileName)
			IF oFile != NULL_OBJECT
				oProject := oFile:Project
				cSource := oProject:ProjectNode:DocumentGetText(cFileName, REF lOpenInVS)
				IF lOpenInVs
					SELF:eType := EditorStreamType.Module
					BEGIN USING VAR oReader := StringReader{cSource}
						aLines := List<STRING>{}
						DO WHILE oReader:Peek() != -1
							aLines:Add(oReader:ReadLine())
						END DO
					END USING
				ENDIF
			ENDIF

			IF .not. lOpenInVS
				SELF:eType := EditorStreamType.File
				SELF:oStream := File.Open(cFileName , FileMode.Open , FileAccess.ReadWrite , FileShare.None)
				SELF:oEncoding := DesignerBase.GetEncoding(SELF:oStream)
				// Do NOT use BEGIN USING because Disposing the StreamReader will also close the stream !
				VAR oReader := StreamReader{oStream , oEncoding}
				aLines := List<STRING>{}
				DO WHILE oReader:Peek() != -1
					aLines:Add(oReader:ReadLine())
				END DO
			END IF
      CATCH
         NOP
		END TRY

		SELF:oXSharpEditor := XSharpBuffer.Create(aLines)
	RETURN

	METHOD InsertLines(newLines AS List<STRING>, nFirstLine AS INT) AS VOID
		LOCAL i AS LONG
		FOR i := 1 TO newLines:Count
			SELF:oXSharpEditor:InsertLine(nFirstLine+i-1, newLines[i])
		NEXT


	METHOD Save() AS LOGIC
		LOCAL lSuccess:= FALSE AS LOGIC
		VAR aLines := SELF:oXSharpEditor:GetStringLines()
		VAR sb := StringBuilder{aLines:Count * 80}
		FOREACH VAR line IN aLines
			//sb:AppendLine(AddPartial(line))
			sb:AppendLine(line) // the virtual editor already checks for class definition modifiers and retains them
		NEXT
        var source := sb:ToString()
    	LOCAL oFile := XSolution.FindFile(_fileName) as XFile
        if oFile == NULL_OBJECT
            XFuncs.ErrorBox("Could not find in codemodel for file :"+_fileName+e"\r\n"+"File will not be saved","Save")
            RETURN FALSE
        ENDIF
        IF oFile:XFileType == XFileType.SourceCode
            IF oFile:Project != NULL .and. oFile:Project:ProjectNode != NULL
                source := oFile:Project:ProjectNode:SynchronizeKeywordCase(source, oFile:FullPath)
            ENDIF
        ENDIF

		IF SELF:eType == EditorStreamType.Module
			VAR oProject := oFile:Project
			IF oProject != NULL_OBJECT
				lSuccess := oProject:ProjectNode:DocumentSetText(oFile:FullPath, source)
		    ELSE
		        // Write to disk
		        File.WriteAllText(oFile:FullPath, source)
			ENDIF
		ELSE
			TRY
				SELF:oStream:SetLength(0)
				var oWriter := StreamWriter{SELF:oStream , SELF:oEncoding}
                oWriter:Write(source)
				oWriter:Flush()
				lSuccess := TRUE
			CATCH e AS Exception
                IF System.Diagnostics.Debugger.IsAttached
					System.Diagnostics.Debug.WriteLine(e:Message)
				ENDIF
			FINALLY
				SELF:oStream:Close()
			END TRY
		END IF
	RETURN lSuccess

/*	METHOD AddPartial(line AS STRING) AS STRING
		IF (line:IndexOf("class",StringComparison.OrdinalIgnoreCase) >= 0)
			VAR line2 := line:TrimStart():ToUpper()
			IF line2:StartsWith("CLASS ")
				VAR prefixlength := line:Length-line2:Length
				VAR spaces		 := line:Substring(0, prefixlength)
				line := spaces+ "PARTIAL "+line:Substring(prefixlength)
			ENDIF
		ENDIF
		RETURN line*/
END CLASS

