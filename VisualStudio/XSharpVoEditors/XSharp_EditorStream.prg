USING System.Collections.Generic
USING System.IO
USING System.Text

USING Xide

// inherits from vulcan's EditorStream
// represents contents of file that can be read/saved directly on disk or 
// through an open editor buffer in VS
CLASS XSharp_EditorStream INHERIT EditorStream
	PROTECT oXSharpEditor AS XSharpBuffer
	PROTECT _fileName     as string
	CONSTRUCTOR()
		SUPER()
	RETURN
	
	NEW ACCESS Editor AS XSharpBuffer
		RETURN SELF:oXSharpEditor

	// called by the editor's GetSaveFileStreams()
	VIRTUAL METHOD Load(aLines as List<String>) AS VOID
		SELF:oXSharpEditor := XSharpBuffer.Create(aLines)

	VIRTUAL METHOD Load(cFileName AS STRING) AS VOID
		LOCAL aLines := NULL AS List<STRING>
		local oFile as XSharpModel.XFile
		local oProject as XSharpModel.XProject
		local cSource as STRING
		_fileName := cFileName
		TRY
			LOCAL lOpenInVS := FALSE AS LOGIC
			// Note this will not work yet for RC or .VNFrm files. These are not in the codemodel
			oFile := XSharpModel.XSolution.FindFile(cFileName)
			IF oFile != NULL_OBJECT
				oProject := oFile:Project
				cSource := oProject:ProjectNode:DocumentGetText(cFileName, ref lOpenInVS)
				if lOpenInVs
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
		END TRY

		SELF:oXSharpEditor := XSharpBuffer.Create(aLines)
	RETURN
	
	METHOD InsertLines(newLines as List<String>, nFirstLine as INT) AS VOID
		LOCAL i as LONG
		for i := 1 to newLines:Count
			SELF:oXSharpEditor:InsertLine(nFirstLine+i-1, newLines[i])
		next
		

	METHOD Save() AS LOGIC
		LOCAL lSuccess:= FALSE AS LOGIC
		VAR aLines := SELF:oXSharpEditor:GetStringLines()
		IF SELF:eType == EditorStreamType.Module
			VAR oFile := XSharpModel.XSolution.FindFile(_fileName)
			IF oFile != NULL_OBJECT
				VAR oProject := oFile:Project
				IF oProject != NULL_OBJECT
					VAR sb := StringBuilder{aLines:Count * 80}
					FOREACH VAR line in aLines
						sb:AppendLine(AddPartial(line))
					NEXT
					lSuccess := oProject:ProjectNode:DocumentSetText(oFile:FullPath, sb:ToString())
				ENDIF
			ENDIF
			
		ELSE
			TRY
				SELF:oStream:SetLength(0)
				VAR oWriter := StreamWriter{SELF:oStream , SELF:oEncoding}
				FOREACH VAR cLine IN aLines
					oWriter:WriteLine(AddPartial(cLine))
				NEXT
				lSuccess := TRUE
				oWriter:Flush()
			CATCH e as Exception
				System.Diagnostics.Debug.WriteLine(e:Message)
			FINALLY
				SELF:oStream:Close()
			END TRY
		END IF
	RETURN lSuccess

	METHOD AddPartial(line AS STRING) AS STRING
		if (line:IndexOf("class",StringComparison.OrdinalIgnoreCase) >= 0)
			var line2 := line:TrimStart():ToUpper()
			IF line2:StartsWith("CLASS ")
				var prefixlength := line:Length-line2:Length 
				VAR spaces		 := line:Substring(0, prefixlength)
				line := spaces+ "PARTIAL "+line:Substring(prefixlength)
			ENDIF
		endif
		return line
END CLASS

