USING System.Collections.Generic
USING System.IO
USING System.Text

USING Xide

// inherits from vulcan's EditorStream
// represents contents of file that can be read/saved directly on disk or 
// through an open editor buffer in VS
CLASS XSharp_EditorStream INHERIT EditorStream
	PROTECT oXSharpVS_FileRepresentation AS OBJECT
	PROTECT oXSharpEditor AS XSharpBuffer
//	PROTECT cDiskFile AS STRING

	CONSTRUCTOR()
		SUPER()
	RETURN
	
	NEW ACCESS Editor AS XSharpBuffer
	RETURN SELF:oXSharpEditor

	// called by the editor's GetSaveFileStreams()
	VIRTUAL METHOD Load(cFileName AS STRING) AS VOID
		LOCAL aLines := NULL AS List<STRING>

		TRY
	
			LOCAL lOpenInVS := FALSE AS LOGIC

			IF FALSE // VS.FileIsOpenInAnEditorBuffer()

				SELF:eType := EditorStreamType.Module

				lOpenInVS := TRUE
				// if file is open in VS, fille the aLines var with the source code lines currently in the VS source editor
//				aLines := Funcs.BufferToLines(VS_file_BufferAsSingleString)
//				aLines := List<STRING>{VS_file_BufferAsArrayOfStrings}
			ENDIF

			IF .not. lOpenInVS

				SELF:eType := EditorStreamType.File

//				SELF:cDiskFile := cFileName
				SELF:oStream := File.Open(cFileName , FileMode.Open , FileAccess.ReadWrite , FileShare.None)
				SELF:oEncoding := DesignerBase.GetEncoding(SELF:oStream)

				LOCAL oReader AS StreamReader
				oReader := StreamReader{oStream , oEncoding}
				aLines := List<STRING>{}
				DO WHILE oReader:Peek() != -1
					aLines:Add(oReader:ReadLine())
				END DO
//				oReader:Dispose()
				
			END IF
		END TRY

		SELF:oXSharpEditor := XSharpBuffer.Create(aLines)
	RETURN

	METHOD Save() AS LOGIC
		LOCAL lSuccess:= FALSE AS LOGIC

		IF SELF:eType == EditorStreamType.Module
			LOCAL aLines AS List<STRING>
			aLines := SELF:oXSharpEditor:GetStringLines()
			// save code contents to the open file buffer
			lSuccess := TRUE
		ELSE
			TRY
				SELF:oStream:SetLength(0)
				LOCAL oWriter AS StreamWriter
				oWriter := StreamWriter{SELF:oStream , SELF:oEncoding}
				LOCAL aLines AS List<STRING>
				aLines := SELF:oXSharpEditor:GetStringLines()
				FOREACH cLine AS STRING IN aLines
					oWriter:WriteLine(cLine)
				NEXT
//				File.WriteAllLines(SELF:cDiskFile , SELF:oXSharpEditor:GetStringLines() , SELF:oEncoding)
				lSuccess := TRUE
				oWriter:Flush()
				oWriter:Dispose()
			FINALLY
				SELF:oStream:Close()
			END TRY
		END IF
	RETURN lSuccess

END CLASS

