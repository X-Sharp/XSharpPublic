// Application : XPorter
// SolutionConverter.prg , Created : 28-9-2016   17:46
// User : robert

USING System.Collections.Generic
USING System.Xml                           


CLASS SolutionConverter
	PROTECT cSource   AS STRING
	PROTECT cTarget   AS STRING
	PROTECT aGuids	  AS Dictionary<STRING, STRING>
	PROTECT aFiles    AS List<STRING>
	PROTECT cPath	  AS STRING                
	PROTECT oProgress AS IProgress
	PROPERTY Target AS STRING GET cTarget
	PROPERTY Source AS STRING GET cSource
	CONSTRUCTOR(cName AS STRING, loProgress AS IProgress)
		cSource := cName
		cPath     := System.IO.Path.GetDirectoryName(cSource)  
		IF ! cPath:EndsWith("\")
			cPath += "\"
		ENDIF
		cTarget := cPath+System.IO.Path.GetFileNameWithoutExtension(cSource)+"-XS.SLN"		
		aFiles := List<STRING> {} 
		aGuids := Dictionary<STRING, STRING>{}
		oProgress := loProgress
	METHOD Start() AS LOGIC
		LOCAL oReader AS System.IO.TextReader
		LOCAL oWriter AS System.IO.TextWriter
		oProgress:WriteLine("Processing..."+cSource)
		oReader := System.IO.StreamReader{cSource}
		oWriter := System.IO.StreamWriter{cTarget}
		DO WHILE oReader:Peek() >= 0
			LOCAL cLine AS STRING
			cLine := oReader:ReadLine()
			IF cLine:Trim():StartsWith("Project(", StringComparison.OrdinalIgnoreCase) 
				IF cLine:ToUpper():Contains("{5891B814-A2E0-4E64-9A2F-2C2ECAB940FE}")
					// Vulcan Project
					// Line looks like this:
					// Project("VULCANGUID") = "Name", "Filename.vnproj", "PROJECTGUID"
					// We can split the line based on the double quote
					// then we get
					// a[1] = Project(
					// a[2] = VULCANGUID
					// a[3] = ) = 
					// a[4] = Name
					// a[5] = , 
					// a[6] = Filename.vnproj
					// a[7] = ,
					// a[8] = PROJECTGUID                        
					// a[9] = <empty>
					// We can then convert the vnproj file and change it to xsproj
					// and replace VULCANGUID with XSHUID
					// RelativePath with changed extension
					// ProjectGUID with new GUID
					LOCAL aElements AS STRING[]
					aElements := cLine:Split(e"\"":ToCharArray(), StringSplitOptions.None)
					IF aElements:Length == 9
						LOCAL cNewFile AS STRING
						LOCAL oPrjConverter AS ProjectConverter
						oPrjConverter := ProjectConverter{oProgress}
						aElements[2] := "{AA6C8D78-22FF-423A-9C7C-5F2393824E04}"	// XS Guid
						cNewFile := System.IO.Path.ChangeExtension(aElements[6], "."+EXTENSION)
						oPrjConverter:ConvertProjectFile(cPath+aElements[6], cPath+cNewFile)     
						aFiles:Add(cPath+cNewFile)
						aElements[6] := cNewFile      
						IF ! aGuids:ContainsKey(aElements[8])
							aGuids:Add(aElements[8], oPrjConverter:Guid)
						ENDIF
						aElements[8] := oPrjConverter:Guid    
						
						cLine := ""
						FOREACH VAR cElement IN aElements
							IF !String.IsNullOrEmpty(cLine)
								cLine += e"\""
							ENDIF
							cLine += cElement
						NEXT
					ELSE
						// Something wrong  
						oProgress:WriteLine( "Incorrect line in Solution: "+ cLine)
						
					ENDIF
				ENDIF
			ELSE
				FOREACH VAR item IN aGuids
					IF cLine:Contains(item:Key)
						cLine := cLine:Replace(item:Key, item:Value)
					ELSEIF cLine:Contains(item:Key:ToLower())
						cLine := cLine:Replace(item:Key:ToLower(), item:Value)
					ENDIF
				NEXT				
			ENDIF
			oWriter:WriteLine(cLine)
		ENDDO    
		oReader:Close()
		oWriter:Close()
		// Now update project references in all xsprj files
		FOREACH VAR sFile IN aFiles
			LOCAL sContents AS STRING
			LOCAL lChanged := FALSE AS LOGIC
			if System.IO.File.Exists(sFile)
				sContents := System.IO.File.ReadAllText(sFile)
				FOREACH VAR sItem IN aGuids
					IF sContents:Contains(sItem:Key)
						sContents := sContents:Replace(sItem:Key, sItem:Value)
						lChanged := TRUE
					ELSEIF sContents:Contains(sItem:Key:ToLower())
						sContents := sContents:Replace(sItem:Key:ToLower(), sItem:Value)
						lChanged := TRUE
					ENDIF
				NEXT                    
				IF lChanged
					System.IO.File.WriteAllText(sFile, sContents)
				ENDIF
			ENDIF
		NEXT
		
		RETURN TRUE		
		
STATIC METHOD Convert(cFile AS STRING, oProgress AS IProgress) AS VOID
	LOCAL oSolutionConverter AS SolutionConverter
	oSolutionConverter := SolutionConverter{cFile, oProgress}
	IF oSolutionConverter:Start()
		oProgress:WriteLine( "Created " +oSolutionConverter:Target)
		oProgress:WriteLine( "Done")
	ELSE                     
		oProgress:WriteLine( "Error converting "+cFile)
	ENDIF  
		
		
END CLASS	


