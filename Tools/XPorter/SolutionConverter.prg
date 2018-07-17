//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Collections.Generic
USING System.Xml                           


CLASS SolutionConverter
	PROTECT cSource   AS STRING
	PROTECT cTarget   AS STRING
	PROTECT aGuids	  AS Dictionary<STRING, STRING>
	PROTECT aFiles    AS List<STRING>
    PROTECT aOthers   AS List<STRING>
	PROTECT cPath	  AS STRING                
	PROTECT oProgress AS IProgress
    protect lAdjustReferences as LOGIC
	PROPERTY Target AS STRING GET cTarget
	PROPERTY Source AS STRING GET cSource
	CONSTRUCTOR(cName AS STRING, loProgress AS IProgress, lAdjust as LOGIC)
		cSource := cName
		cPath     := System.IO.Path.GetDirectoryName(cSource)  
		IF ! cPath:EndsWith("\")
			cPath += "\"
		ENDIF
		cTarget := cPath+System.IO.Path.GetFileNameWithoutExtension(cSource)+"-XS.SLN"		
		aFiles := List<STRING> {} 
        aOthers:= List<STRING> {} 
		aGuids := Dictionary<STRING, STRING>{}
		oProgress := loProgress
        lAdjustReferences := lAdjust
	METHOD Start() AS LOGIC
		LOCAL oReader AS System.IO.TextReader
		LOCAL oWriter AS System.IO.TextWriter
		LOCAL lChanged := FALSE AS LOGIC
		oProgress:WriteLine("Processing..."+cSource)
		oReader := System.IO.StreamReader{cSource}
		oWriter := System.IO.StreamWriter{cTarget}
		DO WHILE oReader:Peek() >= 0
			LOCAL cLine AS STRING
			cLine := oReader:ReadLine()
			IF cLine:Trim():StartsWith("Project(", StringComparison.OrdinalIgnoreCase) 
                // VULCANGUID == {5891B814-A2E0-4E64-9A2F-2C2ECAB940FE}
				// Project
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
    				IF aElements[2]:ToUpper() == "{5891B814-A2E0-4E64-9A2F-2C2ECAB940FE}"
						LOCAL cNewFile AS STRING
						LOCAL oPrjConverter AS ProjectConverter
						oPrjConverter := ProjectConverter{oProgress}
						aElements[2] := "{AA6C8D78-22FF-423A-9C7C-5F2393824E04}"	// XS Guid
						cNewFile := System.IO.Path.ChangeExtension(aElements[6], "."+EXTENSION)
						oPrjConverter:ConvertProjectFile(cPath+aElements[6], cPath+cNewFile)     
						aFiles:Add(cPath+cNewFile)
                        // add old file to the list too
                        IF SELF:lAdjustReferences
                            VAR cOldFile := System.IO.Path.GetFilename(aElements[6])
						    IF ! aGuids:ContainsKey(cOldFile)
							    aGuids:Add(cOldFile,System.IO.Path.GetFilename(cNewFile))   
						    ENDIF
                        ENDIF
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
                        aOthers:Add(cPath+aElements[6])
				    ENDIF
				ELSE
					// Something wrong  
					oProgress:WriteLine( "Incorrect line in Solution: "+ cLine)
				ENDIF
			ELSE
				FOREACH VAR item IN aGuids
                    lChanged := FALSE 
                    cLine := Replace(cLine , Item:Key, Item:Value, ref lChanged)
				NEXT				
			ENDIF
			oWriter:WriteLine(cLine)
		ENDDO    
		oReader:Close()
		oWriter:Close()
		// Now read the file again and replace all 'forward' references
		VAR sContents := System.IO.File.ReadAllText(cTarget)
		FOREACH VAR sItem IN aGuids
            sContents := Replace(sContents, sItem:Key, sItem:Value, ref lChanged)
		NEXT                    
		if lChanged
			System.IO.File.WriteAllText(cTarget, sContents)
		endif

		// Now update project references in all xsprj files
		FOREACH VAR sFile IN aFiles
			lChanged := FALSE 
			if System.IO.File.Exists(sFile)
				sContents := System.IO.File.ReadAllText(sFile)
				FOREACH VAR sItem IN aGuids
                    sContents := Replace(sContents, sItem:Key, sItem:Value, ref lChanged)
				NEXT                    
				IF lChanged
					System.IO.File.WriteAllText(sFile, sContents)
				ENDIF
			ENDIF
		NEXT
        if self:lAdjustReferences
		    FOREACH VAR sFile IN aOthers
			    lChanged := FALSE 
			    if System.IO.File.Exists(sFile)
				    sContents := System.IO.File.ReadAllText(sFile)
				    FOREACH VAR sItem IN aGuids
                        sContents := Replace(sContents, sItem:Key, sItem:Value, ref lChanged)
				    NEXT                    
				    IF lChanged
                        oProgress:WriteLine("Adjusted "+sFile)
                        System.IO.File.Copy(sFile, sFile+".BAK",true)
					    System.IO.File.WriteAllText(sFile, sContents)
				    ENDIF
			    ENDIF
		    NEXT
        endif
		RETURN TRUE		
		
STATIC METHOD Convert(cFile AS STRING, oProgress AS IProgress, lAdjustReferences as LOGIC) AS VOID
	LOCAL oSolutionConverter AS SolutionConverter
	oSolutionConverter := SolutionConverter{cFile, oProgress, lAdjustReferences}
	IF oSolutionConverter:Start()
		oProgress:WriteLine( "Created " +oSolutionConverter:Target)
		oProgress:WriteLine( "Done")
	ELSE                     
		oProgress:WriteLine( "Error converting "+cFile)
	ENDIF  


STATIC METHOD Replace(sSource as STRING, sKey as STRING, sValue as STRING, lChanged REF LOGIC) AS STRING
    sKey := sKey:ToLower()
    VAR pos := sSource:ToLower():IndexOf(sKey)
    DO WHILE pos >= 0
        VAR sLeft := sSource:Substring(0, pos)
        var sRight := sSource:Substring(pos+sKey:Length)
        sSource := sLeft + sValue+sRight
        lChanged := TRUE
        pos := sSource:ToLower():IndexOf(sKey)
    ENDDO
    RETURN sSource
		
		
END CLASS	


