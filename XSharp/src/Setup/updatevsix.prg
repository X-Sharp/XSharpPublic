//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.XML

FUNCTION Start (args AS STRING[]) AS VOID	
	LOCAL oDoc AS XMLDocument                                 
	LOCAL lChanged := FALSE AS LOGIC
	IF args:Length != 2
		Console.WriteLine("2 arguments expected")
		Console.ReadLine()
		RETURN
	ENDIF
	LOCAL cVersion AS STRING
	LOCAL cFile AS STRING
	cVersion := args[1]
	cFile 	 := args[2]
	IF ! SYstem.IO.File.Exists(cFile)
		Console.WriteLine("Cannot find file: "+cFile)
		Console.ReadLine()
		RETURN
	ENDIF
	IF cVersion:Length != 7
		Console.WriteLine("Expected version in the form of n.n.n.n")
		Console.ReadLine()
		RETURN
	ENDIF
	LOCAL oReader AS System.IO.TextReader
	oReader := System.IO.StreamReader{cFile,TRUE}
    oDoc := System.Xml.XmlDocument{}
    oDoc:Load(oReader)
	oReader:Close()
	VAR oMan := oDoc:FirstChild  ASTYPE XMLElement
	IF oMan:Name:ToUpper() == "PACKAGEMANIFEST"
		VAR oMeta := oMan:FirstChild ASTYPE XMLElement
		IF oMeta:Name:ToUpper() == "METADATA" 
			VAR oId := oMeta:FirstChild ASTYPE XMLElement
			IF oId:Name:ToUpper() == "IDENTITY"
				FOREACH  oAtt AS XmlAttribute IN oID:Attributes
					IF oAtt:Name:ToUpper() == "VERSION" 
						oAtt:Value:= cVersion
						lChanged := TRUE
					ENDIF
				NEXT
			ENDIF
		ENDIF
	ENDIF                                
	
	// Update file
    VAR cString := oDoc:OuterXml
    VAR oStringWriter := System.IO.StringWriter{}
    VAR oWriter     := System.Xml.XmlTextWriter{oStringWriter}
    oWriter:Formatting := System.Xml.Formatting.Indented
    oDoc:WriteTo(oWriter)
	cString := oStringWriter:ToString()
	System.IO.File.WriteAllText(cFile, cString)
	IF lChanged
		Console.ForegroundColor := ConsoleColor.Yellow
		Console.WriteLine("Updated version info for file: "+cFile)
		System.Threading.Thread.Sleep(500)
	ELSE                  
		Console.ForegroundColor := ConsoleColor.Red                                        
		Console.WriteLine("FAILED to update version info for file: "+cFile)
		COnsole.ReadLine()
	ENDIF
	
	RETURN
	
