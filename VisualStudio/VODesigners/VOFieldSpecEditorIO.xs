//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Windows.Forms
using System.Drawing
using System.Collections
using System.Collections.Generic
using XSharp.VOEditors
using System.Xml

using System.IO
using System.Text


PARTIAL CLASS VOFieldSpecEditor INHERIT DesignerBase
/*
	STATIC METHOD ReadFieldSpecsInModule_unused(cModule AS STRING) AS ArrayList
		IF File.Exists(cModule + ".fieldSpecs.vnfs")
			RETURN OpenXml(cModule + ".fieldSpecs.vnfs" , NULL)
		ELSE
			RETURN ReadVnfsFilesInModule(cModule , NULL)
		END IF
	//RETURN NULL
*/
/*	STATIC METHOD ReadVnfsFilesInModule_unused(cModule AS STRING , oDesigner AS VOFieldSpecEditor) AS ArrayList
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL aFieldSpecs AS ArrayList
		LOCAL cDirectory AS STRING
		LOCAL aFiles AS STRING[]
		LOCAL n AS INT
		
		aFieldSpecs := ArrayList{}
		cDirectory := FileInfo{cModule}:DirectoryName
		cModule := FileInfo{cModule}:Name
		
		aFiles := Directory.GetFiles(cDirectory , cModule + "*.vnfs")
		FOR n := 1 UPTO aFiles:Length
			oDesign := NULL
			TRY
				IF .not. aFiles[n]:ToUpper():Contains("FIELDSPECS.VNFS")
					oDesign := OpenVNfs(aFiles[n] , oDesigner)
				END IF
			END TRY
			IF oDesign != NULL
				aFieldSpecs:Add(oDesign)
			END IF
		NEXT
	RETURN aFieldSpecs
*/
	STATIC METHOD OpenVNfs(cFileName AS STRING , oDesigner AS VOFieldSpecEditor) AS FSEDesignFieldSpec
		LOCAL oItem AS FSEDesignListViewItem
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL cName AS STRING
		LOCAL nPos AS INT

		oItem := FSEDesignListViewItem{oDesigner}
		oDesign := oItem:oDesign

		cName := cFileName
		nPos := cName:LastIndexOf(".")
		IF nPos == - 1
			RETURN NULL
		ELSE
			cName := cName:Substring(0 , nPos)
			nPos := cName:LastIndexOf('.')
			IF nPos == -1
				RETURN NULL
			ELSE
				cName := cName:Substring(nPos + 1)
			END IF
		END IF

		TRY
			OpenVNfs(cFileName , oDesign)
			oDesign:lModified := FALSE
			oDesign:cVNfsFileName := cFileName
			
			// Hack to properly capitalize fieldspec name, which is extracted in lower case from the filename
			IF oDesign:GetProperty("HLName"):TextValue:ToUpper() == cName:ToUpper()
				cName := oDesign:GetProperty("HLName"):TextValue
			ELSEIF oDesign:GetProperty("HLHelpContext"):TextValue:ToUpper() == cName:ToUpper()
				cName := oDesign:GetProperty("HLHelpContext"):TextValue
			END IF
			oDesign:GetProperty("classname"):Value := cName

		CATCH e AS Exception
            System.Diagnostics.Debug.WriteLine(e:Message)

			oDesign := NULL

		END TRY

	RETURN oDesign

	STATIC METHOD OpenVNfs(cFileName AS STRING , oDesign AS DesignItem) AS LOGIC
		LOCAL oReader AS BinaryReader
		LOCAL oStream AS FileStream
		LOCAL aBytes AS BYTE[]
		LOCAL cValue AS STRING
		LOCAL nPos AS INT

        oStream := File.Open(cFileName , FileMode.Open , FileAccess.Read)
		oReader := BinaryReader{oStream , System.Text.Encoding.GetEncoding(0)}
		aBytes := oReader:ReadBytes((Int32)oReader:BaseStream:Length)
		oReader:Close()

		nPos := 0
		cValue := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("HLName"):Value := cValue
		cValue := __ReadNextVNFsString(aBytes , nPos , 64)
		oDesign:GetProperty("HLCaption"):Value := cValue
		cValue := __ReadNextVNFsString(aBytes , nPos , 255)
		oDesign:GetProperty("HLDescription"):Value := cValue
		cValue := __ReadNextVNFsString(aBytes , nPos , 64)
		oDesign:GetProperty("HLHelpContext"):Value := cValue

		cValue := __ReadNextVNFsString(aBytes , nPos , 1)
		SWITCH cValue
		CASE  "C"
			oDesign:GetProperty("Type"):Value := 0
		CASE "N"
			oDesign:GetProperty("Type"):Value := 1
		CASE "D"
			oDesign:GetProperty("Type"):Value := 2
		CASE "L"
			oDesign:GetProperty("Type"):Value := 3
		CASE "M"
			oDesign:GetProperty("Type"):Value := 4
		CASE "O"
			oDesign:GetProperty("Type"):Value := 5
		CASE "X"
			oDesign:GetProperty("Type"):Value := 6
		END SWITCH
		
		oDesign:GetProperty("TypeDiag"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("TypeHelp"):Value := __ReadNextVNFsString(aBytes , nPos , 64)

		oDesign:GetProperty("Len"):Value := __ReadNextVNFsInt16(aBytes , nPos)
		oDesign:GetProperty("LenDiag"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("LenHelp"):Value := __ReadNextVNFsString(aBytes , nPos , 64)

		oDesign:GetProperty("Dec"):Value := __ReadNextVNFsInt16(aBytes , nPos)

		oDesign:GetProperty("Required"):Value := iif(__ReadNextVNFsString(aBytes , nPos , 1) == "Y" , 0 , 1)
		oDesign:GetProperty("ReqDiag"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("ReqHelp"):Value := __ReadNextVNFsString(aBytes , nPos , 64)

		oDesign:GetProperty("MinLen"):Value := __ReadNextVNFsInt16(aBytes , nPos)
		oDesign:GetProperty("MinLenDiag"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("MinLenHelp"):Value := __ReadNextVNFsString(aBytes , nPos , 64)

		oDesign:GetProperty("MinRange"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("MaxRange"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("RangeDiag"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("RangeHelp"):Value := __ReadNextVNFsString(aBytes , nPos , 64)
		
		oDesign:GetProperty("Validation"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("ValidDiag"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		oDesign:GetProperty("ValidHelp"):Value := __ReadNextVNFsString(aBytes , nPos , 128)

		oDesign:GetProperty("Picture"):Value := __ReadNextVNFsString(aBytes , nPos , 128)
		
		nPos += 4
		TRY
			oDesign:GetProperty("superclass"):Value := __ReadNextVNFsString(aBytes , nPos , 80)
		END TRY

	RETURN TRUE

	STATIC METHOD OpenXml(cFileName AS STRING , oDesigner AS VOFieldSpecEditor) AS ArrayList
		LOCAL oItem AS FSEDesignListViewItem
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL oNode , oFSNode AS XmlNode
		LOCAL oDocument AS XmlDocument
		LOCAL aDesign AS ArrayList
		LOCAL lSuccess := FALSE AS LOGIC
		
		aDesign := ArrayList{}

		IF ! File.Exists(cFileName)
			RETURN aDesign
		END IF
		oDocument := XmlDocument{}
		TRY
			oDocument:Load(cFileName)
			lSuccess := TRUE
		END TRY
		IF .not. lSuccess
			RETURN aDesign
		END IF
		
		oNode := oDocument:FirstChild
		DO WHILE oNode != NULL
			IF oNode:Name:ToUpper() == "DESIGNERS"
				EXIT
			END IF
			oNode := oNode:NextSibling
		END DO
		IF oNode == NULL
			RETURN aDesign
		END IF

		oFSNode := oNode:FirstChild

		IF oFSNode:ChildNodes:Count == 0 .and. oNode:ChildNodes:Count == 1 // empty template
			aDesign:Add(NULL)
			RETURN aDesign
		END IF

		DO WHILE oFSNode != NULL
			IF oFSNode:Name:ToUpper() == "FIELDSPEC"
				oItem := FSEDesignListViewItem{oDesigner}
				oDesign := oItem:oDesign
				TRY
					OpenXml(oFSNode , oDesign)
					oDesign:lModified := FALSE
					oDesign:cVNfsFileName := cFileName
					aDesign:Add(oDesign)
				END TRY
			END IF
			oFSNode := oFSNode:NextSibling
		END DO

	RETURN aDesign

	STATIC METHOD OpenXml(oFSNode AS XmlNode , oDesign AS DesignItem) AS VOID
		LOCAL oProp AS DesignProperty
		LOCAL oNode AS XmlNode
		oNode := oFSNode:FirstChild
		DO WHILE oNode != NULL
			oProp := oDesign:GetProperty(oNode:Name)
			IF oProp != NULL
				DO CASE
				CASE oProp:Type == PropertyType.Numeric
					oProp:Value := Int32.Parse(oNode:InnerText)
				OTHERWISE
					oProp:Value := oNode:InnerText
				END CASE
			END IF
			oNode := oNode:NextSibling
		END DO
	RETURN

	STATIC METHOD SaveToXml(cFileName AS STRING , aDesign AS ArrayList) AS LOGIC
		LOCAL oDocument AS XmlDocument
		LOCAL oElement AS XmlElement

		oDocument := XmlDocument{}
		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))
		oElement := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oElement)

		SaveToXml(oDocument , oElement , aDesign)

		oDocument:Save(cFileName)
	RETURN TRUE

	STATIC METHOD SaveToXml(oDocument AS XmlDocument , oDesigners AS XmlNode , aDesign AS ArrayList) AS LOGIC
		LOCAL n AS INT
		FOR n := 0 UPTO aDesign:Count - 1
			SaveToXml(oDocument , oDesigners , (FSEDesignFieldSpec)aDesign[n])
		NEXT
	RETURN TRUE

	STATIC METHOD SaveToXml(oDocument AS XmlDocument , oXmlNode AS XmlNode , oDesign AS FSEDesignFieldSpec) AS LOGIC
		LOCAL oElement AS XmlElement
		LOCAL n AS INT
		oElement := oDocument:CreateElement("FieldSpec")
		Funcs.ApplyNameAttribute(oDocument , oElement , oDesign:Name)
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			Funcs.AppendProperty(oDocument , oElement , (DesignProperty)oDesign:aProperties[n])
		NEXT
		oXmlNode:AppendChild(oElement)
	RETURN TRUE
	
	METHOD SavePrg(oStream AS EditorStream , aDesign AS ArrayList) AS LOGIC
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL aPrevNames AS List<STRING>
		LOCAL oCode AS CodeContents
		LOCAL cName AS STRING
		LOCAL n AS INT
		
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (FSEDesignFieldSpec)aDesign[n]
			oCode := GetCodeContents(oDesign)
			cName := oDesign:GetProperty("classname"):TextValue
			oStream:CodeManager:ReplaceEntity(cName , cName , EntityType._Class , oCode:aClass)
			oStream:CodeManager:ReplaceEntity(cName , cName , EntityType._Constructor , oCode:aConstructor)
		NEXT
		
		aPrevNames := List<STRING>{}
		FOR n := 0 UPTO SELF:aUsedFieldSpecNames:Count - 1
			aPrevNames:Add(SELF:aUsedFieldSpecNames[n])
		NEXT
		SELF:LoadUsedFieldSpecNames()
		FOR n := 0 UPTO aPrevNames:Count - 1
			cName := aPrevNames[n]
			IF .not. SELF:aUsedFieldSpecNames:Contains(cName)
				oStream:CodeManager:DeleteClass(cName)
			END IF
		NEXT
		
		oStream:Save()
	RETURN TRUE

END CLASS
