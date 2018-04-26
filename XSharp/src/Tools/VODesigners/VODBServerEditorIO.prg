#using System.Collections
#using System.Collections.Generic
#using System.Windows.Forms
#using System.Drawing
#using System.IO
#using System.Text
#using System.Xml


PARTIAL CLASS VODBServerEditor INHERIT DesignerBase
	
	METHOD OpenVNdbs(cFileName AS STRING) AS LOGIC
	RETURN SELF:OpenVNdbs(File.Open(cFileName , FileMode.Open , FileAccess.Read) , cFileName:Substring(0 , cFileName:LastIndexOf('.')))
	METHOD OpenVNdbs(oStream AS FileStream , cBaseName AS STRING) AS LOGIC
		LOCAL oDesign , oOrder AS DBEDesignDBServer
		LOCAL oFieldSpec AS FSEDesignFieldSpec
		LOCAL oReader AS BinaryReader
		LOCAL cFieldSpec AS STRING
		LOCAL aOrders AS STRING[]
		LOCAL aFiles AS STRING[]
		LOCAL cBaseDir AS STRING
		LOCAL cValue AS STRING
		LOCAL n,m AS INT
		LOCAL aSorted AS SortedList
		LOCAL nPos AS INT

		LOCAL aMissingFieldSpecs AS List<STRING>
		aMissingFieldSpecs := List<STRING>{}

		aSorted := SortedList{}

		oReader := BinaryReader{oStream , System.Text.Encoding.GetEncoding(0)}
		oDesign := SELF:oMainDesign
		cValue := SELF:__ReadString(oReader , 128)
		SELF:oNameEdit:Text := cValue
		oDesign:GetProperty("classname"):Value := cValue
		oDesign:GetProperty("hlcaption"):Value := SELF:__ReadString(oReader , 64)
		oDesign:GetProperty("hldescription"):Value := SELF:__ReadString(oReader , 255)
		oDesign:GetProperty("hlhelpcontext"):Value := SELF:__ReadString(oReader , 64)
//		oDesign:GetProperty("share"):Value := iif(oReader:ReadUInt16() == 1 , 0 , 1)
		oDesign:GetProperty("share"):Value := 2 - (INT)oReader:ReadUInt16()
//		oDesign:GetProperty("ro"):Value := iif(oReader:ReadUInt16() == 1 , 0 , 1)
		oDesign:GetProperty("ro"):Value := 2 - (INT)oReader:ReadUInt16()
		oDesign:GetProperty("rdd"):Value := SELF:__ReadString(oReader , 260)
//		oDesign:GetProperty("dbfname"):Value := SELF:__ReadString(oReader , 260)
		cValue := SELF:__ReadString(oReader , 260)
		SELF:oFileNameEdit:Text := cValue
		oDesign:GetProperty("filename"):Value := cValue
		oDesign:GetProperty("noaccass"):Value := iif(oReader:ReadByte() >= 128 , 0 , 1)
		oReader:ReadUInt16()
		oDesign:GetProperty("superclass"):Value := SELF:__ReadString(oReader , 80)

		oReader:Close()

		cBaseDir := cBaseName:Substring(0 , cBaseName:LastIndexOf('\\'))
		cBaseName := cBaseName:Substring(cBaseName:LastIndexOf('\\') + 1)
		aFiles := Directory.GetFiles(cBaseDir , cBaseName + "_*.xsfld")
		FOR n := 1 UPTO aFiles:Length
//			MessageBox.Show(aFiles[n])
			SELF:aFilesToDelete:Add(aFiles[n])
			oReader := BinaryReader{File.Open(aFiles[n] , FileMode.Open , FileAccess.Read) , System.Text.Encoding.GetEncoding(0)}
			oDesign := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , NULL , DBServerItemType.Field})
			oDesign:GetProperty("fldname"):Value := SELF:__ReadString(oReader , 128)
			SELF:__ReadString(oReader , 128)
			SELF:__ReadString(oReader , 64)
			SELF:__ReadString(oReader , 255)
			SELF:__ReadString(oReader , 64)
			oDesign:GetProperty("included"):Value := (INT)oReader:ReadByte()
			nPos := (INT)oReader:ReadUInt16()
//			MessageBox.Show(nPos:ToString())
			DO WHILE aSorted:ContainsKey(nPos)
				nPos ++
			END DO
			oDesign:GetProperty("pos"):Value := nPos
			aSorted:Add(nPos , oDesign)

			cFieldSpec := SELF:__ReadString(oReader , 128):Trim()
			oDesign:GetProperty("classname"):Value := cFieldSpec
			oReader:ReadUInt32()
			oReader:ReadUInt32()
			oReader:ReadByte()
			oReader:ReadUInt16()
			oReader:Close()
			
			oFieldSpec := SELF:GetAvailableFieldSpec(cFieldSpec)
			IF oFieldSpec == NULL
				IF .not. aMissingFieldSpecs:Contains(cFieldSpec:ToUpper())
					Funcs.WarningBox("Could not find FieldSpec definition " + cFieldSpec)
					aMissingFieldSpecs:Add(cFieldSpec:ToUpper())
				END IF
			ELSE
				SELF:CopyPropertyValues(oFieldSpec , oDesign)
			END IF
		NEXT
		
		FOR n := 0 UPTO aSorted:Count - 1
			oDesign := (DBEDesignDBServer)aSorted:GetByIndex(n)
			SELF:oFieldList:Items:Add(oDesign:oItem)
			oDesign:oItem:SetValues()
		NEXT

		aFiles := Directory.GetFiles(cBaseDir , cBaseName + "_*.xsind")
		FOR n := 1 UPTO aFiles:Length
//			MessageBox.Show(aFiles[n])
			SELF:aFilesToDelete:Add(aFiles[n])
			oReader := BinaryReader{File.Open(aFiles[n] , FileMode.Open , FileAccess.Read) , System.Text.Encoding.GetEncoding(0)}
			oDesign := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , "-1" , DBServerItemType.Index})
			LOCAL cIndexName AS STRING
			cIndexName := SELF:__ReadString(oReader , 128):Trim()
			oDesign:GetProperty("name"):Value := cIndexName
			oDesign:GetProperty("filename"):Value := SELF:__ReadString(oReader , 260)
			oReader:ReadBytes(4)
			oReader:ReadBytes(2)
/*			MessageBox.Show(SELF:__ReadString(oReader , 256) , SELF:__ReadString(oReader , 256))
			MessageBox.Show(oReader:ReadUInt16():ToString() , oReader:ReadUInt16():ToString())*/
			oReader:Close()

			IF n == 1 .and. (INT)SELF:oMainDesign:GetProperty("order"):Value == 0
				SELF:oMainDesign:GetProperty("order"):Value := 1
			END IF
			
			LOCAL cBaseIndexName AS STRING
/*			cBaseName := aFiles[n]
			cBaseName := cBaseName:Substring(cBaseName:LastIndexOf('\\') + 1)
			cBaseName := cBaseName:Substring(0 , cBaseName:LastIndexOf('_'))*/
			cBaseIndexName := cBaseName + "_" + cIndexName
			oDesign:oItem:SetValues()
			aOrders := Directory.GetFiles(cBaseDir , cBaseIndexName + "_*.xsord")
//			MessageBox.Show(aOrders:Length:ToString())
			FOR m := 1 UPTO aOrders:Length
				SELF:aFilesToDelete:Add(aOrders[m])
				oReader := BinaryReader{File.Open(aOrders[m] , FileMode.Open , FileAccess.Read) , System.Text.Encoding.GetEncoding(0)}
				oOrder := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , "0" , DBServerItemType.Order})
				oOrder:GetProperty("tag"):Value := SELF:__ReadString(oReader , 128)
				oOrder:GetProperty("Duplicate"):Value := iif(oReader:ReadByte() == 1 , 0 , 1)
				oOrder:GetProperty("Ascending"):Value := iif(oReader:ReadByte() == 1 , 0 , 1)
				oOrder:GetProperty("KeyExp"):Value := SELF:__ReadString(oReader , 256)
				oOrder:GetProperty("ForExp"):Value := SELF:__ReadString(oReader , 256)
				oDesign:aOrders:Add(oOrder)
				oOrder:oItem:SetValues()
				oReader:Close()
			NEXT
			
		NEXT
		
//		SELF:DisplayProperties()
		SELF:ClearUndoBuffer()

	RETURN TRUE

	METHOD __ReadString(oReader AS BinaryReader , nLength AS INT) AS STRING
		LOCAL aBytes AS BYTE[]
		LOCAL cRet AS STRING
		LOCAL n AS INT
		aBytes := oReader:ReadBytes(nLength)
		n := 1
		DO WHILE n <= nLength
			IF aBytes[n] == 0
				EXIT
			ENDIF
			n ++
		END DO
		cRet := System.Text.Encoding.Default:GetString(aBytes , 0 , n - 1)
	RETURN cRet


	METHOD OpenXml(cFileName AS STRING) AS LOGIC
		LOCAL oDocument AS XmlDocument
		LOCAL oMainNode AS XmlNode
		LOCAL oXmlNode AS XmlNode
		LOCAL oSubNode AS XmlNode
		LOCAL cModule AS STRING
		LOCAL lSuccess AS LOGIC

		SELF:oSurface:Controls:Add(SELF:oPanel)
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		
		cModule := Funcs.GetModuleFilenameFromBinary(cFileName)

		oDocument := XmlDocument{}
		TRY
			oDocument:Load(cFileName)
			lSuccess := TRUE
		END TRY
		IF .not. lSuccess .or. oDocument:FirstChild == NULL
			RETURN FALSE
		END IF

		oMainNode := oDocument:FirstChild
		DO WHILE oMainNode != NULL
			IF oMainNode:Name:ToUpper() == "DESIGNERS"
				EXIT
			END IF
			oMainNode := oMainNode:NextSibling
		END DO
		IF oMainNode == NULL
			RETURN FALSE
		END IF

		oMainNode := oMainNode:FirstChild
		DO WHILE oMainNode != NULL
			IF oMainNode:Name:ToUpper() == "DBSERVER"
				EXIT
			END IF
			oMainNode := oMainNode:NextSibling
		END DO
		IF oMainNode == NULL
			RETURN FALSE
		END IF

		oXmlNode := oMainNode:FirstChild
		DO WHILE oXmlNode != NULL
			SWITCH oXmlNode:Name:ToUpper() 
			CASE "FIELDS"
				oSubNode := oXmlNode:FirstChild
				DO WHILE oSubNode != NULL
					SELF:OpenXmlField(oSubNode , cModule)
					oSubNode := oSubNode:NextSibling
				END DO
			CASE "INDEXES"
				oSubNode := oXmlNode:FirstChild
				DO WHILE oSubNode != NULL
					SELF:OpenXmlIndex(oSubNode)
					oSubNode := oSubNode:NextSibling
				END DO
			OTHERWISE
				Funcs.ReadXmlProperty(oXmlNode , SELF:oMainDesign)
			END SWITCH
			
			oXmlNode := oXmlNode:NextSibling
		END DO
		
		SELF:oNameEdit:Text := SELF:oMainDesign:GetProperty("classname"):TextValue
		SELF:oFileNameEdit:Text := SELF:oMainDesign:GetProperty("filename"):TextValue

		SELF:ClearUndoBuffer()

	RETURN TRUE
	
	METHOD OpenXmlField(oFieldNode AS XmlNode , cModule AS STRING) AS VOID
		LOCAL oFieldSpec AS FSEDesignFieldSpec
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL oSubNode AS XmlNode
		LOCAL n AS INT
		
		oDesign := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , NULL , DBServerItemType.Field})
		oSubNode := oFieldNode:FirstChild
		DO WHILE oSubNode != NULL
			DO CASE
			CASE oSubNode:Name:ToUpper() == "FIELDSPEC"
				oFieldSpec := SELF:GetAvailableFieldSpec(oSubNode:InnerText)
				IF oFieldSpec != NULL
					SELF:CopyPropertyValues(oFieldSpec , oDesign)
					IF .not. SELF:IsFieldSpecInModule(oSubNode:InnerText)
						FOR n := 6 UPTO oDesign:aProperties:Count - 1
							oDesign:GetProperty(n):lReadOnly := TRUE
						NEXT
					END IF
				END IF
/*				LOCAL oFSNode AS XmlNode
				LOCAL cFSFileName AS STRING
				oFSNode := FindFieldSpecInXmlFiles(cModule , oSubNode:InnerText)
				TRY
					IF oFSNode != NULL
						VOFieldSpecEditor.OpenXml(oFSNode , oDesign)
//						MessageBox.Show("Read fieldspec from xml: " + oDesign:Name , "DBServer")
					ELSE
						cFSFileName := FindFieldSpecInVnfsFiles(cModule , oSubNode:InnerText)
						IF cFSFileName != NULL
							VOFieldSpecEditor.OpenVNfs(cFSFileName , oDesign)
							oDesign:GetProperty("classname"):Value := oSubNode:InnerText
						END IF
					END IF
				END TRY*/
			OTHERWISE
				Funcs.ReadXmlProperty(oSubNode , oDesign)
			END CASE
			oSubNode := oSubNode:NextSibling
		END DO
		SELF:oFieldList:Items:Add(oDesign:oItem)
		oDesign:oItem:SetValues()
	RETURN
	METHOD OpenXmlIndex(oIndexNode AS XmlNode) AS VOID
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL oSubNode AS XmlNode
		LOCAL oOrderNode AS XmlNode
		oDesign := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , "-1" , DBServerItemType.Index})
		oSubNode := oIndexNode:FirstChild
		DO WHILE oSubNode != NULL
			DO CASE
			CASE oSubNode:Name:ToUpper() == "ORDERS"
				oOrderNode := oSubNode:FirstChild
				DO WHILE oOrderNode != NULL
					SELF:OpenXmlOrder(oOrderNode , oDesign)
					oOrderNode := oOrderNode:NextSibling
				END DO
			OTHERWISE
				Funcs.ReadXmlProperty(oSubNode , oDesign)
			END CASE
			oSubNode := oSubNode:NextSibling
		END DO
		oDesign:oItem:SetValues()
	RETURN
	METHOD OpenXmlOrder(oOrderNode AS XmlNode , oIndex AS DBEDesignDBServer) AS VOID
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL oSubNode AS XmlNode
		oDesign := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , "0" , DBServerItemType.Order})
		oSubNode := oOrderNode:FirstChild
		DO WHILE oSubNode != NULL
			Funcs.ReadXmlProperty(oSubNode , oDesign)
			oSubNode := oSubNode:NextSibling
		END DO
		oIndex:aOrders:Add(oDesign)
		oDesign:oItem:SetValues()
	RETURN
/*	
	STATIC METHOD FindFieldSpecInXmlFiles_unused(cModule AS STRING , cName AS STRING) AS XmlNode
		LOCAL oXmlNode AS XmlNode
		LOCAL cBaseDir AS STRING
		LOCAL aFiles AS STRING[]
		LOCAL n AS INT

		cName := cName:ToUpper()
		IF File.Exists(cModule + ".FieldSpecs.xsfs")
			oXmlNode := FindFieldSpecInXmlFile(cModule + ".FieldSpecs.xsfs" , cName)
			IF oXmlNode != NULL
				RETURN oXmlNode
			END IF
		END IF

		cBaseDir := FileInfo{cModule}:Directory:FullName
		aFiles := Directory.GetFiles(cBaseDir , "*.FieldSpecs.xsfs")
		FOR n := 1 UPTO aFiles:Length
			oXmlNode := FindFieldSpecInXmlFile(aFiles[n] , cName)
			IF oXmlNode != NULL
				RETURN oXmlNode
			END IF
		NEXT
//		aFiles := Directory.GetDirectories(cBaseDir)
//		FOR n := 1 UPTO aFiles:Length
//			oXmlNode := FindFieldSpecInXmlFiles(aFiles[n] , cName)
//			IF oXmlNode != NULL
//				RETURN oXmlNode
//			END IF
//		NEXT
	RETURN NULL
	STATIC METHOD FindFieldSpecInXmlFile_unused(cFileName AS STRING , cName AS STRING) AS XmlNode
		LOCAL oDocument AS XmlDocument
		LOCAL oXmlNode AS XmlNode
		LOCAL lSuccess AS LOGIC
		
		cName := cName:ToUpper()
		TRY
			lSuccess := FALSE
			oDocument := XmlDocument{}
			oDocument:Load(cFileName)
			lSuccess := TRUE
		END TRY
		IF !lSuccess
			RETURN NULL
		END IF
		
		oXmlNode := oDocument:FirstChild
		DO WHILE oXmlNode != NULL
			IF oXmlNode:Name:ToUpper() == "DESIGNERS"
				EXIT
			END IF
			oXmlNode := oXmlNode:NextSibling
		END DO
		IF oXmlNode != NULL
			oXmlNode := oXmlNode:FirstChild
			DO WHILE oXmlNode != NULL
				IF oXmlNode:Name:ToUpper() == "FIELDSPEC" .and. ;
							oXmlNode:Attributes:Count >= 1 .and. oXmlNode:Attributes[0]:Name:ToUpper() == "NAME" .and. oXmlNode:Attributes[0]:Value:ToUpper() == cName
					RETURN oXmlNode
				END IF
				oXmlNode := oXmlNode:NextSibling
			END DO
		END IF
	RETURN NULL

	STATIC METHOD FindFieldSpecInVnfsFiles_unused(cModule AS STRING , cName AS STRING) AS STRING
//		LOCAL cFileName AS STRING
		LOCAL cBaseDir AS STRING
		LOCAL aFiles AS STRING[]
//		LOCAL n AS INT
		
		cBaseDir := FileInfo{cModule}:Directory:FullName

		cName := cName:ToUpper()
		aFiles := Directory.GetFiles(cBaseDir , "*." + cName + ".xsfs")
		IF aFiles:Length == 1
			RETURN aFiles[1]
		END IF
//		aFiles := Directory.GetDirectories(cBaseDir)
//		FOR n := 1 UPTO aFiles:Length
//			cFileName := FindFieldSpecInVnfsFiles(aFiles[n] , cName)
//			IF cFileName != NULL
//				RETURN cFileName
//			END IF
//		NEXT
	RETURN NULL
*/	

	METHOD SaveToXml(cFileName AS STRING) AS LOGIC
		LOCAL oDocument AS XmlDocument
		LOCAL oDesigners AS XmlNode
		LOCAL oMainNode AS XmlNode
		LOCAL oXmlNode AS XmlNode
		LOCAL oSubNode AS XmlNode
		LOCAL oIndexNode,oOrderNode AS XmlNode
		LOCAL oDesign,oIndex,oOrder AS DBEDesignDBServer
		LOCAL aDesign AS ArrayList
		LOCAL n,m,k,l AS INT
		
		oDocument := XmlDocument{}
		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))

		oDesigners := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oDesigners)

//		oMainNode := oDocument:CreateElement(SELF:oMainDesign:Name)
		oMainNode := oDocument:CreateElement("DBServer")
		Funcs.ApplyNameAttribute(oDocument , oMainNode , SELF:oMainDesign:Name)
		oDesigners:AppendChild(oMainNode)
//		oXmlNode := oDocument:CreateElement("Properties")
//		oMainNode:AppendChild(oXmlNode)
		oDesign := SELF:oMainDesign
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
//			Funcs.AppendProperty(oDocument , oXmlNode , (DesignProperty)oDesign:aProperties[n])
			Funcs.AppendProperty(oDocument , oMainNode , (DesignProperty)oDesign:aProperties[n])
		NEXT

		oXmlNode := oDocument:CreateElement("Fields")
		oMainNode:AppendChild(oXmlNode)
		aDesign := SELF:GetAllDesignItems(DBServerItemType.Field)
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[n]
//			oSubNode := oDocument:CreateElement(oDesign:Name)
			oSubNode := oDocument:CreateElement("Field")
			Funcs.ApplyNameAttribute(oDocument , oSubNode , oDesign:Name)
			oXmlNode:AppendChild(oSubNode)
			Funcs.AppendProperty(oDocument , oSubNode , oDesign:GetProperty("fldname"))
			Funcs.AppendProperty(oDocument , oSubNode , oDesign:GetProperty("pos"))
			Funcs.AppendProperty(oDocument , oSubNode , oDesign:GetProperty("included"))
			Funcs.AppendProperty(oDocument , oSubNode , oDesign:GetProperty("caption"))
			Funcs.AppendProperty(oDocument , oSubNode , oDesign:GetProperty("description"))
			Funcs.AppendElement(oDocument , oSubNode , "FieldSpec" , oDesign:GetProperty("classname"):TextValue)
		NEXT


		oXmlNode := oDocument:CreateElement("Indexes")
		oMainNode:AppendChild(oXmlNode)
		aDesign := SELF:GetAllDesignItems(DBServerItemType.Index)
		FOR n := 0 UPTO aDesign:Count - 1
			oIndex := (DBEDesignDBServer)aDesign[n]
//			oSubNode := oDocument:CreateElement(oIndex:oItem:Text)
			oSubNode := oDocument:CreateElement("Index")
			Funcs.ApplyNameAttribute(oDocument , oSubNode , oIndex:Name)
			FOR m := 0 UPTO oIndex:aProperties:Count - 1
				Funcs.AppendProperty(oDocument , oSubNode , (DesignProperty)oIndex:aProperties[m])
			NEXT
			oIndexNode := oDocument:CreateElement("Orders")
			oSubNode:AppendChild(oIndexNode)
			FOR k := 0 UPTO oIndex:aOrders:Count - 1
				oOrder := (DBEDesignDBServer)oIndex:aOrders[k]
//				oOrderNode := oDocument:CreateElement(oOrder:GetProperty("tag"):TextValue)
				oOrderNode := oDocument:CreateElement("Order")
				Funcs.ApplyNameAttribute(oDocument , oOrderNode , oOrder:Name)
				oIndexNode:AppendChild(oOrderNode)
				FOR l := 0 UPTO oOrder:aProperties:Count - 1
					Funcs.AppendProperty(oDocument , oOrderNode , (DesignProperty)oOrder:aProperties[l])
				NEXT
			NEXT
			oXmlNode:AppendChild(oSubNode)
		NEXT

		oDocument:Save(cFileName)
	RETURN TRUE

	METHOD SavePrg(oStream AS EditorStream , oCode AS CodeContents , aFieldSpecs AS ArrayList) AS LOGIC
		LOCAL aAdditional AS List<STRING>
		LOCAL oTempEditor AS CodeEditor
		LOCAL oEntity AS ParseInfo
		LOCAL cName AS STRING
		LOCAL n AS INT
		
		cName := SELF:oMainDesign:GetProperty("classname"):TextValue
		oStream:Editor:ReplaceEntity(cName , cName , EntityType._Class , oCode:aClass)
		oStream:Editor:ReplaceEntity(cName , cName , EntityType._Constructor , oCode:aConstructor)
		oStream:Editor:ReplaceEntity("FIELDDESC" , cName , EntityType._Access , oCode:aFieldDesc)
		oStream:Editor:ReplaceEntity("INDEXLIST" , cName , EntityType._Access , oCode:aIndexList)

		FOR n := 0 UPTO oCode:aAdditional:Count - 1
			aAdditional := oCode:aAdditional[n]
			oTempEditor := CodeEditor{aAdditional}
			oEntity := oTempEditor:GetFirstEntity()
			IF oEntity != NULL
				oStream:Editor:ReplaceEntity(oEntity:cName , cName , oEntity:eType , aAdditional)
			END IF
		NEXT
		
		SELF:SaveAccessAssign(oStream:Editor , cName , SELF:oMainDesign:GetProperty("noaccass"):TextValue:ToUpper() == "YES")
		
		LOCAL oFieldSpec AS FSEDesignFieldSpec
		FOR n := 0 UPTO aFieldSpecs:Count - 1
			oFieldSpec := (FSEDesignFieldSpec)aFieldSpecs[n]
			oCode := VOFieldSpecEditor.GetCodeContents(oFieldSpec)
			cName := oFieldSpec:GetProperty("classname"):TextValue
			oStream:Editor:ReplaceEntity(cName , cName , EntityType._Class , oCode:aClass)
			oStream:Editor:ReplaceEntity(cName , cName , EntityType._Constructor , oCode:aConstructor)
		NEXT
		
		oStream:Save()
		
	RETURN TRUE

	METHOD SaveAccessAssign(oEditor AS CodeEditor , cClass AS STRING , lNoAccAss AS LOGIC) AS VOID
		LOCAL aValues AS NameValueCollection
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL oTempEditor AS CodeEditor
		LOCAL aTempEntity AS List<STRING>
		LOCAL aEntity AS List<STRING>
		LOCAL oProp AS DesignProperty
		LOCAL aDesign AS ArrayList
		LOCAL oEntity AS ParseInfo
		LOCAL cValue AS STRING
		LOCAL cLine AS STRING
		LOCAL n,m,k AS INT

		aEntity := List<STRING>{}
		aDesign := SELF:GetAllDesignItems(DBServerItemType.Field)
		FOR m := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[m]
			aValues := NameValueCollection{}
			FOR n := 0 UPTO oDesign:aProperties:Count - 1
				oProp := (DesignProperty)oDesign:aProperties[n]
				DO CASE
				CASE oProp:Name == "hlname"
					// it appears that %hlname% tag is translated to %fldname% in VO
					cValue := oDesign:GetProperty("fldname"):TextValue // BIG BAD UGLY HACK
				CASE oProp:cEnumType == "YESNO"
					cValue := iif(oProp:ValueLogic , "TRUE" , "FALSE")
				CASE oProp:Name == "type"
					IF (INT)oProp:Value == 6
						cValue := "X"
					ELSE
						cValue := oProp:TextValue:Substring(0,1)
					END IF
				OTHERWISE
					cValue := oProp:TextValue
				END CASE
				aValues:Add(oProp:Name , cValue)
			NEXT
			
			cValue := oDesign:GetProperty("Type"):TextValue:ToUpper()
			SWITCH cValue
			case "CHARACTER" 
            CASE "MEMO"
				cValue := "STRING"
			CASE "NUMERIC"
				cValue := "FLOAT"
			OTHERWISE
				cValue := "USUAL"
			END SWITCH
			aValues:Add("usualtype" , cValue)
			
			FOR k := 0 UPTO VODBServerEditor.Template:aAccessAssign:Count - 1
				aTempEntity := VODBServerEditor.Template:aAccessAssign[k]
				aEntity:Clear()
				FOR n := 0 UPTO aTempEntity:Count - 1
					cLine := aTempEntity[n]
					cLine := TranslateLine(cLine , aValues)
					aEntity:Add(cLine)
				NEXT
				oTempEditor := CodeEditor{aEntity}
				oEntity := oTempEditor:GetFirstEntity()
				IF oEntity != NULL
					IF lNoAccAss .or. oDesign:GetProperty("included"):TextValue == "0"
						oEditor:DeleteEntity(oEntity:cName , cClass , oEntity:eType)
					ELSE
						oEditor:ReplaceEntity(oEntity:cName , cClass , oEntity:eType , aEntity)
					ENDIF
				END IF
			NEXT
		NEXT
		
	RETURN

END CLASS

