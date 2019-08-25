#using System.Collections
#using System.Collections.Generic
#using System.Windows.Forms
#using System.Drawing
#using System.Text
#using System.IO
#using System.Xml


PARTIAL CLASS VOMenuEditor INHERIT DesignerBase

	METHOD OpenVNmnu(cFileName AS STRING) AS LOGIC
		SELF:Surface:Controls:Add(SELF:oTree)
		SELF:oTree:SelectedNode := SELF:oMainNode
		SELF:oMainNode:Nodes:Clear()
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		
		IF SELF:OpenXml(cFileName)
			RETURN TRUE
		END IF

	RETURN SELF:OpenVNmnu(File.Open(cFileName , FileMode.Open , FileAccess.Read))

	PROTECTED METHOD OpenVNmnu(oStream AS FileStream) AS LOGIC
		LOCAL oDescr AS VOMenuDescription
		LOCAL oReader AS BinaryReader
		LOCAL aBytes AS BYTE[]

		oReader := BinaryReader{oStream , System.Text.Encoding.GetEncoding(0)}
		aBytes := oReader:ReadBytes((Int32)oReader:BaseStream:Length)
		oReader:Close()
		
		oDescr := OpenVNmnu(aBytes)
		SELF:AddMenuNode(SELF:oMainNode , oDescr:oMainItem)

		SELF:oMainNode:ExpandAll()
		SELF:DisplayProperties()
	RETURN TRUE

	PROTECTED METHOD AddMenuNode(oParent AS DesignTreeNode , oItem AS VOMenuItem) AS VOID
		LOCAL oProp AS VODesignProperty
		LOCAL oDesign AS DesignMenuItem
		LOCAL oNode AS DesignTreeNode
		LOCAL cProp AS STRING
		LOCAL n AS INT
		
		oDesign := oParent:oDesign
		FOR n := 0 UPTO oItem:aProperties:Count - 1
			cProp := oItem:aProperties:GetKey(n)
			oProp := oDesign:GetProperty(cProp)
			IF oProp != NULL
				oProp:Value := oItem:aProperties[n]
			END IF
		NEXT
		
		IF oParent == SELF:oMainNode
			oParent:Text := oDesign:GetProperty("Name"):TextValue
		ELSE
			oDesign:GetProperty("MenuId"):Value := 0 // so that it will be recalculated
			oParent:Text := Funcs.TranslateCaption( oDesign:GetProperty("Caption"):TextValue , FALSE )
			SELF:oTree:SetNodeColor(oParent)
		END IF
		
		FOR n := 0 UPTO oItem:aSubItems:Count - 1
			oNode := DesignTreeNode{0 , SELF}
			oParent:Nodes:Add(oNode)
			SELF:AddMenuNode(oNode , (VOMenuItem)oItem:aSubItems[n])
		NEXT
	RETURN


	PROTECTED METHOD OpenXml(cFileName AS STRING) AS LOGIC
		LOCAL oNode , oMenuNode AS XmlNode
		LOCAL oDocument AS XmlDocument
		LOCAL lSuccess AS LOGIC
		
		IF ! File.Exists(cFileName)
			RETURN FALSE
		END IF
		oDocument := XmlDocument{}
		TRY
			oDocument:Load(cFileName)
			lSuccess := TRUE
		END TRY
		IF .not. lSuccess
			RETURN FALSE
		END IF
		
		oNode := oDocument:FirstChild
		DO WHILE oNode != NULL
			IF oNode:Name:ToUpper() == "DESIGNERS"
				EXIT
			END IF
			oNode := oNode:NextSibling
		END DO
		IF oNode == NULL
			RETURN FALSE
		END IF

		oMenuNode := oNode:FirstChild
		DO WHILE oMenuNode != NULL
			IF oMenuNode:Name:ToUpper() == "VOMENU"
				SELF:OpenXml(oMenuNode , SELF:oMainNode:oDesign)
				SELF:oMainNode:Text := SELF:oMainNode:oDesign:GetProperty("Name"):TextValue
				SELF:oMainNode:ExpandAll()
				RETURN TRUE
			END IF
			oMenuNode := oMenuNode:NextSibling
		END DO

	RETURN FALSE
	PROTECTED METHOD OpenXml(oParentNode AS XmlNode , oParent AS DesignMenuItem) AS VOID
		LOCAL oXmlNode , oSubNode AS XmlNode
		LOCAL oNode AS DesignTreeNode
		
		IF oParentNode == NULL
			RETURN
		END IF
		oXmlNode := oParentNode:FirstChild
		DO WHILE oXmlNode != NULL
			IF oXmlNode:Name:ToUpper() == "MENUITEMS"
				oSubNode := oXmlNode:FirstChild
				DO WHILE oSubNode != NULL
					IF oSubNode:Name:ToUpper() == "MENUITEM"
						oNode := DesignTreeNode{0 , SELF}
						oParent:oNode:Nodes:Add(oNode)
						SELF:OpenXml(oSubNode , oNode:oDesign)
						oNode:Text := Funcs.TranslateCaption( oNode:oDesign:GetProperty("Caption"):TextValue , FALSE )
						SELF:oTree:SetNodeColor(oNode)
					END IF
					oSubNode := oSubNode:NextSibling
				END DO
			ELSE
				Funcs.ReadXmlProperty(oXmlNode , oParent)
			END IF
			oXmlNode := oXmlNode:NextSibling
		END DO
	RETURN

	METHOD GetMenuDescription() AS VOMenuDescription
		LOCAL oDescr AS VOMenuDescription
		oDescr := VOMenuDescription{}
		oDescr:cName := SELF:oMainNode:oDesign:Name
		SELF:GetMenuDescription(oDescr:oMainItem , SELF:oMainNode)
	RETURN oDescr
	METHOD GetMenuDescription(oParent AS VOMenuItem , oNode AS DesignTreeNode) AS VOID
		LOCAL oDesign AS DesignMenuItem
		LOCAL oProp AS VODesignProperty
		LOCAL oItem AS VOMenuItem
		LOCAL n AS INT
		oDesign := oNode:oDesign
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			oProp := (VODesignProperty)oDesign:aProperties[n]
			oParent:aProperties:Add(oProp:Name , oProp:TextValue)
		NEXT
		FOR n := 0 UPTO oNode:Nodes:Count - 1
			oItem := VOMenuItem{}
			oParent:aSubItems:Add(oItem)
			SELF:GetMenuDescription(oItem , (DesignTreeNode)oNode:Nodes[n])
		NEXT
	RETURN
	

	METHOD SaveToXml(oStream AS FileStream) AS LOGIC
		LOCAL oDocument AS XmlDocument
		LOCAL lSuccess AS LOGIC
		oDocument := XmlDocument{}
		lSuccess := SaveToXml(SELF:GetMenuDescription() , oDocument)
		IF lSuccess
			TRY
				oDocument:Save(oStream)
				oStream:Flush()
			CATCH
				lSuccess := FALSE
			END TRY
		END IF
		oStream:Close()
	RETURN lSuccess

	METHOD SaveRC(oStream AS EditorStream , oAccelStream AS EditorStream , oCode AS CodeContents , cPathToVh AS STRING) AS LOGIC
		LOCAL n AS INT

		oStream:Editor:Clear()
		oStream:Editor:AddLine(e"#include \"" + cPathToVh + e"GlobalDefines.vh\"")
		oStream:Editor:AddLine(e"#include \"VOWin32APILibrary.vh\"")
		oStream:Editor:AddLine(e"")
		FOR n := 0 UPTO oCode:aResource:Count - 1
			oStream:Editor:AddLine(oCode:aResource[n])
		NEXT

		
		IF oAccelStream:IsValid
			oAccelStream:Editor:Clear()
			oAccelStream:Editor:AddLine(e"#include \"" + cPathToVh + e"GlobalDefines.vh\"")
			oAccelStream:Editor:AddLine(e"#include \"VOWin32APILibrary.vh\"")
			oAccelStream:Editor:AddLine("")
			FOR n := 0 UPTO oCode:aAccelResource:Count - 1
				oAccelStream:Editor:AddLine(oCode:aAccelResource[n])
			NEXT
			oAccelStream:Save()
		ELSE
			oStream:Editor:AddLine("")
			FOR n := 0 UPTO oCode:aAccelResource:Count - 1
				oStream:Editor:AddLine(oCode:aAccelResource[n])
			NEXT
		END IF

		oStream:Save()

	RETURN TRUE

	METHOD SavePrg(oStream AS EditorStream , oCode AS CodeContents) AS LOGIC
		LOCAL cName AS STRING
		cName := SELF:oMainNode:oDesign:Name
		oStream:Editor:ReplaceEntity(cName , cName , EntityType._Class , oCode:aClass)
		oStream:Editor:ReplaceEntity(cName , cName , EntityType._Constructor , oCode:aConstructor)
		IF SELF:HasAccelerators
			cName := SELF:oMainNode:oDesign:Name + "_Accelerator"
			oStream:Editor:ReplaceEntity(cName , cName , EntityType._Class , oCode:aAccelClass)
			oStream:Editor:ReplaceEntity(cName , cName , EntityType._Constructor , oCode:aAccelConstructor)
		END IF
		oStream:Save()
	RETURN TRUE

	METHOD SaveVh(oStream AS EditorStream , oCode AS CodeContents) AS LOGIC
		oStream:Editor:ReplaceDefines(oCode:aDefines, oCode:aDefineValues, TRUE)
//		oStream:Editor:DeleteDefines(oCode:aDefines)
		oStream:Save()
	RETURN TRUE

END CLASS
