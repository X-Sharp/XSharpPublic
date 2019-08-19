#using System.Collections.Generic
#using System.Collections
#using System.Xml
#using System.Text

ENUM DBServerItemType
	MEMBER DBServer
	MEMBER @@Field
	MEMBER Index
	MEMBER Order
END ENUM

CLASS DBServerBinary
	PROTECT cName AS STRING
	PROTECT eType AS DBServerItemType
	PROTECT aBytes AS BYTE[]
	STATIC PROTECT aBinaries AS ArrayList
	STATIC CONSTRUCTOR()
		aBinaries := ArrayList{}
	RETURN
	CONSTRUCTOR(_cName AS STRING , _eType AS DBServerItemType , _aBytes AS BYTE[])
		SELF:cName := _cName
		SELF:eType := _eType
		SELF:aBytes := _aBytes
	RETURN
	STATIC METHOD Reset() AS VOID
		aBinaries:Clear()
	RETURN
	ACCESS Bytes AS BYTE[]
	RETURN SELF:aBytes
	STATIC METHOD Add(cName AS STRING , eType AS DBServerItemType , aBytes AS BYTE[]) AS VOID
		LOCAL oBinary AS DBServerBinary
		LOCAL oTest AS DBServerBinary
		LOCAL n AS INT
		oBinary := DBServerBinary{cName , eType , aBytes}
		FOR n := 0 UPTO aBinaries:Count - 1
			oTest := (DBServerBinary)aBinaries[n]
			IF oTest:eType == eType .and. oTest:cName:ToUpper() == cName:ToUpper()
				// for some reason sometimes orders are duplicated, let's keep the last one
				aBinaries[n] := oBinary
				RETURN
			END IF
		NEXT
		aBinaries:Add(oBinary)
	RETURN
	STATIC METHOD Get(cName AS STRING , eType AS DBServerItemType) AS ArrayList
		LOCAL oBinary AS DBServerBinary
		LOCAL aRet AS ArrayList
		LOCAL n AS INT
		aRet := ArrayList{}
		FOR n := 0 UPTO aBinaries:Count - 1
			oBinary := (DBServerBinary)aBinaries[n]
			IF oBinary:eType == eType
				IF oBinary:cName:ToUpper():StartsWith(cName:ToUpper())
					aRet:Add(oBinary)
				END IF
			END IF
		NEXT
	RETURN aRet
END CLASS

CLASS VODBServerItem
	EXPORT cName AS STRING
	EXPORT eType AS DBServerItemType
	EXPORT aProperties AS System.Collections.Specialized.NameValueCollection
	EXPORT aOrders AS ArrayList
	CONSTRUCTOR(_eType AS DBServerItemType)
		SELF:aProperties := System.Collections.Specialized.NameValueCollection{}
		SELF:cName := ""
		SELF:eType := _eType
		IF SELF:eType == DBServerItemType.Index
			SELF:aOrders := ArrayList{}
		END IF
	RETURN
END CLASS

CLASS VODBServerDescription
	EXPORT oItem AS VODBServerItem
	EXPORT aFields AS ArrayList
	EXPORT aIndexes AS ArrayList
	EXPORT cName AS STRING
	CONSTRUCTOR()
		SELF:oItem := VODBServerItem{DBServerItemType.DBServer}
		SELF:aFields := ArrayList{}
		SELF:aIndexes := ArrayList{}
	RETURN
	STATIC METHOD LoadFromBinary(aBytes AS BYTE[]) AS VODBServerDescription
	RETURN VODBServerEditor.OpenVNdbs(aBytes , "")
	METHOD SaveToDocument(cFileName AS STRING) AS LOGIC
	RETURN VODBServerEditor.SaveToXml(SELF , cFileName)
END CLASS


PARTIAL CLASS VODBServerEditor
	
	STATIC METHOD OpenVNdbs(aBytes AS BYTE[] , cBaseName AS STRING) AS VODBServerDescription
		LOCAL oDescr AS VODBServerDescription
		LOCAL oItem AS VODBServerItem
		LOCAL oBinary AS DBServerBinary
		LOCAL aFields AS ArrayList
		LOCAL aIndexes AS ArrayList
		LOCAL aOrders AS ArrayList
		LOCAL oField AS VODBServerItem
		LOCAL oIndex AS VODBServerItem
		LOCAL oOrder AS VODBServerItem
		LOCAL cValue AS STRING
		LOCAL nValue AS INT
		LOCAL n,m AS INT
		LOCAL aSorted AS SortedList
		LOCAL nPos AS INT

        oDescr := VODBServerDescription{}
        oItem := oDescr:oItem
		
		aSorted := SortedList{}
		
		cValue := __ReadNextVNDBString(aBytes , nPos , 128)
		oDescr:cName := cValue
		oItem:aProperties:Add("classname" , cValue)
		oItem:aProperties:Add("hlcaption" , __ReadNextVNDBString(aBytes , nPos , 64))
		oItem:aProperties:Add("hldescription" , __ReadNextVNDBString(aBytes , nPos , 255))
		oItem:aProperties:Add("hlhelpcontext" , __ReadNextVNDBString(aBytes , nPos , 64))

		nValue := 2 - __ReadNextVNDBInt16(aBytes , nPos)
		IF nValue != 0
			oItem:aProperties:Add("share" , iif(nValue == 1 , "True" , "False"))
		END IF
		nValue := 2 - __ReadNextVNDBInt16(aBytes , nPos)
		IF nValue != 0
			oItem:aProperties:Add("ro" , iif(nValue == 1 , "True" , "False"))
		END IF

		oItem:aProperties:Add("rdd" , __ReadNextVNDBString(aBytes , nPos , 260))
		oItem:aProperties:Add("filename" , __ReadNextVNDBString(aBytes , nPos , 260))

		oItem:aProperties:Add("noaccass" , iif(__ReadNextVNDBByte(aBytes , nPos) >= 128 , "Yes" , "No"))
		__ReadNextVNDBInt16(aBytes , nPos)
		oItem:aProperties:Add("superclass" , __ReadNextVNDBString(aBytes , nPos , 80))

		aFields := DBServerBinary.Get(oDescr:cName + "_" , DBServerItemType.Field)
		FOR n := 0 UPTO aFields:Count -1
			oBinary := (DBServerBinary)aFields[n]
            aBytes := oBinary:Bytes
            nPos := 0
            oField := VODBServerItem{DBServerItemType.Field}
            cValue := __ReadNextVNDBString(aBytes , nPos , 128)
            oField:cName := cValue
            oField:aProperties:Add("fldname" , cValue)
			__ReadNextVNDBString(aBytes , nPos , 128)
			__ReadNextVNDBString(aBytes , nPos , 64)
			__ReadNextVNDBString(aBytes , nPos , 255)
			__ReadNextVNDBString(aBytes , nPos , 64)
			oField:aProperties:Add("included" , __ReadNextVNDBByte(aBytes , nPos):ToString())
			m := __ReadNextVNDBInt16(aBytes , nPos)
			DO WHILE aSorted:ContainsKey(m)
				m ++
			END DO
			oField:aProperties:Add("pos" , m:ToString())
			aSorted:Add(m , oField)
			oField:aProperties:Add("FieldSpec" , __ReadNextVNDBString(aBytes , nPos , 128))
		NEXT
		FOR n := 0 UPTO aSorted:Count - 1
			oField := (VODBServerItem)aSorted:GetByIndex(n)
			oDescr:aFields:Add(oField)
		NEXT

		aIndexes := DBServerBinary.Get(oDescr:cName + "_" , DBServerItemType.Index)
		IF aIndexes:Count != 0
			oItem:aProperties:Add("order" , "1")
		END IF
		FOR n := 0 UPTO aIndexes:Count - 1
			oBinary := (DBServerBinary)aIndexes[n]
            aBytes := oBinary:Bytes
            nPos := 0
            oIndex := VODBServerItem{DBServerItemType.Index}
            cValue := __ReadNextVNDBString(aBytes , nPos , 128)
            oIndex:cName := cValue
            oIndex:aProperties:Add("name" , cValue)
            oIndex:aProperties:Add("filename" , __ReadNextVNDBString(aBytes , nPos , 260))
            oDescr:aIndexes:Add(oIndex)
            
            aOrders := DBServerBinary.Get(oDescr:cName + "_" + oIndex:cName + "_" , DBServerItemType.Order)
			FOR m := 0 UPTO aOrders:Count - 1
				oBinary := (DBServerBinary)aOrders[m]
	            aBytes := oBinary:Bytes
	            nPos := 0
	            oOrder := VODBServerItem{DBServerItemType.Order}
	            cValue := __ReadNextVNDBString(aBytes , nPos , 128)
	            oOrder:cName := cValue
	            oOrder:aProperties:Add("tag" , cValue)
	            oOrder:aProperties:Add("Duplicate" , iif(__ReadNextVNDBByte(aBytes , nPos) == 1 , "Yes" , "No"))
	            oOrder:aProperties:Add("Ascending" , iif(__ReadNextVNDBByte(aBytes , nPos) == 1 , "Yes" , "No"))
	            oOrder:aProperties:Add("KeyExp" , __ReadNextVNDBString(aBytes , nPos , 256))
	            oOrder:aProperties:Add("ForExp" , __ReadNextVNDBString(aBytes , nPos , 256))
				oIndex:aOrders:Add(oOrder)
			NEXT

		NEXT
			
	RETURN oDescr

	STATIC METHOD SaveToXml(oDescr AS VODBServerDescription , cFileName AS STRING) AS LOGIC
		LOCAL oField AS VODBServerItem
		LOCAL oIndex AS VODBServerItem
		LOCAL oOrder AS VODBServerItem
		LOCAL oItem AS VODBServerItem
		LOCAL oDocument AS XmlDocument
		LOCAL oDesigners AS XmlNode
		LOCAL oMainNode AS XmlNode
		LOCAL oXmlNode AS XmlNode
		LOCAL oSubNode AS XmlNode
		LOCAL oIndexNode,oOrderNode AS XmlNode
		LOCAL n,m,k AS INT
		
		oDocument := XmlDocument{}
		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))

		oDesigners := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oDesigners)

		oMainNode := oDocument:CreateElement("DBServer")
		Funcs.ApplyNameAttribute(oDocument , oMainNode , oDescr:cName)
		oDesigners:AppendChild(oMainNode)
		
		oItem := oDescr:oItem
		FOR n := 0 UPTO oItem:aProperties:Count - 1
			Funcs.AppendElement(oDocument , oMainNode , oItem:aProperties:GetKey(n) , oItem:aProperties[n])
		NEXT

		oXmlNode := oDocument:CreateElement("Fields")
		oMainNode:AppendChild(oXmlNode)

		FOR m := 0 UPTO oDescr:aFields:Count - 1
			oField := (VODBServerItem)oDescr:aFields[m]
			oSubNode := oDocument:CreateElement("Field")
			Funcs.ApplyNameAttribute(oDocument , oSubNode , oField:cName)
			oXmlNode:AppendChild(oSubNode)
			FOR n := 0 UPTO oField:aProperties:Count - 1
				Funcs.AppendElement(oDocument , oSubNode , oField:aProperties:GetKey(n) , oField:aProperties[n])
			NEXT
		NEXT

		oXmlNode := oDocument:CreateElement("Indexes")
		oMainNode:AppendChild(oXmlNode)
		FOR m := 0 UPTO oDescr:aIndexes:Count - 1
			oIndex := (VODBServerItem)oDescr:aIndexes[m]
			oSubNode := oDocument:CreateElement("Index")
			Funcs.ApplyNameAttribute(oDocument , oSubNode , oIndex:cName)
			FOR n := 0 UPTO oIndex:aProperties:Count - 1
				Funcs.AppendElement(oDocument , oSubNode , oIndex:aProperties:GetKey(n) , oIndex:aProperties[n])
			NEXT
			oIndexNode := oDocument:CreateElement("Orders")
			oSubNode:AppendChild(oIndexNode)
			FOR k := 0 UPTO oIndex:aOrders:Count - 1
				oOrder := (VODBServerItem)oIndex:aOrders[k]
				oOrderNode := oDocument:CreateElement("Order")
				Funcs.ApplyNameAttribute(oDocument , oOrderNode , oOrder:cName)
				oIndexNode:AppendChild(oOrderNode)
				FOR n := 0 UPTO oOrder:aProperties:Count - 1
					Funcs.AppendElement(oDocument , oOrderNode , oOrder:aProperties:GetKey(n) , oOrder:aProperties[n])
				NEXT
			NEXT
			oXmlNode:AppendChild(oSubNode)
		NEXT

		oDocument:Save(cFileName)
	RETURN TRUE

	INTERNAL STATIC METHOD __ReadNextVNDBString(aBytes AS BYTE[] , nPos REF INT , nChars AS INT) AS STRING
		LOCAL cRet AS STRING
		LOCAL nLength AS INT
		LOCAL n AS INT
		IF nPos + nChars > aBytes:Length
			nChars := aBytes:Length - nPos
		END IF
		FOR n := 0 UPTO nChars - 1
			IF aBytes[nPos + n + 1] == 0
				EXIT
			ELSE
				nLength := n + 1
			END IF
		NEXT
		IF nLength != 0
			cRet := Encoding.Default:GetString(aBytes , nPos , nLength)
		ELSE
			cRet := ""
		END IF
		nPos += nChars
	RETURN cRet:Trim()
	INTERNAL STATIC METHOD __ReadNextVNDBInt16(aBytes AS BYTE[] , nPos REF INT) AS INT
		LOCAL nRet AS INT
		nRet := (INT)aBytes[nPos + 1] + ((INT)aBytes[nPos + 2]) * 256
		IF nRet == 65535
			nRet := -1
		END IF
		nPos += 2
	RETURN nRet
	INTERNAL STATIC METHOD __ReadNextVNDBByte(aBytes AS BYTE[] , nPos REF INT) AS BYTE
		nPos += 1
	RETURN aBytes[nPos]

END CLASS

CLASS VOMenuDescription
	INTERNAL oMainItem AS VOMenuItem
	INTERNAL cName AS STRING
	CONSTRUCTOR()
		SUPER()
		SELF:oMainItem := VOMenuItem{}
	RETURN
	STATIC METHOD LoadFromBinary(aBytes AS BYTE[]) AS VOMenuDescription
	RETURN VOMenuEditor.OpenVNmnu(aBytes)
	METHOD SaveToDocument(cFileName AS STRING) AS LOGIC
	RETURN VOMenuEditor.SaveToXml(SELF , cFileName)

	ACCESS Name AS STRING
	RETURN SELF:cName
	
	ASSIGN InheritFrom(cValue AS STRING)
		SELF:SetValue("Inherit" , cValue)
	RETURN                         
	ASSIGN ToolbarInheritFrom(cValue AS STRING)
		SELF:SetValue("ToolbarInherit" , cValue)
	RETURN                         
	ASSIGN Ribbon(cValue AS STRING)
		SELF:SetValue("Ribbon" , cValue)
	RETURN                         
	
	PROTECTED METHOD SetValue(cName AS STRING , cValue AS STRING) AS VOID
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:oMainItem:aProperties:Count - 1
			IF SELF:oMainItem:aProperties:GetKey(n):ToUpper() == cName:ToUpper()
				SELF:oMainItem:aProperties:Set(cName , cValue)
				RETURN
			END IF
		NEXT
		SELF:oMainItem:aProperties:Add(cName , cValue)
	RETURN 
END CLASS

CLASS VOMenuItem
	EXPORT nDepth AS INT
	EXPORT aProperties AS System.Collections.Specialized.NameValueCollection
	EXPORT aSubItems AS ArrayList
	CONSTRUCTOR()
		SELF:aProperties := System.Collections.Specialized.NameValueCollection{}
		SELF:aSubItems := ArrayList{}
	RETURN
END CLASS


PARTIAL CLASS VOMenuEditor

	STATIC METHOD OpenVNmnu(aBytes AS BYTE[]) AS VOMenuDescription
		LOCAL oDescr AS VOMenuDescription
		LOCAL oItem, oBase AS VOMenuItem
		LOCAL aItems AS ArrayList
		LOCAL cValue AS STRING
		LOCAL nByte AS BYTE
		LOCAL nPos AS INT
		LOCAL n AS INT
		
		oDescr := VOMenuDescription{}
		nPos := 1

		cValue := __ReadNextVNMnuString(aBytes , nPos)
		oDescr:cName := cValue
		oDescr:oMainItem:aProperties:Add("Name" , cValue)

		DO WHILE TRUE
			oItem := __ReadVNMnuItem(aBytes , nPos)
			IF oItem == NULL
				EXIT
			END IF
			aItems := oDescr:oMainItem:aSubItems
			FOR n := 1 UPTO oItem:nDepth
				IF aItems:Count != 0
					oBase := (VOMenuItem)aItems[aItems:Count - 1]
					aItems := oBase:aSubItems
				END IF
			NEXT
			aItems:Add(oItem)
		ENDDO

		oItem := oDescr:oMainItem

		nPos ++
		nByte := aBytes[nPos]
		IF nByte == 78 // N
			cValue := "No Toolbar"
		ELSEIF nByte == 70 // F
			cValue := "Flat Toolbar"
		ELSE // Y
			cValue := "Raised Toolbar"
		ENDIF
		oItem:aProperties:Add("Toolbar" , cValue)

		nPos += 2
		nByte := aBytes[nPos]
		IF nByte == 84 // T
			cValue := "Text"
		ELSEIF nByte == 73 // I
			cValue := "Icon"
		ELSE // other
			cValue := "Text and Icon"
		ENDIF
		oItem:aProperties:Add("Show" , cValue)

		nPos += 3
		oItem:aProperties:Add("UseBands" , iif(aBytes[nPos] >= 128 , "Yes" , "No"))
		
		IF aBytes:Length > nPos + 5 .and. aBytes[nPos + 1] == 5 .and. aBytes[nPos + 2] == 7 .and. aBytes[nPos + 3] == 76
			nPos += 4
			oItem:aProperties:Add("Inherit" , __ReadNextVNMnuString(aBytes , nPos))
			oItem:aProperties:Add("ToolbarInherit" , __ReadNextVNMnuString(aBytes , nPos))
		ELSE
			oItem:aProperties:Add("Inherit" , "")
			oItem:aProperties:Add("ToolbarInherit" , "")
		END IF
		
	RETURN oDescr

	PROTECTED STATIC METHOD __ReadVNMnuItem(aBytes AS BYTE[] , nPos REF INT) AS VOMenuItem
		LOCAL nButtonID AS INT
		LOCAL oItem AS VOMenuItem
		LOCAL cValue AS STRING
		LOCAL nValue AS INT
		LOCAL nAt AS INT
		
		oItem := VOMenuItem{}
		IF aBytes[nPos + 1] < 48 .or. aBytes[nPos + 1] > 57
			RETURN NULL
		END IF
		cValue := __ReadNextVNMnuString(aBytes , nPos , FALSE)
		oItem:nDepth := Funcs.Val(cValue)
	
		oItem:aProperties:Add("Caption" , __ReadNextVNMnuString(aBytes , nPos))
	
		cValue := __ReadNextVNMnuString(aBytes , nPos) // Menu ID
//		oItem:nMenuID := Funcs.Val(cValue)
	
		cValue := __ReadNextVNMnuString(aBytes , nPos)
		nAt := cValue:IndexOf('\t') + 1
		IF nAt != 0
			oItem:aProperties:Add("EventName" , Left(cValue , (DWORD)nAt - 1):Trim())
			oItem:aProperties:Add("ID" , SubStr(cValue , nAt + 1):Trim())
		ELSE
			oItem:aProperties:Add("EventName" , cValue:Trim())
			oItem:aProperties:Add("ID" , "")
		END IF
	
		oItem:aProperties:Add("Description" , __ReadNextVNMnuString(aBytes , nPos))
		oItem:aProperties:Add("HelpContext" , __ReadNextVNMnuString(aBytes , nPos))
		oItem:aProperties:Add("Accelerator" , __ReadNextVNMnuAccel(aBytes , nPos):ToString())

		cValue := __ReadNextVNMnuString(aBytes , nPos , FALSE)
		nValue := Funcs.Val(cValue)
		oItem:aProperties:Add("Enabled" , iif((nValue & 2) != 0 , "Yes" , "No"))
		oItem:aProperties:Add("Checked" , iif((nValue & 1) != 0 , "Yes" , "No"))
		
		cValue := __ReadNextVNMnuString(aBytes , nPos , FALSE)
		IF !cValue == "-1"
			nButtonID := Funcs.Val(cValue)
			IF nButtonID >= 1  .and. nButtonID < VOMenuEditor.VOMenuToolBar:Count + 1
				oItem:aProperties:Add("ButtonBmp" , VOMenuEditor.VOMenuToolBar:GetName(nButtonID - 1))
			ELSE
				oItem:aProperties:Add("ButtonBmp" , "")
			END IF
		ELSE
			oItem:aProperties:Add("ButtonBmp" , "")
		ENDIF
	
		cValue := __ReadNextVNMnuString(aBytes , nPos)
		IF cValue:Length > 8
			cValue := cValue:Substring(8)
		ELSE
			cValue := ""
		ENDIF
		oItem:aProperties:Add("ButtonCaption" , cValue)
	
		cValue := __ReadNextVNMnuString(aBytes , nPos)
		IF cValue:Length > 8
			cValue := cValue:Substring(8)
		ELSE
			cValue := ""
		ENDIF
		oItem:aProperties:Add("ButtonTooltip" , cValue)
	
		cValue := __ReadNextVNMnuString(aBytes , nPos , FALSE)
		oItem:aProperties:Add("ButtonPos" , cValue)
		
	RETURN oItem
		
	PROTECTED STATIC METHOD __ReadNextVNMnuString(aBytes AS BYTE[] , nPos REF INT) AS STRING
	RETURN __ReadNextVNMnuString(aBytes , nPos , TRUE)
	PROTECTED STATIC METHOD __ReadNextVNMnuString(aBytes AS BYTE[] , nPos REF INT , lTranslate AS LOGIC) AS STRING
		LOCAL oRead AS List<BYTE>
		LOCAL cRet AS STRING
		LOCAL bByte AS BYTE
		LOCAL n AS INT

		oRead := List<BYTE>{}
		cRet := ""
		DO WHILE nPos <= aBytes:Length
			bByte := aBytes[nPos]
			DO CASE
			CASE bByte == 13
				nPos ++
				EXIT
			CASE bByte == 9 .or. bByte >= 32
				oRead:Add(bByte)
			END CASE
			nPos ++
		END DO
		
		IF oRead:Count > 0
			IF lTranslate
				LOCAL aRead AS BYTE[]
				aRead := BYTE[]{oRead:Count}
				FOR n := 1 UPTO oRead:Count
					aRead[n] := oRead[n - 1]
				NEXT
				cRet := Encoding.Default:GetString(aRead , 0 , aRead:Length)
			ELSE
				LOCAL oBuilder AS StringBuilder
				oBuilder := StringBuilder{oRead:Count}
				FOR n := 0 UPTO oRead:Count - 1
					oBuilder:Append((Char)oRead[n])
				NEXT
				cRet := oBuilder:ToString()
			END IF
		END IF
		
	RETURN cRet:Trim()
	
	PROTECTED STATIC METHOD __ReadNextVNMnuAccel(aBytes AS BYTE[] , nPos REF INT) AS MenuAccelerator
		LOCAL cAccelerator AS STRING
		LOCAL nAccelerator AS INT
		LOCAL bModif AS BYTE
		LOCAL n AS INT

		cAccelerator := ""
		IF aBytes[nPos] == 10
			nPos ++
		END IF		
		IF aBytes[nPos] == 13
			nPos ++
			RETURN MenuAccelerator{"" , FALSE , FALSE , FALSE}
		END IF		
		bModif := aBytes[nPos]
		IF bModif >= 48 .and. bModif <= 55
			nPos ++
			DO WHILE aBytes[nPos] != 13
				cAccelerator += ChrW(aBytes[nPos])
				nPos ++
			END DO
			nPos ++
			FOR n := 0 UPTO VOMenuEditor.AccelKeys:Count - 1
				IF VOMenuEditor.AccelKeys:GetName(n) == cAccelerator
					RETURN MenuAccelerator{cAccelerator , (bModif & 4) != 0 , (bModif & 2) != 0 , (bModif & 1) != 0}
				END IF
			NEXT
		ELSE
			nPos ++
			nAccelerator := (INT)aBytes[nPos]
			nPos += 4
			bModif := aBytes[nPos]
			IF bModif > 7
				bModif -= 8
			END IF
			DO WHILE aBytes[nPos] != 13
				nPos ++
			END DO
			nPos ++
			FOR n := 0 UPTO VOMenuEditor.AccelKeys:Count - 1
				IF (INT)VOMenuEditor.AccelKeys:GetValue(n) == nAccelerator
					RETURN MenuAccelerator{VOMenuEditor.AccelKeys:GetName(n) , (bModif & 2) != 0 , (bModif & 1) != 0 , (bModif & 4) != 0}
				END IF
			NEXT
		END IF
	RETURN MenuAccelerator{"" , FALSE , FALSE , FALSE}

	STATIC METHOD SaveToXml(oDescr AS VOMenuDescription , cFileName AS STRING) AS LOGIC
		LOCAL oDocument AS XmlDocument
		LOCAL lSuccess AS LOGIC
		oDocument := XmlDocument{}
		lSuccess := SaveToXml(oDescr , oDocument)
		IF lSuccess
			TRY
				oDocument:Save(cFileName)
			CATCH
				lSuccess := FALSE
			END TRY
		END IF
	RETURN lSuccess

	STATIC METHOD SaveToXml(oDescr AS VOMenuDescription , oDocument AS XmlDocument) AS LOGIC
		LOCAL oDesigners AS XmlNode
		LOCAL oMainNode AS XmlNode
		LOCAL lSuccess AS LOGIC
		
		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))

		oDesigners := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oDesigners)

		oMainNode := oDocument:CreateElement("VOMenu")

		Funcs.ApplyNameAttribute(oDocument , oMainNode , oDescr:Name)
		oDesigners:AppendChild(oMainNode)

		lSuccess := SaveToXml(oDocument , oDescr:oMainItem , oMainNode)
	RETURN lSuccess

	PROTECTED STATIC METHOD SaveToXml(oDocument AS XmlDocument , oParent AS VOMenuItem , oParentNode AS XmlNode) AS LOGIC
		LOCAL oSubNode , oItemsNode AS XmlNode
		LOCAL oItem AS VOMenuItem
		LOCAL n AS INT
		
		FOR n := 0 UPTO oParent:aProperties:Count - 1
//			IF .not. oPair:Name:Name:EndsWith("ID")
			IF oParent:aProperties:GetKey(n) != "MenuID"
				Funcs.AppendElement(oDocument , oParentNode , oParent:aProperties:GetKey(n) , oParent:aProperties[n])
			END IF
		NEXT
		IF oParent:aSubItems:Count != 0
			oItemsNode := oDocument:CreateElement("MenuItems")
			oParentNode:AppendChild(oItemsNode)
			FOR n := 0 UPTO oParent:aSubItems:Count - 1
				oItem := (VOMenuItem)oParent:aSubItems[n]
				oSubNode := oDocument:CreateElement("MenuItem")
				oItemsNode:AppendChild(oSubNode)
				SaveToXml(oDocument , oItem , oSubNode)
			NEXT
		END IF
	RETURN TRUE


	STATIC EXPORT AccelKeys AS NameValueCollection
	STATIC EXPORT VOMenuToolBar AS NameValueCollection
	STATIC EXPORT VOMenuToolBarString AS STRING

	STATIC CONSTRUCTOR()
		LOCAL oCollection AS NameValueCollection
		LOCAL n AS INT

		oCollection := NameValueCollection{}
		FOR n := 0x41 UPTO 0x5A	// A..Z
			oCollection:Add(ChrW((DWORD)n) , n)
		NEXT
		FOR n := 0x30 UPTO 0x39 // 0..9
			oCollection:Add(ChrW((DWORD)n) , n)
		NEXT
		FOR n := 0x70 UPTO 0x87 // F1..F24
			oCollection:Add("F" + (n - 0x6F):ToString() , n)
		NEXT
		FOR n := 0x60 UPTO 0x69 // N0..N9
			oCollection:Add("Num " + (n - 0x60):ToString() , n)
		NEXT
		
		oCollection:Add("Page Down" , (INT)0x21)
		oCollection:Add("Page Up" , (INT)0x22)
		oCollection:Add("End" , (INT)0x23)
		oCollection:Add("Home" , (INT)0x24)

		oCollection:Add("Return" , (INT)0xD)
		oCollection:Add("Pause" , (INT)0x13)
		oCollection:Add("Caps Lock" , (INT)0x14)
		oCollection:Add("Insert" , (INT)0x2D)
		oCollection:Add("Delete" , (INT)0x2E)
		oCollection:Add("Help" , (INT)0x2F)

		oCollection:Add("Num Lock" , (INT)0x90)
		oCollection:Add("Scroll Lock" , (INT)0x91)

		oCollection:Add("*" , (INT)0x6B)
		oCollection:Add("+" , (INT)0x6C)
		oCollection:Add("-" , (INT)0x6D)
		oCollection:Add("/" , (INT)0x6F)
		
		VOMenuEditor.AccelKeys := oCollection


		oCollection := NameValueCollection{}

		oCollection:Add("New Sheet"   ,   "IDT_NEWSHEET")
		oCollection:Add("Open"   ,   "IDT_OPEN")
		oCollection:Add("Close"   ,   "IDT_CLOSE")
		oCollection:Add("Save"   ,   "IDT_SAVE")
		oCollection:Add("Print"   ,   "IDT_PRINT")
		oCollection:Add("Preview"   ,   "IDT_PREVIEW")
		oCollection:Add("Sum"   ,   "IDT_SUM")
		oCollection:Add("Bold"   ,   "IDT_BOLD")
		oCollection:Add("Italic"   ,   "IDT_ITALIC")
		oCollection:Add("Underline"   ,   "IDT_UNDERLINE")
		oCollection:Add("Cross Out"   ,   "IDT_CROSSOUT")
		oCollection:Add("Size+"   ,   "IDT_SIZEPLUS")
		oCollection:Add("Size-"   ,   "IDT_SIZEMINUS")
		oCollection:Add("Supersript"   ,   "IDT_SUPERSCRIPT")
		oCollection:Add("Subscript"   ,   "IDT_SUBSCRIPT")
		oCollection:Add("Currency"   ,   "IDT_CURRENCY")
		oCollection:Add("Percent"   ,   "IDT_PERCENT")
		oCollection:Add("Comma"   ,   "IDT_COMMA")
		oCollection:Add("Decimal+"   ,   "IDT_DECIMALPLUS")
		oCollection:Add("Decimal-"   ,   "IDT_DECIMALMINUS")
		oCollection:Add("Left"   ,   "IDT_LEFT")
		oCollection:Add("Center"   ,   "IDT_CENTER")
		oCollection:Add("Right"   ,   "IDT_JUSTIFY")
		oCollection:Add("Justify"   ,   "IDT_RIGHT")
		oCollection:Add("Add Text"   ,   "IDT_ADDTEXT")
		oCollection:Add("Center Text"   ,   "IDT_CENTERTEXT")
		oCollection:Add("Table"   ,   "IDT_TABLE")
		oCollection:Add("Border"   ,   "IDT_BORDERRECT")
		oCollection:Add("Edge"   ,   "IDT_BORDERLOW")
		oCollection:Add("Shading"   ,   "IDT_LIGHTSHADING")
		oCollection:Add("Copy"   ,   "IDT_COPY")
		oCollection:Add("Paste Format"   ,   "IDT_PASTEFORMAT")
		oCollection:Add("Paste Value"   ,   "IDT_PASTEVALUE")
		oCollection:Add("Undo"   ,   "IDT_UNDO")
		oCollection:Add("Repeat"   ,   "IDT_REPEAT")
		oCollection:Add("Zoom In"   ,   "IDT_ZOOMIN")
		oCollection:Add("Zoom Out"   ,   "IDT_ZOOMOUT")
		oCollection:Add("Sort A-Z"   ,   "IDT_SORTATOZ")
		oCollection:Add("Sort Z-A"   ,   "IDT_SORTZTOA")
		oCollection:Add("Lock"   ,   "IDT_LOCK")
		oCollection:Add("Outline"   ,   "IDT_OUTLINESYMBOL")
		oCollection:Add("Select Cell"   ,   "IDT_SELECTCELL")
		oCollection:Add("Scroll-"   ,   "IDT_SCROLLMINUS")
		oCollection:Add("Scroll+"   ,   "IDT_SCROLLPLUS")
		oCollection:Add("Button"   ,   "IDT_BUTTON")
		oCollection:Add("Picture"   ,   "IDT_PICTURE")
		oCollection:Add("Spell Checker"   ,   "IDT_SPELLCHECK")
		oCollection:Add("Calculate"   ,   "IDT_CALCULATE")
		oCollection:Add("Area"   ,   "IDT_AREACHART")
		oCollection:Add("Bar"   ,   "IDT_BARCHART")
		oCollection:Add("Column"   ,   "IDT_COLUMNCHART")
		oCollection:Add("Stack"   ,   "IDT_STACKCHART")
		oCollection:Add("Line"   ,   "IDT_LINECHART")
		oCollection:Add("Pie"   ,   "IDT_PIECHART")
		oCollection:Add("Scatter"   ,   "IDT_SCATTERCHART")
		oCollection:Add("3D Area"   ,   "IDT_3DAREACHART")
		oCollection:Add("3D Bar"   ,   "IDT_3DBARCHART")
		oCollection:Add("3D Column"   ,   "IDT_3DCOLUMNCHART")
		oCollection:Add("3D Col-Area"   ,   "IDT_3DCOLUMNAREACHART")
		oCollection:Add("3D Line"   ,   "IDT_3DLINECHART")
		oCollection:Add("3D Pie"   ,   "IDT_3DPIECHART")
		oCollection:Add("3D Surface"   ,   "IDT_3DSURFACECHART")
		oCollection:Add("Radar"   ,   "IDT_RADARCHART")
		oCollection:Add("Col-Line"   ,   "IDT_COLUMNLINECHART")
		oCollection:Add("Col-Stock"   ,   "IDT_COLUMNSTOCKCHART")
		oCollection:Add("Chart Format"   ,   "IDT_CHARTFORMAT")
		oCollection:Add("Edit Chart"   ,   "IDT_EDITCHART")
		oCollection:Add("Line"   ,   "IDT_LINE")
		oCollection:Add("Arrow"   ,   "IDT_ARROW")
		oCollection:Add("Freehand"   ,   "IDT_FREEHAND")
		oCollection:Add("Rectangle"   ,   "IDT_RECTANGLE")
		oCollection:Add("Ellipse"   ,   "IDT_ELLIPSE")
		oCollection:Add("Arc"   ,   "IDT_ARC")
		oCollection:Add("Polygon"   ,   "IDT_POLYGON")
		oCollection:Add("Filled Rect"   ,   "IDT_FILLEDRECTANGLE")
		oCollection:Add("Filled Ellipse"   ,   "IDT_FILLEDELLIPSE")
		oCollection:Add("Filled Arc"   ,   "IDT_FILLEDARC")
		oCollection:Add("Filled Polygon"   ,   "IDT_FILLEDPOLYGON")
		oCollection:Add("Select Object"   ,   "IDT_SELECTOBJECT")
		oCollection:Add("Edit Point"   ,   "IDT_EDITPOINT")
		oCollection:Add("Group"   ,   "IDT_GROUP")
		oCollection:Add("Ungroup"   ,   "IDT_UNGROUP")
		oCollection:Add("Front"   ,   "IDT_FRONT")
		oCollection:Add("Back"   ,   "IDT_BACK")
		oCollection:Add("Color"   ,   "IDT_COLOR")
		oCollection:Add("3D Frame"   ,   "IDT_3DFRAME")
		oCollection:Add("New Macro"   ,   "IDT_NEWMACRO")
		oCollection:Add("Function"   ,   "IDT_FUNCTION")
		oCollection:Add("CALC"   ,   "IDT_CALC")
		oCollection:Add("Go"   ,   "IDT_GOMACRO")
		oCollection:Add("Step"   ,   "IDT_STEPMACRO")
		oCollection:Add("Trace Out"   ,   "IDT_TRACEOUTMACRO")
		oCollection:Add("Trace In"   ,   "IDT_TRACEINMACRO")
		oCollection:Add("Run"   ,   "IDT_RUNMACRO")
		oCollection:Add("Skip"   ,   "IDT_SKIPMACRO")
		oCollection:Add("Reset"   ,   "IDT_RESETMACRO")
		oCollection:Add("Record"   ,   "IDT_RECORDMACRO")
		oCollection:Add("Stop"   ,   "IDT_STOPMACRO")
		oCollection:Add("Pause"   ,   "IDT_PAUSEMACRO")
		oCollection:Add("New"   ,   "IDT_NEW")
		oCollection:Add("Cut"   ,   "IDT_CUT")
		oCollection:Add("Paste"   ,   "IDT_PASTE")
		oCollection:Add("Number List"   ,   "IDT_NUMBERLIST")
		oCollection:Add("Bullet List"   ,   "IDT_BULLETLIST")
		oCollection:Add("Unindent"   ,   "IDT_UNINDENT")
		oCollection:Add("Indent"   ,   "IDT_INDENT")
		oCollection:Add("Col Format"   ,   "IDT_COLUMNFORMAT")
		oCollection:Add("Frame"   ,   "IDT_FRAME")
		oCollection:Add("Object"   ,   "IDT_OBJECT")
		oCollection:Add("Graph"   ,   "IDT_GRAPH")
		oCollection:Add("Envelope"   ,   "IDT_ENVELOPE")
		oCollection:Add("Page Layout"   ,   "IDT_PAGELAYOUT")
		oCollection:Add("Page Normal"   ,   "IDT_PAGENORMAL")
		oCollection:Add("Page Fit"   ,   "IDT_PAGEFIT")
		oCollection:Add("Left Tab"   ,   "IDT_LEFTTAB")
		oCollection:Add("Center Tab"   ,   "IDT_CENTERTAB")
		oCollection:Add("Right Tab"   ,   "IDT_RIGHTTAB")
		oCollection:Add("Decimal Tab"   ,   "IDT_DECIMALTAB")
		oCollection:Add("Paragraph"   ,   "IDT_PARAGRAPH")
		oCollection:Add("Help"   ,   "IDT_HELP")
		oCollection:Add("Delete"   ,   "IDT_DELETE")
		oCollection:Add("Time"   ,   "IDT_TIME")
		oCollection:Add("Mail"   ,   "IDT_MAIL")
		oCollection:Add("Tutorial"   ,   "IDT_TUTORIAL")
		oCollection:Add("Disk I/O"   ,   "IDT_DISKETTEIO")
		oCollection:Add("Find"   ,   "IDT_FIND")
		oCollection:Add("Previous Record"   ,   "IDT_PREVREC")
		oCollection:Add("Next Record"   ,   "IDT_NEXTREC")
		oCollection:Add("Start Record"   ,   "IDT_STARTREC")
		oCollection:Add("End Record"   ,   "IDT_ENDREC")
		oCollection:Add("View Table"   ,   "IDT_VGBROWSE")
		oCollection:Add("View Form"		,   "IDT_VFORM")

		VOMenuEditor.VOMenuToolBar := oCollection

		LOCAL oBuilder AS System.Text.StringBuilder
		oBuilder := System.Text.StringBuilder{1000}
		FOR n := 0 UPTO VOMenuEditor.VOMenuToolBar:Count - 1
			IF n != 0
				oBuilder:Append(",")
			END IF
			oBuilder:Append(VOMenuEditor.VOMenuToolBar:GetName(n))
		NEXT
		VOMenuEditor.VOMenuToolBarString := oBuilder:ToString()
		
	RETURN
END CLASS


STATIC PARTIAL CLASS Funcs
	INTERNAL STATIC METHOD Val(cValue AS STRING) AS INT
		LOCAL nRet AS INT
		Int32.TryParse(cValue , nRet)
	RETURN nRet
	STATIC METHOD ApplyNameAttribute(oDocument AS XmlDocument , oXmlNode AS XmlNode , cValue AS STRING) AS VOID
		LOCAL oAttribute AS XmlAttribute
		oAttribute := oDocument:CreateAttribute("Name")
		oAttribute:Value := cValue
		oXmlNode:Attributes:Append(oAttribute)
	RETURN
	STATIC METHOD AppendElement(oDocument AS XmlDocument , oXmlNode AS XmlNode , cName AS STRING , cValue AS STRING) AS VOID
		LOCAL oElement AS XmlElement
		oElement := oDocument:CreateElement(cName)
		oElement:InnerText := cValue
		oXmlNode:AppendChild(oElement)
	RETURN
	STATIC METHOD AppendElement(oDocument AS XmlDocument , oXmlNode AS XmlNode , cName AS STRING , nValue AS INT) AS VOID
		AppendElement(oDocument , oXmlNode , cName , nValue:ToString())
	RETURN
	STATIC METHOD AppendElement(oDocument AS XmlDocument , oXmlNode AS XmlNode , cName AS STRING , lValue AS LOGIC) AS VOID
		AppendElement(oDocument , oXmlNode , cName , iif(lValue , "Yes" , "No"))
	RETURN
END CLASS

STRUCTURE NameValue
	EXPORT Name AS STRING
	EXPORT Value AS OBJECT
	CONSTRUCTOR(_cName AS STRING , _oValue AS OBJECT)
		SELF:Name := _cName
		SELF:Value := _oValue
	RETURN
END STRUCTURE

CLASS NameValueCollection IMPLEMENTS ICollection
	PROTECT aCollection AS ArrayList
	PROTECT oEnumerator AS NameValueEnumerator
	CONSTRUCTOR()
		SELF:aCollection := ArrayList{}
		SELF:oEnumerator := NameValueEnumerator{SELF}
	RETURN
	METHOD Add(cName AS STRING , oValue AS OBJECT) AS VOID
		SELF:aCollection:Add(NameValue{cName , oValue})
	RETURN
	METHOD Get(nIndex AS INT) AS NameValue
	RETURN ((NameValue)SELF:aCollection[nIndex])
	METHOD Set(nIndex AS INT , oValue AS OBJECT) AS VOID
		LOCAL oPair AS NameValue
		oPair := SELF:Get(nIndex)
		oPair:Value := oValue
		SELF:aCollection[nIndex] := oPair
	RETURN
	METHOD GetName(nIndex AS INT) AS STRING
	RETURN ((NameValue)SELF:aCollection[nIndex]):Name
	METHOD GetValue(nIndex AS INT) AS OBJECT
	RETURN ((NameValue)SELF:aCollection[nIndex]):Value
	METHOD GetNameIndex(cName AS STRING) AS INT
		LOCAL n AS INT
		cName := cName:ToUpper()
		FOR n := 0 UPTO SELF:aCollection:Count - 1
			IF ((NameValue)SELF:aCollection[n]):Name:ToUpper() == cName
				RETURN n
			END IF
		NEXT
	RETURN -1
	METHOD GetValueIndex(oValue AS OBJECT) AS INT
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:aCollection:Count - 1
			IF ((NameValue)SELF:aCollection[n]):Value == oValue
				RETURN n
			END IF
		NEXT
	RETURN -1
	
	VIRTUAL ACCESS Count() AS INT
	RETURN SELF:aCollection:Count
	VIRTUAL ACCESS IsSynchronized() AS LOGIC
	RETURN FALSE
	VIRTUAL ACCESS SyncRoot() AS OBJECT
	RETURN NULL
	VIRTUAL METHOD CopyTo(aArray AS System.Array , nIndex AS INT) AS VOID
	RETURN
	VIRTUAL METHOD GetEnumerator() AS IEnumerator
		SELF:oEnumerator:Reset()
	RETURN SELF:oEnumerator
END CLASS

CLASS NameValueEnumerator IMPLEMENTS IEnumerator
	PROTECT oCollection AS NameValueCollection
	PROTECT nIndex AS INT
	CONSTRUCTOR(_oCollection AS NameValueCollection)
		SELF:oCollection := _oCollection
		SELF:Reset()
	RETURN
	VIRTUAL ACCESS Current() AS OBJECT
	RETURN SELF:oCollection:Get(SELF:nIndex)
	VIRTUAL METHOD MoveNext() AS LOGIC
		SELF:nIndex ++
	RETURN SELF:nIndex < SELF:oCollection:Count
	VIRTUAL METHOD Reset() AS VOID
		SELF:nIndex := -1
	RETURN

END CLASS

CLASS MenuAccelerator
	PROTECT cKey AS STRING
	PROTECT lControl , lShift , lAlt AS LOGIC
	CONSTRUCTOR(_cKey AS STRING , _lControl AS LOGIC , _lShift AS LOGIC , _lAlt AS LOGIC)
		SUPER()
		SELF:cKey := _cKey
		SELF:lControl := _lControl
		SELF:lShift := _lShift
		SELF:lAlt := _lAlt
	RETURN
	ACCESS Key AS STRING
	RETURN SELF:cKey
	ACCESS Control AS LOGIC
	RETURN SELF:lControl
	ACCESS Shift AS LOGIC
	RETURN SELF:lShift
	ACCESS Alt AS LOGIC
	RETURN SELF:lAlt
	ACCESS IsEmpty AS LOGIC
	RETURN SELF:cKey:Trim():Length == 0
	ACCESS KeyValue AS INT
		LOCAL n AS INT
		FOR n := 0 UPTO VOMenuEditor.AccelKeys:Count - 1
			IF VOMenuEditor.AccelKeys:GetName(n):ToUpper() == SELF:cKey:ToUpper()
				RETURN (INT)VOMenuEditor.AccelKeys:GetValue(n)
			END IF
		NEXT
	RETURN 0
	
	METHOD ToString() AS STRING
		LOCAL cRet AS STRING
		IF SELF:IsEmpty
			RETURN "None"
		END IF
		IF SELF:lAlt
			cRet := "Alt+" + cRet
		END IF
		IF SELF:lControl
			cRet := "Ctrl+" + cRet
		END IF
		IF SELF:lShift
			cRet := "Shift+" + cRet
		END IF
		cRet += SELF:cKey
	RETURN cRet
	
END CLASS




CLASS VOFieldSpecDescription
	INTERNAL aItems AS ArrayList
	CONSTRUCTOR()
		SUPER()
		SELF:aItems := ArrayList{}
	RETURN
	METHOD LoadFromBinary(aBytes AS BYTE[] , cName AS STRING) AS VOID
		SELF:aItems:Add(VOFieldSpecEditor.OpenVNfs(aBytes , cName))
	RETURN
	METHOD SaveToDocument(cFileName AS STRING) AS LOGIC
	RETURN VOFieldSpecEditor.SaveToXml(SELF , cFileName)
	ACCESS IsEmpty AS LOGIC
	RETURN SELF:aItems:Count == 0
END CLASS

CLASS VOFieldSpecItem
	EXPORT aProperties AS System.Collections.Specialized.NameValueCollection
	EXPORT cName AS STRING
	CONSTRUCTOR()
		SELF:aProperties := System.Collections.Specialized.NameValueCollection{}
		SELF:cName := ""
	RETURN
END CLASS

PARTIAL CLASS VOFieldSpecEditor
	STATIC METHOD OpenVNfs(aBytes AS BYTE[] , cName AS STRING) AS VOFieldSpecItem
		LOCAL oItem AS VOFieldSpecItem
		LOCAL cValue AS STRING
		LOCAL nPos AS INT

		oItem := VOFieldSpecItem{}
		oItem:cName := cName
		oItem:aProperties:Add("classname" , cName)
		
		nPos := 0
		cValue := __ReadNextVNFsString(aBytes , nPos , 128)
		oItem:aProperties:Add("HLName" , cValue)
		cValue := __ReadNextVNFsString(aBytes , nPos , 64)
		oItem:aProperties:Add("HLCaption" , cValue)
		cValue := __ReadNextVNFsString(aBytes , nPos , 255)
		oItem:aProperties:Add("HLDescription" , cValue)
		cValue := __ReadNextVNFsString(aBytes , nPos , 64)
		oItem:aProperties:Add("HLHelpContext" , cValue)

//		oItem:aProperties:Add("Type" , __ReadNextVNFsString(aBytes , nPos , 1))
		cValue := __ReadNextVNFsString(aBytes , nPos , 1)
		SWITCH cValue
		CASE "C"
			cValue := "Character"
		CASE "N"
			cValue := "Numeric"
		CASE "D"
			cValue := "Date"
		CASE "L"
			cValue := "Logic"
		CASE "M"
			cValue := "Memo"
		CASE "O"
			cValue := "OLE"
		CASE "X"
			cValue := "Multimedia"
		END SWITCH
		oItem:aProperties:Add("Type" , cValue)
		
		oItem:aProperties:Add("TypeDiag" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("TypeHelp" , __ReadNextVNFsString(aBytes , nPos , 64))

		oItem:aProperties:Add("Len" , __ReadNextVNFsInt16(aBytes , nPos):ToString())
		oItem:aProperties:Add("LenDiag" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("LenHelp" , __ReadNextVNFsString(aBytes , nPos , 64))

		oItem:aProperties:Add("Dec" , __ReadNextVNFsInt16(aBytes , nPos):ToString())

		oItem:aProperties:Add("Required" , iif(__ReadNextVNFsString(aBytes , nPos , 1) == "Y" , "Yes" , "No"))
		oItem:aProperties:Add("ReqDiag" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("ReqHelp" , __ReadNextVNFsString(aBytes , nPos , 64))

		oItem:aProperties:Add("MinLen" , __ReadNextVNFsInt16(aBytes , nPos):ToString())
		oItem:aProperties:Add("MinLenDiag" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("MinLenHelp" , __ReadNextVNFsString(aBytes , nPos , 64))

		oItem:aProperties:Add("MinRange" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("MaxRange" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("RangeDiag" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("RangeHelp" , __ReadNextVNFsString(aBytes , nPos , 64))
		
		oItem:aProperties:Add("Validation" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("ValidDiag" , __ReadNextVNFsString(aBytes , nPos , 128))
		oItem:aProperties:Add("ValidHelp" , __ReadNextVNFsString(aBytes , nPos , 128))

		oItem:aProperties:Add("Picture" , __ReadNextVNFsString(aBytes , nPos , 128))
		
		nPos += 4
		TRY
			oItem:aProperties:Add("superclass" , __ReadNextVNFsString(aBytes , nPos , 80))
		END TRY

	RETURN oItem

	STATIC METHOD SaveToXml(oDescr AS VOFieldSpecDescription , cFileName AS STRING) AS LOGIC
		LOCAL oDocument AS XmlDocument
		LOCAL oElement AS XmlElement
		LOCAL n AS INT

		oDocument := XmlDocument{}
		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))
		oElement := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oElement)

		FOR n := 0 UPTO oDescr:aItems:Count - 1
			SaveToXml(oDocument , oElement , (VOFieldSpecItem)oDescr:aItems[n])
		NEXT

		oDocument:Save(cFileName)
	RETURN TRUE
	STATIC METHOD SaveToXml(oDocument AS XmlDocument , oXmlNode AS XmlNode , oItem AS VOFieldSpecItem) AS LOGIC
		LOCAL oElement AS XmlElement
		LOCAL n AS INT
		oElement := oDocument:CreateElement("FieldSpec")
		Funcs.ApplyNameAttribute(oDocument , oElement , oItem:cName)
		FOR n := 0 UPTO oItem:aProperties:Count - 1
			Funcs.AppendElement(oDocument , oElement , oItem:aProperties:GetKey(n) , oItem:aProperties[n])
		NEXT
		oXmlNode:AppendChild(oElement)
	RETURN TRUE

	INTERNAL STATIC METHOD __ReadNextVNFsString(aBytes AS BYTE[] , nPos REF INT , nChars AS INT) AS STRING
		LOCAL cRet AS STRING
		LOCAL nLength AS INT
		LOCAL n AS INT
		IF nPos + nChars > aBytes:Length
			nChars := aBytes:Length - nPos
		END IF
		FOR n := 0 UPTO nChars - 1
			IF aBytes[nPos + n + 1] == 0
				EXIT
			ELSE
				nLength := n + 1
			END IF
		NEXT
		IF nLength != 0
			cRet := Encoding.Default:GetString(aBytes , nPos , nLength)
		ELSE
			cRet := ""
		END IF
		nPos += nChars
	RETURN cRet:Trim()
	INTERNAL STATIC METHOD __ReadNextVNFsInt16(aBytes AS BYTE[] , nPos REF INT) AS INT
		LOCAL nRet AS INT
		nRet := (INT)aBytes[nPos + 1] + ((INT)aBytes[nPos + 2]) * 256
		IF nRet == 65535
			nRet := -1
		END IF
		nPos += 2
	RETURN nRet
END CLASS
