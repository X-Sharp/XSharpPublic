// Note: The code here is much as possible 1:1 copied from very old original code in XIDE, in order to avoid introducing problems
// Please do not adjust it to make it more readable/efficient, at least until we have made absolutely sure everything works as expected

USING System.Windows.Forms
USING System.Text
USING System.Collections.Generic
USING System.Collections
USING System.IO
USING System.Xml

CLASS BinaryEntity

	STATIC METHOD SaveWindowToWed(aBytes AS BYTE[], cPrg AS STRING , cVNForm AS STRING) AS VOID
		LOCAL aItems AS List<VOWEDItem>
		LOCAL oWriter AS StreamWriter
		LOCAL oItem AS VOWEDItem
		LOCAL cWedFile AS STRING
		LOCAL cLine AS STRING
		LOCAL cForm AS STRING
		LOCAL n AS INT

		aItems := ReadWindowFromBytes(aBytes)
		IF aItems == NULL .or. aItems:Count == 0
			RETURN
		END IF

		cWedFile := cPrg + ".wed"
		oWriter := StreamWriter{cWedFile , TRUE , System.Text.Encoding.GetEncoding(0)}
		oItem := aItems[0]
		cForm := oItem:cName
		cLine := "DESIGNERSTART = VOWindowEditor," + oItem:cControl + "," + cForm
		oWriter:WriteLine(cLine)
		oWriter:WriteLine("GUID=" + NewGuid())
		VOWEDItem.SaveItem(oWriter , oItem)

		oWriter:WriteLine("")
		oWriter:WriteLine("SUBCONTROLSSTART")
		FOR n := 1 UPTO aItems:Count - 1
			oItem := aItems[n]
			cLine := "CONTROL=" + oItem:cControl
			oWriter:WriteLine(cLine)
			oWriter:WriteLine("GUID=" + NewGuid())
			VOWEDItem.SaveItem(oWriter , oItem)
			oWriter:WriteLine("")
		NEXT
		oWriter:WriteLine("SUBCONTROLSEND")
		oWriter:WriteLine("DESIGNEREND = " + cForm)
		oWriter:WriteLine("")

		oWriter:Close()
	RETURN

	STATIC PRIVATE METHOD ReadWindowFromBytes(aBytes AS BYTE[]) AS List<VOWEDItem>
		LOCAL aItems AS List<VOWEDItem>
		LOCAL oMemory AS MemoryStream
		LOCAL oBinary AS BinaryReader
		LOCAL oItem AS VOWEDItem

		oMemory := MemoryStream{aBytes}
		oMemory:Write(aBytes, 0, aBytes:Length)
		oMemory:Position := 0

		aItems := NULL

		IF aItems == NULL
			aItems := List<VOWEDItem>{}
			oBinary := BinaryReader{oMemory , System.Text.Encoding.GetEncoding(0)}
			TRY
				oItem := VOWEDItem.ReadVNFrmControl(oBinary)
				aItems:Add(oItem)
				TRY
					DO WHILE TRUE
						oItem := VOWEDItem.ReadVNFrmControl(oBinary)
						aItems:Add(oItem)
					END DO
				CATCH
					NOP
				END TRY
			CATCH
				NOP
			END TRY
			oBinary:Close()
			oMemory:Dispose()
		END IF
	RETURN aItems

	STATIC METHOD SaveWindowToXml(cBinaryFileName AS STRING, aBytes AS BYTE[]) AS VOID
		LOCAL aDesign AS List<VOWEDItem>
		LOCAL oStream AS FileStream
		LOCAL oDocument AS XmlDocument
		LOCAL oDesigners AS XmlNode
		LOCAL oMainNode AS XmlNode
		LOCAL oDesign AS VOWEDItem
		LOCAL oNode AS XmlNode
		LOCAL n AS INT

		aDesign := ReadWindowFromBytes(aBytes)
		IF aDesign == NULL .or. aDesign:Count == 0
			RETURN
		END IF

		oStream := File.Create(cBinaryFileName)

		oDocument := XmlDocument{}
		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))

		oDesigners := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oDesigners)

		oMainNode := oDocument:CreateElement("VOWindow")

		oDesign := aDesign[0]
		ApplyNameAttribute(oDocument , oMainNode , oDesign:Name)
		oDesigners:AppendChild(oMainNode)

		oNode := oDocument:CreateElement("Window")
		ApplyNameAttribute(oDocument , oNode , oDesign:Name)
		oMainNode:AppendChild(oNode)
		SaveWindowItemToXml(oDocument , oDesign , oNode)

		FOR n := 1 UPTO aDesign:Count - 1
			oDesign := (VOWEDItem)aDesign[n]
			oNode := oDocument:CreateElement("Control")
			ApplyNameAttribute(oDocument , oNode , oDesign:Name)
			oMainNode:AppendChild(oNode)
			SaveWindowItemToXml(oDocument , oDesign , oNode)
		NEXT

		oDocument:Save(oStream)

		oStream:Flush()
		oStream:Close()
	RETURN

	STATIC PRIVATE METHOD SaveWindowItemToXml(oDocument AS XmlDocument , oItem AS VOWEDItem , oParentNode AS XmlNode) AS LOGIC
		LOCAL oGroupNode AS XmlNode
		LOCAL n AS INT

		AppendElement(oDocument , oParentNode , "Name" , oItem:cName)
		AppendElement(oDocument , oParentNode , "Class" , oItem:cControl)
		AppendElement(oDocument , oParentNode , "Caption" , oItem:cCaption)
		AppendElement(oDocument , oParentNode , "Order" , oItem:nOrder)
		AppendElement(oDocument , oParentNode , "Left" , oItem:nLeft)
		AppendElement(oDocument , oParentNode , "Top" , oItem:nTop)
		AppendElement(oDocument , oParentNode , "Width" , oItem:nWidth)
		AppendElement(oDocument , oParentNode , "Height" , oItem:nHeight)
		AppendElement(oDocument , oParentNode , "BrowseIndex" , oItem:nBrowseIndex)
		AppendElement(oDocument , oParentNode , "BrowseSize" , oItem:nBrowseSize)
		AppendElement(oDocument , oParentNode , "SubForm" , oItem:lSubForm)
		AppendElement(oDocument , oParentNode , "NoSave" , oItem:nNoSave == 1)
		AppendElement(oDocument , oParentNode , "Deleted" , oItem:nDeleted)
		AppendElement(oDocument , oParentNode , "Styles" , oItem:dStyles:ToString())
		AppendElement(oDocument , oParentNode , "ExStyles" , oItem:dExStyles:ToString())

		IF oItem:aProperties:Count != 0
			oGroupNode := oDocument:CreateElement("ExProperties")
			oParentNode:AppendChild(oGroupNode)
			FOR n := 0 UPTO oItem:aProperties:Count - 1
				TRY
					AppendElement(oDocument , oGroupNode , oItem:aProperties:Get(n):Name , (STRING)oItem:aProperties:Get(n):Value)
				CATCH e AS Exception
					ShowWarning("There was an error while applying property named '" + oItem:aProperties:Get(n):Name + "' to a binary entity. Exception message:" + CRLF + CRLF + e:ToString())
				END TRY
			NEXT
		END IF

		IF oItem:aTabPages:Count != 0
			LOCAL oPage AS VOTabPageOptions
			LOCAL oPageNode AS XmlNode
			oGroupNode := oDocument:CreateElement("TabPages")
			oParentNode:AppendChild(oGroupNode)
			FOR n := 0 UPTO oItem:aTabPages:Count - 1
				oPage := (VOTabPageOptions)oItem:aTabPages[n]
				oPageNode := oDocument:CreateElement("TabPage")
				ApplyNameAttribute(oDocument , oPageNode , oPage:cName)
				oGroupNode:AppendChild(oPageNode)
				AppendElement(oDocument , oPageNode , "Name" , oPage:cName)
				AppendElement(oDocument , oPageNode , "Caption" , oPage:cCaption)
				AppendElement(oDocument , oPageNode , "DataAware" , oPage:lDataAware)
			NEXT
		END IF

	RETURN TRUE


	STATIC METHOD SaveMenuToWed(aBytes AS BYTE[], cPrg AS STRING , cVNForm AS STRING) AS VOID
		LOCAL oWriter AS StreamWriter
		LOCAL oItem AS VOMedItem
		LOCAL cLine AS STRING
		LOCAL cForm AS STRING
		LOCAL nPos AS INT
		LOCAL nDepth AS INT
		LOCAL cWedFile AS STRING
		LOCAL aLines AS List<STRING>
		LOCAL nByte AS BYTE
		LOCAL n AS INT

		cWedFile := cPrg + ".wed"

		nPos := 1
		cForm := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos)

		aLines := List<STRING>{}
		TRY
			DO WHILE TRUE
				oItem := VOMedItem.__ReadVNMnuItem(aBytes , REF nPos)
				IF oItem == NULL
					EXIT
				ENDIF

				IF oItem:nDepth > nDepth
					nDepth := oItem:nDepth
					aLines:Add("")
					aLines:Add("SUBCONTROLSSTART")
					aLines:Add("")
				ELSEIF oItem:nDepth < nDepth
					DO WHILE oItem:nDepth < nDepth
						nDepth --
						aLines:Add("")
						aLines:Add("SUBCONTROLSEND")
						aLines:Add("")
					END DO
				ENDIF

				aLines:Add("CONTROL=MENUITEM")
				aLines:Add("GUID=" + NewGuid())

				aLines:Add("OriginalDefineValue=" + oItem:cOriginalDefineValue)
				aLines:Add("EventName=" + oItem:cEventName)
				aLines:Add("Caption=" + oItem:cCaption)
				aLines:Add("Description=" + oItem:cDescription)
				aLines:Add("HelpContext=" + oItem:cHelpID)
				aLines:Add("Accelerator=" + oItem:oAccelerator:ToString())
				aLines:Add("Enabled=" + iif(oItem:lEnabled , "Yes" , "No"))
				aLines:Add("Checked=" + iif(oItem:lChecked , "Yes" , "No"))
				aLines:Add("ButtonBmp=" + oItem:cButtonID)
				aLines:Add("ButtonCaption=" + oItem:cButtonCaption)
				aLines:Add("ButtonToolTip=" + oItem:cButtonTooltip)
				aLines:Add("ButtonPos=" + oItem:nButtonPosition:ToString())
			END DO
		CATCH
			NOP
		END TRY

		FOR n := 1 UPTO nDepth
			aLines:Add("")
			aLines:Add("SUBCONTROLSEND")
			aLines:Add("")
		NEXT

		oWriter := StreamWriter{cWedFile , TRUE , System.Text.Encoding.GetEncoding(0)}
		cLine := "DESIGNERSTART = VOMenu," + "Menu" + "," + cForm
		oWriter:WriteLine(cLine)
		oWriter:WriteLine("GUID=" + NewGuid())
		oWriter:WriteLine("Name=" + cForm)

		nPos ++
		IF nPos < aBytes:Length // some old vo apps have short vnmnu files
			nByte := aBytes[nPos]
			cLine := "Toolbar="
			IF nByte == 78 // N
				cLine += "No Toolbar"
			ELSEIF nByte == 70 // F
				cLine += "Flat Toolbar"
			ELSE // Y
				cLine += "Raised Toolbar"
			ENDIF
			oWriter:WriteLine(cLine)

			nPos += 2
			nByte := aBytes[nPos]
			cLine := "Show="
			IF nByte == 84 // T
				cLine += "Text"
			ELSEIF nByte == 73 // I
				cLine += "Icon"
			ELSE // other
				cLine += "Text and Icon"
			ENDIF
			oWriter:WriteLine(cLine)

			nPos += 3
			oWriter:WriteLine("UseBands=" + iif(aBytes[nPos] >= 128 , "Yes" , "No"))

			IF aBytes:Length > nPos + 5 .and. aBytes[nPos + 1] == 5 .and. aBytes[nPos + 2] == 7 .and. aBytes[nPos + 3] == 76
				nPos += 4
				oWriter:WriteLine("Inherit=" + VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos))
				oWriter:WriteLine("ToolbarInherit=" + VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos))
			END IF
		END IF

		oWriter:WriteLine("")
		oWriter:WriteLine("SUBCONTROLSSTART")
		oWriter:WriteLine("")

		FOR n := 0 UPTO aLines:Count - 1
			oWriter:WriteLine(aLines[n])
		NEXT

		oWriter:WriteLine("")
		oWriter:WriteLine("SUBCONTROLSEND")
		oWriter:WriteLine("")
		oWriter:WriteLine("DESIGNEREND = " + cForm)
		oWriter:WriteLine("")

		oWriter:Flush()
		oWriter:Close()
	RETURN

	STATIC PRIVATE METHOD ReadMenuFromBytes(aBytes AS BYTE[]) AS VOMenuDescription
		LOCAL oDescr AS VOMenuDescription
		LOCAL oItem, oBase AS VOMenuItem
		LOCAL aItems AS ArrayList
		LOCAL cValue AS STRING
		LOCAL nByte AS BYTE
		LOCAL nPos AS INT
		LOCAL n AS INT

		oDescr := VOMenuDescription{}
		nPos := 1

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos)
		oDescr:cName := cValue
		oDescr:oMainItem:aProperties:Add("Name" , cValue)

		DO WHILE TRUE
			oItem := VOMenuItem.__ReadVNMnuItem(aBytes , REF nPos)
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
			oItem:aProperties:Add("Inherit" , VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos))
			oItem:aProperties:Add("ToolbarInherit" , VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos))
		ELSE
			oItem:aProperties:Add("Inherit" , "")
			oItem:aProperties:Add("ToolbarInherit" , "")
		END IF

	RETURN oDescr

	STATIC METHOD SaveMenuToXml(cBinaryFileName AS STRING, aBytes AS BYTE[]) AS VOID
		LOCAL oDescr AS VOMenuDescription
		LOCAL oDocument AS XmlDocument
		oDescr := ReadMenuFromBytes(aBytes)
		oDocument := XmlDocument{}
		SaveMenuToXml(oDescr , oDocument)
		TRY
			oDocument:Save(cBinaryFileName)
		CATCH
			NOP
		END TRY
	RETURN

	STATIC PRIVATE METHOD SaveMenuToXml(oDescr AS VOMenuDescription , oDocument AS XmlDocument) AS VOID
		LOCAL oDesigners AS XmlNode
		LOCAL oMainNode AS XmlNode

		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))

		oDesigners := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oDesigners)

		oMainNode := oDocument:CreateElement("VOMenu")

		ApplyNameAttribute(oDocument , oMainNode , oDescr:Name)
		oDesigners:AppendChild(oMainNode)

		SaveMenuToXml(oDocument , oDescr:oMainItem , oMainNode)
	RETURN

	STATIC PRIVATE METHOD SaveMenuToXml(oDocument AS XmlDocument , oParent AS VOMenuItem , oParentNode AS XmlNode) AS LOGIC
		LOCAL oSubNode , oItemsNode AS XmlNode
		LOCAL oItem AS VOMenuItem
		LOCAL n AS INT

		FOR n := 0 UPTO oParent:aProperties:Count - 1
//			IF .not. oPair:Name:Name:EndsWith("ID")
			IF oParent:aProperties:GetKey(n) != "MenuID"
				AppendElement(oDocument , oParentNode , oParent:aProperties:GetKey(n) , oParent:aProperties[n])
			END IF
		NEXT
		IF oParent:aSubItems:Count != 0
			oItemsNode := oDocument:CreateElement("MenuItems")
			oParentNode:AppendChild(oItemsNode)
			FOR n := 0 UPTO oParent:aSubItems:Count - 1
				oItem := (VOMenuItem)oParent:aSubItems[n]
				oSubNode := oDocument:CreateElement("MenuItem")
				oItemsNode:AppendChild(oSubNode)
				SaveMenuToXml(oDocument , oItem , oSubNode)
			NEXT
		END IF
	RETURN TRUE

	STATIC PRIVATE METHOD ApplyNameAttribute(oDocument AS XmlDocument , oXmlNode AS XmlNode , cValue AS STRING) AS VOID
		LOCAL oAttribute AS XmlAttribute
		oAttribute := oDocument:CreateAttribute("Name")
		oAttribute:Value := cValue
		oXmlNode:Attributes:Append(oAttribute)
	RETURN
	STATIC PRIVATE METHOD AppendElement(oDocument AS XmlDocument , oXmlNode AS XmlNode , cName AS STRING , cValue AS STRING) AS VOID
		LOCAL oElement AS XmlElement
		oElement := oDocument:CreateElement(cName)
		oElement:InnerText := cValue
		oXmlNode:AppendChild(oElement)
	RETURN
	STATIC PRIVATE METHOD AppendElement(oDocument AS XmlDocument , oXmlNode AS XmlNode , cName AS STRING , nValue AS INT) AS VOID
		AppendElement(oDocument , oXmlNode , cName , nValue:ToString())
	RETURN
	STATIC PRIVATE METHOD AppendElement(oDocument AS XmlDocument , oXmlNode AS XmlNode , cName AS STRING , lValue AS LOGIC) AS VOID
		AppendElement(oDocument , oXmlNode , cName , iif(lValue , "Yes" , "No"))
	RETURN
END CLASS


CLASS VOWEDItem
	EXPORT cName AS STRING
	EXPORT nOrder AS INT
	EXPORT cControl AS STRING
	EXPORT cCaption AS STRING
	EXPORT cClass AS STRING

	EXPORT nLeft   AS INT
	EXPORT nTop    AS INT
	EXPORT nWidth  AS INT
	EXPORT nHeight AS INT

	EXPORT nBrowseIndex AS INT
	EXPORT nBrowseSize AS INT
	EXPORT lSubForm AS LOGIC
	EXPORT nNoSave AS INT
	EXPORT nDeleted AS INT
	EXPORT dStyles AS DWORD
	EXPORT dExStyles AS DWORD

	EXPORT aProperties AS NameValueCollection
	EXPORT aTabPages AS List<VOTabPageOptions>

	PROPERTY Name AS STRING GET SELF:cName

	CONSTRUCTOR()
		SUPER()
		SELF:aProperties := NameValueCollection{}
		SELF:aTabPages := List<VOTabPageOptions>{}
	RETURN
	STATIC METHOD SaveItem(oStream AS StreamWriter , oItem AS VOWEDItem) AS VOID
		LOCAL cSpace AS STRING
		LOCAL n AS INT
		cSpace := "  "
		oStream:WriteLine(cSpace + "Name=" + oItem:cName)
		oStream:WriteLine(cSpace + "Class=" + oItem:cControl)
		oStream:WriteLine(cSpace + "Caption=" + oItem:cCaption)
		oStream:WriteLine(cSpace + "Order=" + oItem:nOrder:ToString())
		oStream:WriteLine(cSpace + "Left=" + oItem:nLeft:ToString())
//		oStream:WriteLine(cSpace + "Right=" + (oItem:nLeft + oItem:nWidth):ToString())
		oStream:WriteLine(cSpace + "Width=" + oItem:nWidth:ToString())
		oStream:WriteLine(cSpace + "Top=" + oItem:nTop:ToString())
//		oStream:WriteLine(cSpace + "Bottom=" + (oItem:nTop + oItem:nHeight):ToString())
		oStream:WriteLine(cSpace + "Height=" + oItem:nHeight:ToString())
		oStream:WriteLine(cSpace + "Browseindex=" + oItem:nBrowseIndex:ToString())
		oStream:WriteLine(cSpace + "Browsesize=" + oItem:nBrowseSize:ToString())
		oStream:WriteLine(cSpace + "Subform=" + iif(oItem:lSubForm , "Yes" , "No"))
		oStream:WriteLine(cSpace + "Nosave=" + iif(oItem:nNoSave==1 , "Yes" , "No"))
		oStream:WriteLine(cSpace + "Deleted=" + oItem:nDeleted:ToString())
		oStream:WriteLine(cSpace + "Styles=" + oItem:dStyles:ToString())
		oStream:WriteLine(cSpace + "Exstyles=" + oItem:dExStyles:ToString())

		FOR n := 0 UPTO oItem:aProperties:Count - 1
			oStream:WriteLine(cSpace + "Property=" + oItem:aProperties:Get(n):Name + "=" + (STRING)oItem:aProperties:Get(n):Value)
		NEXT

		IF oItem:aTabPages != NULL
			LOCAL oPage AS VOTabPageOptions
			FOR n := 0 UPTO oItem:aTabPages:Count - 1
				oPage := oItem:aTabPages[n]
				oStream:WriteLine(cSpace + "TabPageName=" + oPage:cName)
				oStream:WriteLine(cSpace + "TabPageCaption=" + oPage:cCaption)
				oStream:WriteLine(cSpace + "TabPageDataAware=" + iif(oPage:lDataAware , "YES" , "NO" ))
			NEXT
		END IF
	RETURN

	STATIC METHOD __ReadString(oReader AS BinaryReader , nLength AS INT) AS STRING
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
		cRet := STRING{System.Text.Encoding.Default:GetChars(aBytes , 0 , n - 1)}
	RETURN cRet
	STATIC METHOD ReadVNFrmControl(oReader AS BinaryReader) AS VOWEDItem
		LOCAL nLeft,nRight,nTop,nBottom AS INT
		LOCAL nAt , nPropLength AS INT
		LOCAL cName , cValue AS STRING
		LOCAL cControl AS STRING
		LOCAL oItem AS VOWEDItem

		oItem := VOWEDItem{}
		oItem:cName := __ReadString(oReader , 63)
		oItem:nOrder := (INT)oReader:ReadInt16()

		cControl := __ReadString(oReader , 65)
		oItem:cClass := cControl
		nAt := cControl:LastIndexOf(':')
		IF nAt != -1
			cControl := cControl:Substring(nAt + 1)
		ENDIF
		oItem:cControl := cControl
		oItem:cCaption := __ReadString(oReader , 65)

		nLeft := (INT)oReader:ReadInt16() + 1
		nTop := (INT)oReader:ReadInt16() + 1
		nRight := (INT)oReader:ReadInt16() + 1
		nBottom := (INT)oReader:ReadInt16() + 1
		oItem:nLeft := nLeft
		oItem:nTop := nTop
		oItem:nWidth := nRight - nLeft
		oItem:nHeight := nBottom - nTop

		oItem:nDeleted := (INT)oReader:ReadInt16()
		oItem:nBrowseIndex := (INT)oReader:ReadInt16()
		oItem:nBrowseSize := (INT)oReader:ReadInt16()
		oItem:lSubForm := oReader:ReadByte() == 1
		oItem:nNoSave := (INT)oReader:ReadByte()
		oItem:dStyles := oReader:ReadUInt32()
		oItem:dExStyles := oReader:ReadUInt32()

		nPropLength := oReader:ReadUInt16()
		oItem:aProperties := NameValueCollection{}
		cName := "";cValue := ""
		LOCAL aBytes AS BYTE[]
		LOCAL lInValue AS LOGIC
		LOCAL n AS INT
		aBytes := oReader:ReadBytes(nPropLength)

		FOR n := 1 UPTO aBytes:Length
			IF aBytes[n] == 0
				oItem:aProperties:Add(cName , cValue)
				cName := "";cValue := ""
				lInValue := FALSE
			ELSE
				DO CASE
//				CASE (Char)aBytes[n] == '"'
				CASE (Char)aBytes[n] == '(' .or. (Char)aBytes[n] == ')'
					lInValue := TRUE
				CASE !lInValue
//					cName += ((Char)aBytes[n]):ToString()
					cName += Encoding.Default:GetString(aBytes , n - 1 , 1)
				OTHERWISE
//					cValue += ((Char)aBytes[n]):ToString()
					cValue += Encoding.Default:GetString(aBytes , n - 1 , 1)
				ENDCASE
			ENDIF
		NEXT
		IF cName != ""
			oItem:aProperties:Add(cName , cValue)
		END IF
//		oItem:cProperties := __ReadString(oReader , nPropLength)

		IF oItem:cControl == "TABCONTROL"
			LOCAL nPages AS INT
			LOCAL nByte AS BYTE
			LOCAL nZeroCount AS INT
			LOCAL cTab AS STRING
			LOCAL oTabPage AS VOTabPageOptions
			nPages := oReader:ReadInt16()
			FOR n := 1 UPTO nPages
				oTabPage := VOTabPageOptions{}
				oItem:aTabPages:Add(oTabPage)
				nZeroCount := 0
				cTab := ""
				DO WHILE nZeroCount < 2
					nByte := oReader:ReadByte()
					IF nByte == 0
						nZeroCount ++
						IF nZeroCount == 1
							oTabPage:cCaption := cTab
						ELSE
							oTabPage:cName := cTab
						ENDIF
						cTab := ""
					ELSE
//						cTab += ((Char)(DWORD)nByte):ToString()
						cTab += Encoding.Default:GetString(<BYTE>{nByte} , 0 , 1)
					ENDIF
				END DO
				oTabPage:lDataAware := oReader:ReadByte() == 89
//				oReader:ReadByte()
			NEXT

		ENDIF

	RETURN oItem

END CLASS

CLASS VOTabPageOptions
	EXPORT cName AS STRING
	EXPORT cCaption AS STRING
	EXPORT lDataAware AS LOGIC
	CONSTRUCTOR()
		SELF:cName := ""
		SELF:cCaption := ""
	RETURN
END CLASS


CLASS VOMedItem
	EXPORT lMenu AS LOGIC

	EXPORT cID AS STRING
	EXPORT cEventName AS STRING
	EXPORT cCaption AS STRING
	EXPORT cDescription AS STRING
	EXPORT cHelpID AS STRING
	EXPORT oAccelerator AS MenuAccelerator
	EXPORT lEnabled AS LOGIC
	EXPORT lChecked AS LOGIC

	EXPORT nDepth AS INT
	EXPORT nFlags AS INT

	EXPORT nButtonID AS INT
	EXPORT cButtonID AS STRING
	EXPORT cButtonCaption AS STRING
	EXPORT cButtonTooltip AS STRING
	EXPORT nButtonPosition AS INT

	EXPORT cOriginalDefineValue AS STRING

	CONSTRUCTOR()
	RETURN

	STATIC METHOD __ReadVNMnuItem(aBytes AS BYTE[] , nPos REF INT) AS VOMedItem
		LOCAL oItem AS VOMedItem
		LOCAL cValue AS STRING
		LOCAL nValue AS INT
		LOCAL nAt AS INT

		oItem := VOMedItem{}
		IF aBytes[nPos + 1] < 48 .or. aBytes[nPos + 1] > 57
			oItem:lMenu := TRUE
//			RETURN oItem
			RETURN NULL
		END IF
		cValue := __ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		oItem:nDepth := Funcs.Val(cValue)

		oItem:cCaption := __ReadNextVNMnuString(aBytes , REF nPos)

		cValue := __ReadNextVNMnuString(aBytes , REF nPos) // Menu ID
		oItem:cOriginalDefineValue := cValue

		cValue := __ReadNextVNMnuString(aBytes , REF nPos)
		nAt := cValue:IndexOf('\t') + 1
		IF nAt != 0
			oItem:cEventName := Funcs.Left(cValue , (DWORD)nAt - 1):Trim()
			oItem:cID := Funcs.SubStr(cValue , nAt + 1):Trim()
		ELSE
			oItem:cEventName := cValue:Trim()
			oItem:cID := ""
		END IF

		oItem:cDescription := __ReadNextVNMnuString(aBytes , REF nPos)

		oItem:cHelpID := __ReadNextVNMnuString(aBytes , REF nPos)

		oItem:oAccelerator := __ReadNextVNMnuAccel(aBytes , REF nPos)

		cValue := __ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		nValue := Funcs.Val(cValue)
		oItem:lEnabled := (nValue & 2) != 0
		oItem:lChecked := (nValue & 1) != 0

		cValue := __ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		IF !cValue == "-1"
			oItem:nButtonID := Funcs.Val(cValue)
			IF oItem:nButtonID >= 1  .and. oItem:nButtonID < VOMenuProperties.VOMenuToolBar:Count + 1
				oItem:cButtonID := VOMenuProperties.VOMenuToolBar:GetName(oItem:nButtonID - 1)
			ELSE
				oItem:cButtonID := ""
			END IF
		ELSE
			oItem:cButtonID := ""
		ENDIF

		cValue := __ReadNextVNMnuString(aBytes , REF nPos)
		IF cValue:Length > 8
			cValue := cValue:Substring(8)
		ELSE
			cValue := ""
		ENDIF
		oItem:cButtonCaption := cValue

		cValue := __ReadNextVNMnuString(aBytes , REF nPos)
		IF cValue:Length > 8
			cValue := cValue:Substring(8)
		ELSE
			cValue := ""
		ENDIF
		oItem:cButtonTooltip := cValue

		cValue := __ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		oItem:nButtonPosition := Funcs.Val(cValue)

	RETURN oItem

	STATIC METHOD __ReadNextVNMnuString(aBytes AS BYTE[] , nPos REF INT) AS STRING
	RETURN __ReadNextVNMnuString(aBytes , REF nPos , TRUE)
	STATIC METHOD __ReadNextVNMnuString(aBytes AS BYTE[] , nPos REF INT , lTranslate AS LOGIC) AS STRING
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
				cRet := System.String{Encoding.Default:GetChars(aRead , 0 , aRead:Length)}
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

	STATIC METHOD __ReadNextVNMnuAccel(aBytes AS BYTE[] , nPos REF INT) AS MenuAccelerator
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
			FOR n := 0 UPTO VOMenuProperties.AccelKeys:Count - 1
				IF VOMenuProperties.AccelKeys:GetName(n) == cAccelerator
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
			FOR n := 0 UPTO VOMenuProperties.AccelKeys:Count - 1
				IF (INT)VOMenuProperties.AccelKeys:GetValue(n) == nAccelerator
					RETURN MenuAccelerator{VOMenuProperties.AccelKeys:GetName(n) , (bModif & 2) != 0 , (bModif & 1) != 0 , (bModif & 4) != 0}
				END IF
			NEXT
		END IF
	RETURN MenuAccelerator{"" , FALSE , FALSE , FALSE}

END CLASS

CLASS VOMenuDescription
	INTERNAL oMainItem AS VOMenuItem
	INTERNAL cName AS STRING
	CONSTRUCTOR()
		SUPER()
		SELF:oMainItem := VOMenuItem{}
	RETURN

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

	STATIC METHOD __ReadVNMnuItem(aBytes AS BYTE[] , nPos REF INT) AS VOMenuItem
		LOCAL nButtonID AS INT
		LOCAL oItem AS VOMenuItem
		LOCAL cValue AS STRING
		LOCAL nValue AS INT
		LOCAL nAt AS INT

		oItem := VOMenuItem{}
		IF aBytes[nPos + 1] < 48 .or. aBytes[nPos + 1] > 57
			RETURN NULL
		END IF
		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		oItem:nDepth := Funcs.Val(cValue)

		oItem:aProperties:Add("Caption" , VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos))

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos) // Menu ID
		oItem:aProperties:Add("OriginalDefineValue" , cValue:Trim())

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos)
		nAt := cValue:IndexOf('\t') + 1
		IF nAt != 0
			oItem:aProperties:Add("EventName" , Funcs.Left(cValue , (DWORD)nAt - 1):Trim())
			oItem:aProperties:Add("ID" , SubStr(cValue , nAt + 1):Trim())
		ELSE
			oItem:aProperties:Add("EventName" , cValue:Trim())
			oItem:aProperties:Add("ID" , "")
		END IF

		oItem:aProperties:Add("Description" , VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos))
		oItem:aProperties:Add("HelpContext" , VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos))
		oItem:aProperties:Add("Accelerator" , VOMedItem.__ReadNextVNMnuAccel(aBytes , REF nPos):ToString())

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		nValue := Funcs.Val(cValue)
		oItem:aProperties:Add("Enabled" , iif((nValue & 2) != 0 , "Yes" , "No"))
		oItem:aProperties:Add("Checked" , iif((nValue & 1) != 0 , "Yes" , "No"))

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		IF !cValue == "-1"
			nButtonID := Funcs.Val(cValue)
			IF nButtonID >= 1  .and. nButtonID < VOMenuProperties.VOMenuToolBar:Count + 1
				oItem:aProperties:Add("ButtonBmp" , VOMenuProperties.VOMenuToolBar:GetName(nButtonID - 1))
			ELSE
				oItem:aProperties:Add("ButtonBmp" , "")
			END IF
		ELSE
			oItem:aProperties:Add("ButtonBmp" , "")
		ENDIF

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos)
		IF cValue:Length > 8
			cValue := cValue:Substring(8)
		ELSE
			cValue := ""
		ENDIF
		oItem:aProperties:Add("ButtonCaption" , cValue)

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos)
		IF cValue:Length > 8
			cValue := cValue:Substring(8)
		ELSE
			cValue := ""
		ENDIF
		oItem:aProperties:Add("ButtonToolTip" , cValue)

		cValue := VOMedItem.__ReadNextVNMnuString(aBytes , REF nPos , FALSE)
		oItem:aProperties:Add("ButtonPos" , cValue)

	RETURN oItem
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
	CONSTRUCTOR()
		SUPER()
		SELF:cKey := ""
	RETURN
	ACCESS Key AS STRING
	RETURN SELF:cKey
	ASSIGN Key(c AS STRING)
	SELF:cKey := c
	ACCESS Control AS LOGIC
	RETURN SELF:lControl
	ASSIGN Control (l AS LOGIC)
	SELF:lControl := l
	ACCESS Shift AS LOGIC
	RETURN SELF:lShift
	ASSIGN Shift (l AS LOGIC)
	SELF:lShift := l
	ACCESS Alt AS LOGIC
	RETURN SELF:lAlt
	ASSIGN Alt(l AS LOGIC)
	SELF:lAlt := l
	ACCESS IsEmpty AS LOGIC
	RETURN SELF:cKey:Trim():Length == 0

	OVERRIDE METHOD ToString() AS STRING
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

STATIC CLASS VOMenuProperties
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

		AccelKeys := oCollection


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

		VOMenuToolBar := oCollection

		LOCAL oBuilder AS System.Text.StringBuilder
		oBuilder := System.Text.StringBuilder{1000}
		FOR n := 0 UPTO VOMenuToolBar:Count - 1
			IF n != 0
				oBuilder:Append(",")
			END IF
			oBuilder:Append(VOMenuToolBar:GetName(n))
		NEXT
		VOMenuToolBarString := oBuilder:ToString()
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

CLASS NameValueCollection
	PROTECT aCollection AS ArrayList
	CONSTRUCTOR()
		SELF:aCollection := ArrayList{}
	RETURN
	METHOD Clear() AS VOID
		SELF:aCollection:Clear()
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
	METHOD GetValue(cName AS STRING) AS OBJECT
		LOCAL nIndex AS INT
		nIndex := SELF:GetNameIndex(cName)
		IF nIndex != -1
			RETURN SELF:GetValue(nIndex)
		END IF
	RETURN NULL
	METHOD SetName(nIndex AS INT , cName AS STRING) AS VOID
		LOCAL oValue AS OBJECT
		oValue := SELF:Get(nIndex):Value
		SELF:aCollection[nIndex] := NameValue{cName , oValue}
	RETURN
	METHOD SetValue(nIndex AS INT , oValue AS OBJECT) AS VOID
		LOCAL cName AS STRING
		cName := SELF:Get(nIndex):Name
		SELF:aCollection[nIndex] := NameValue{cName , oValue}
	RETURN
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

	METHOD ContainsName(cName AS STRING) AS LOGIC
	RETURN SELF:GetNameIndex(cName) != -1

	VIRTUAL ACCESS Count() AS INT
	RETURN SELF:aCollection:Count
END CLASS

STATIC CLASS Funcs
	STATIC METHOD Val(cValue AS STRING) AS INT
		LOCAL nRet AS INT
		Int32.TryParse(cValue , OUT nRet)
	RETURN nRet

	STATIC METHOD Left(c AS STRING , dwLen AS INT) AS STRING
	RETURN Funcs.Left(c , (DWORD)dwLen)
	STATIC METHOD Left(c AS STRING , dwLen AS DWORD) AS STRING
	RETURN iif(dwLen >= c:Length , c , c:Substring(0 , (INT)dwLen ))

	STATIC METHOD SubStr(c AS STRING , nStart AS DWORD , dwLen AS DWORD) AS STRING
	RETURN Funcs.SubStr(c , (INT)nStart , (INT)dwLen)
	STATIC METHOD SubStr(c AS STRING , nStart AS INT , dwLen AS INT) AS STRING
		LOCAL cReturn AS STRING

		cReturn := ""

		IF nStart == 0
			nStart := 1
		ENDIF

		IF nStart < 0
			nStart := c:Length+nStart+1
		ENDIF

		IF dwLen < 0
			dwLen := c:Length
		ENDIF

		IF nStart <= c:Length .and. nStart > 0
			dwLen := Math.Min( c:Length - nStart + 1, dwLen )
			cReturn := c:Substring( nStart - 1, dwLen )
		ENDIF

	RETURN cReturn

	STATIC METHOD SubStr(c AS STRING , nStart AS DWORD) AS STRING
	RETURN Funcs.SubStr(c , (INT)nStart)
	STATIC METHOD SubStr(c AS STRING , nStart AS INT) AS STRING
		LOCAL cReturn AS STRING

		cReturn := ""

		IF nStart == 0
			nStart := 1
		ENDIF

		IF nStart < 0
			nStart := c:Length+nStart+1
		ENDIF

		LOCAL nLength AS INT
		IF nStart <= c:Length .and. nStart > 0
			nLength := Math.Min( c:Length - nStart + 1, c:Length )
			cReturn := c:Substring( nStart - 1, nLength )
		ENDIF

	RETURN cReturn
END CLASS
