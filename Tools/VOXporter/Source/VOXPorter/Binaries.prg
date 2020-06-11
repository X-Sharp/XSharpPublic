// Note: The code here is much as possible 1:1 copied from very old original code in XIDE, in order to avoid introducing problems
// Please do not adjust it to make it more readable/efficient, at least until we have made absolutely sure everything works as expected

#using System.Windows.Forms
#using System.Text
#using System.Collections.Generic
#using System.Collections
#using System.IO
#using System.Xml

CLASS BinaryEntity

	STATIC METHOD SaveToWed(aBytes AS BYTE[], cPrg AS STRING , cVNForm AS STRING) AS VOID
		LOCAL aItems AS List<VOWEDItem>
		LOCAL oWriter AS StreamWriter
		LOCAL oItem AS VOWEDItem
		LOCAL cWedFile AS STRING
		LOCAL cLine AS STRING
		LOCAL cForm AS STRING
		LOCAL n AS INT

		aItems := ReadFromBytes(aBytes)
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

	STATIC METHOD ReadFromBytes(aBytes AS BYTE[]) AS List<VOWEDItem>
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
				END TRY
			END TRY
			oBinary:Close()
			oMemory:Dispose()
		END IF
	RETURN aItems

	STATIC METHOD SaveToXml(cVNFrmFileName AS STRING, aBytes AS BYTE[]) AS VOID
		LOCAL aDesign AS List<VOWEDItem>
		LOCAL oStream AS FileStream
		LOCAL oDocument AS XmlDocument
		LOCAL oDesigners AS XmlNode
		LOCAL oMainNode AS XmlNode
		LOCAL oDesign AS VOWEDItem
		LOCAL oNode AS XmlNode
		LOCAL n AS INT
		
		aDesign := ReadFromBytes(aBytes)
		IF aDesign == NULL .or. aDesign:Count == 0
			RETURN
		END IF
		
		oStream := File.Create(cVNFrmFileName)
		
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
		SaveToXml(oDocument , oDesign , oNode)
		
		FOR n := 1 UPTO aDesign:Count - 1
			oDesign := (VOWEDItem)aDesign[n]
			oNode := oDocument:CreateElement("Control")
			ApplyNameAttribute(oDocument , oNode , oDesign:Name)
			oMainNode:AppendChild(oNode)
			SaveToXml(oDocument , oDesign , oNode)
		NEXT

		oDocument:Save(oStream)

		oStream:Flush()
		oStream:Close()
	RETURN

	PRIVATE STATIC METHOD SaveToXml(oDocument AS XmlDocument , oItem AS VOWEDItem , oParentNode AS XmlNode) AS LOGIC
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
				AppendElement(oDocument , oGroupNode , oItem:aProperties:Get(n):Name , (STRING)oItem:aProperties:Get(n):Value)
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

