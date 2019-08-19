#using System.Collections
#using System.Text
#using System.Xml
#using System.IO

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
	EXPORT nNoSave AS BYTE
	EXPORT nDeleted AS INT
	EXPORT dStyles AS DWORD
	EXPORT dExStyles AS DWORD
	
	EXPORT cProperties AS STRING
	EXPORT aProperties AS NameValueCollection
	EXPORT aTabPages AS ArrayList
	CONSTRUCTOR()
		SELF:aProperties := NameValueCollection{}
		SELF:aTabPages := ArrayList{}
	RETURN
END CLASS


PARTIAL CLASS VOWindowEditor INHERIT WindowDesignerBase

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
/*	METHOD __WriteString_unused(oWriter AS BinaryWriter , cString AS STRING , nLength AS INT) AS VOID
		LOCAL aBytes AS BYTE[]
		aBytes := BYTE[]{nLength}
		IF cString:Length < nLength
			nLength := cString:Length
		ENDIF
		System.Text.Encoding.Default:GetBytes(cString , 0 , nLength , aBytes , 0)
		oWriter:Write(aBytes)
	RETURN*/

	METHOD InitializeNew(cName AS STRING) AS LOGIC
		LOCAL oDlg AS WindowTypeSelectDlg
		LOCAL lSuccess AS LOGIC

		oDlg := WindowTypeSelectDlg{}
		IF oDlg:ShowDialog() == System.Windows.Forms.DialogResult.OK
			lSuccess := SELF:CreateNewWindow(oDlg:cName , cName)
			IF lSuccess
				LOCAL oTemplate AS VOControlTemplate
				LOCAL oItem AS VOWEDItem
				LOCAL n AS INT
				oTemplate := VOWindowEditorTemplate.Get(oDlg:cName)
				oItem := VOWEDItem{}
				oItem:cClass := oTemplate:cFullClass
				FOR n := 0 UPTO oTemplate:aStyles:Count - 1
					oItem:dStyles += VODefines.GetDefineValue(oTemplate:aStyles[n])
				NEXT
				FOR n := 0 UPTO oTemplate:aExStyles:Count - 1
					oItem:dExStyles += VODefines.GetDefineValue(oTemplate:aExStyles[n])
				NEXT
				SELF:__ReadVNFrmProperties(oItem , SELF:oWindowDesign)
			END IF
		ENDIF
	RETURN lSuccess

	METHOD OpenXml(cFileName AS STRING , lFileIsXml REF LOGIC) AS LOGIC
		LOCAL oNode , oWindowNode AS XmlNode
		LOCAL oDocument AS XmlDocument
		LOCAL lNewWindow AS LOGIC
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
		lFileIsXml := TRUE
		
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

		SELF:Reset()
		SELF:BeginAction()
		SELF:lLoading := TRUE

		oWindowNode := oNode:FirstChild
		
		IF oWindowNode != NULL .and. oWindowNode:Name:ToUpper() == "VOWINDOW" .and. oWindowNode:ChildNodes:Count == 0 // empty template

				TRY
					lSuccess := SELF:InitializeNew(oWindowNode:Attributes:GetNamedItem("Name"):InnerText)
				END TRY
				lNewWindow := TRUE

		ELSE
		
			DO WHILE oWindowNode != NULL
				IF oWindowNode:Name:ToUpper() == "VOWINDOW"
					lSuccess := SELF:OpenXml(oWindowNode)
					EXIT
				END IF
				oWindowNode := oWindowNode:NextSibling
			END DO

		END IF

		IF lSuccess
			SELF:EndAction()
			SELF:lLoading := FALSE
			SELF:ArrangeControlOrder()
			SELF:HandleViewModeChange()
			SELF:ClearUndoBuffer()
			SELF:oToolBox:SetViewMode(SELF:ViewMode)
			IF lNewWindow
				SELF:nActionSaved := -1
			END IF
		ELSE
			SELF:Reset()
		ENDIF

	RETURN lSuccess

	METHOD OpenXml(oBaseNode AS XmlNode) AS LOGIC
		LOCAL oDesign AS DesignWindowItem
		LOCAL oItem AS VOWEDItem
		LOCAL cGuid AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL oNode AS XmlNode

		oNode := oBaseNode:FirstChild
		TRY
			DO WHILE oNode != NULL
				IF oNode:Name:ToUpper() == "WINDOW" .or. oNode:Name:ToUpper() == "CONTROL"
					oItem := SELF:OpenXmlItem(oNode)
					IF oNode:Name:ToUpper() == "WINDOW"
						IF .not. SELF:CreateNewWindow(oItem:cControl , "Name")
							THROW VODesignersException{"Could not find template definition for window type: " + oItem:cControl}
						ENDIF
						cGuid := SELF:oWindowDesign:cGuid
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , oItem:cName})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , oItem:cCaption})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , oItem:nWidth})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , oItem:nHeight})
						SELF:__ReadVNFrmProperties(oItem , SELF:oWindowDesign)
					ELSE
						cGuid := Guid.NewGuid():ToString()
						oDesign := (DesignWindowItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{cGuid , oItem:cControl , ""})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , oItem:cName})
						IF oDesign:GetProperty("Caption") != NULL
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , oItem:cCaption})
						END IF
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , oItem:nLeft})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , oItem:nTop})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , oItem:nWidth})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , oItem:nHeight})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , oItem:nHeight})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "__Order" , oItem:nOrder})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , oItem:nBrowseIndex})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseSize" , oItem:nBrowseSize})
		
						SELF:__ReadVNFrmProperties(oItem , oDesign)
						IF oItem:nDeleted == 2
							oItem:nDeleted := 0
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , -1})
						END IF
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Deleted" , (Int32)oItem:nDeleted})
						IF oDesign:Column != NULL .and. oDesign:BrowseIndex != -1
							oDesign:Column:Width := oDesign:BrowseSize * 6
						END IF
						IF SELF:ViewMode == ViewMode.Browse
							IF oDesign:Column != NULL
								oDesign:Column:Visible := oDesign:BrowseIndex != -1
							END IF
							oDesign:Control:Hide()
						ELSE
							IF oDesign:Column != NULL
								oDesign:Column:Visible := FALSE
							END IF
						END IF
					END IF
				END IF
				oNode := oNode:NextSibling
			END DO
			lSuccess := TRUE

		CATCH e AS VODesignersException
			
			Funcs.WarningBox( "Error loading xml window definition." + e"'\r\n\r\n" + e:Message )
			
		CATCH e AS Exception 
			
			Funcs.ErrorBox( "Error loading xml window definition." + e"'\r\n\r\n" + e:ToString() )
		END TRY

	RETURN lSuccess

	METHOD OpenXmlItem(oBaseNode AS XmlNode) AS VOWEDItem
		LOCAL oPage AS VOTabPageOptions
		LOCAL oPageNode AS XmlNode
		LOCAL oPropNode AS XmlNode
		LOCAL oItem AS VOWEDItem
		LOCAL oNode AS XmlNode
		LOCAL cValue AS STRING
		LOCAL cName AS STRING

		oItem := VOWEDItem{}
		oNode := oBaseNode:FirstChild
		DO WHILE oNode != NULL
			cName := oNode:Name:ToUpper():Trim()
			cValue := oNode:InnerText
			DO CASE
			CASE cName == "NAME"
				oItem:cName := cValue
			CASE cName == "CLASS"
				oItem:cClass := cValue
				LOCAL nAt AS INT
				nAt := cValue:LastIndexOf(':')
				IF nAt != -1
					cValue := cValue:Substring(nAt + 1)
				ENDIF
				oItem:cControl := cValue
			CASE cName == "CAPTION"
				oItem:cCaption := cValue
			CASE cName == "ORDER"
				oItem:nOrder := Int32.Parse(cValue)
			CASE cName == "LEFT"
				oItem:nLeft := Int32.Parse(cValue)
			CASE cName == "TOP"
				oItem:nTop := Int32.Parse(cValue)
			CASE cName == "WIDTH"
				oItem:nWidth := Int32.Parse(cValue)
			CASE cName == "HEIGHT"
				oItem:nHeight := Int32.Parse(cValue)
			CASE cName == "BROWSEINDEX"
				oItem:nBrowseIndex := Int32.Parse(cValue)
			CASE cName == "BROWSESIZE"
				oItem:nBrowseSize := Int32.Parse(cValue)
			CASE cName == "SUBFORM"
				oItem:lSubForm := cValue:ToUpper() == "YES"
			CASE cName == "NOSAVE"
				oItem:nNoSave := iif(cValue:ToUpper() == "YES" , 1 , 0)
			CASE cName == "DELETED"
				oItem:nDeleted := Int32.Parse(cValue)
			CASE cName == "STYLES"
				oItem:dStyles := UInt32.Parse(cValue)
			CASE cName == "EXSTYLES"
				oItem:dExStyles := UInt32.Parse(cValue)
			CASE cName == "EXPROPERTIES"
				oPropNode := oNode:FirstChild
				DO WHILE oPropNode != NULL
					oItem:aProperties:Add(oPropNode:Name , oPropNode:InnerText)
					oPropNode := oPropNode:NextSibling
				END DO
			CASE cName == "TABPAGES"
				oPageNode := oNode:FirstChild
				DO WHILE oPageNode != NULL
					IF oPageNode:Name:ToUpper() == "TABPAGE"
						oPage := VOTabPageOptions{}
						oItem:aTabPages:Add(oPage)
						oPropNode := oPageNode:FirstChild
						DO WHILE oPropNode != NULL
							cName := oPropNode:Name:ToUpper():Trim()
							cValue := oPropNode:InnerText
							DO CASE
							CASE cName == "NAME"
								oPage:cName := cValue
							CASE cName == "CAPTION"
								oPage:cCaption := cValue
							CASE cName == "DATAAWARE"
								oPage:lDataAware := cValue:ToUpper() == "YES"
							END CASE
							oPropNode := oPropNode:NextSibling
						END DO
						oPageNode := oPageNode:NextSibling
					END IF
				END DO
			END CASE
			oNode := oNode:NextSibling
		END DO
		
		SELF:AdjustStylesOnLoad(oItem)
		
	RETURN oItem
	
	
	METHOD OpenVNfrm(cFileName AS STRING) AS LOGIC
		LOCAL oReader AS BinaryReader
		LOCAL oDesign AS DesignWindowItem
		LOCAL oItem AS VOWEDItem
		LOCAL cGuid AS STRING
		LOCAL lError AS LOGIC
		LOCAL lFileIsXml AS LOGIC
		
		IF SELF:OpenXml(cFileName , lFileIsXml)
			RETURN TRUE
		ELSEIF lFileIsXml
			RETURN FALSE
		END IF

		SELF:Reset()

		SELF:BeginAction()
		SELF:lLoading := TRUE

		TRY
			oReader := BinaryReader{File.Open(cFileName , FileMode.Open , FileAccess.Read) , System.Text.Encoding.GetEncoding(0)}
			oItem := SELF:__ReadVNFrmControl(oReader)
	
			IF .not. SELF:CreateNewWindow(oItem:cControl , "Name")
				THROW VODesignersException{"Could not find template definition for window type: " + oItem:cControl}
			ENDIF
			cGuid := SELF:oWindowDesign:cGuid
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , oItem:cName})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , oItem:cCaption})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , (INT)oItem:nWidth})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , (INT)oItem:nHeight})
	
			SELF:__ReadVNFrmProperties(oItem , SELF:oWindowDesign)

			DO WHILE oReader:BaseStream:Position < oReader:BaseStream:Length - 50
				TRY
					oItem := SELF:__ReadVNFrmControl(oReader)
				CATCH
					lError := TRUE
				END TRY
				IF lError
					EXIT
				ENDIF
//				System.Windows.Forms.MessageBox.Show(oItem:cName + " " + oItem:nBrowseSize:ToString() + " " + oItem:nDeleted:ToString() , oItem:nBrowseIndex:ToString())
				cGuid := Guid.NewGuid():ToString()
//				SELF:oSurface:Parent:Text := oItem:cControl
				oDesign := (DesignWindowItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{cGuid , oItem:cControl , ""})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , oItem:cName})
				IF oDesign:GetProperty("Caption") != NULL
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , oItem:cCaption})
				END IF
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , (INT)oItem:nLeft})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , (INT)oItem:nTop})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , (INT)oItem:nWidth})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , (INT)oItem:nHeight})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , (INT)oItem:nHeight})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "__Order" , (INT)oItem:nOrder})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , (Int32)oItem:nBrowseIndex})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseSize" , (Int32)oItem:nBrowseSize})

				SELF:__ReadVNFrmProperties(oItem , oDesign)
//				oDesign:Deleted := (Int32)oItem:nDeleted
				IF oItem:nDeleted == 2
					oItem:nDeleted := 0
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , -1})
				END IF
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Deleted" , (Int32)oItem:nDeleted})
				IF oDesign:Column != NULL .and. oDesign:BrowseIndex != -1
					oDesign:Column:Width := oDesign:BrowseSize * 6
				END IF
				IF SELF:ViewMode == ViewMode.Browse
					IF oDesign:Column != NULL
						oDesign:Column:Visible := oDesign:BrowseIndex != -1
					END IF
					oDesign:Control:Hide()
				ELSE
					IF oDesign:Column != NULL
						oDesign:Column:Visible := FALSE
					END IF
				END IF
				
			END DO
			lError := FALSE

		CATCH e AS VODesignersException
			
			lError := TRUE
			Funcs.WarningBox( "Error loading '" + cFileName + e"'\r\n\r\n" + e:Message )
			
		CATCH e AS Exception 
			
			lError := TRUE
			Funcs.ErrorBox( "Error loading '" + cFileName + e"'\r\n\r\n" + e:ToString() )
			
		FINALLY
		
			IF oReader != NULL
				oReader:Close()
			ENDIF

		END TRY

		IF lError
			SELF:Reset()
		ELSE
			SELF:EndAction()
			SELF:lLoading := FALSE
			SELF:ArrangeControlOrder()
			SELF:HandleViewModeChange()
			SELF:ClearUndoBuffer()
			SELF:oToolBox:SetViewMode(SELF:ViewMode)
		ENDIF

	RETURN !lError

	METHOD __ReadVNFrmProperties(oItem AS VOWEDItem , oDesign AS DesignWindowItem) AS VOID
		LOCAL oProp AS VODesignProperty
		LOCAL dTryValue AS DWORD
		LOCAL dStyles AS DWORD
		LOCAL cValue AS STRING
		LOCAL dValue AS DWORD
		LOCAL n,m AS INT

		FOR n := 0 UPTO oItem:aProperties:Count - 1
//			oProp := oDesign:GetProperty(oItem:aProperties:GetName(n))
			oProp := oDesign:GetPropertyByMember(oItem:aProperties:GetName(n))
			IF oProp == NULL .and. oItem:aProperties:GetName(n) == "NewFont"
				oProp := oDesign:GetPropertyByMember("Font")
			ENDIF
			IF oProp == NULL .and. oItem:aProperties:GetName(n) == "DBInhFrom"
				oProp := oDesign:GetProperty("_DBInhFrom")
			ENDIF
			IF oProp == NULL .and. oItem:aProperties:GetName(n) == "DCInhFrom"
				oProp := oDesign:GetProperty("_DCInhFrom")
			ENDIF
			IF oProp != NULL
				cValue := oItem:aProperties:GetValue(n):ToString()
				IF oProp:lMultiple
					LOCAL nMultiPos AS INT
					m := 0
					nMultiPos := 1
					DO WHILE m < cValue:Length
						IF cValue[m] == ','
							nMultiPos ++
							m ++
						ELSE
							cValue := cValue:Substring(m)
							DO WHILE cValue:Length > 1 .and. cValue[cValue:Length - 1] == ','
								cValue := cValue:Substring(0 , cValue:Length - 1)
							END DO
							EXIT
						ENDIF
					END DO
					oProp := oDesign:GetPropertyByMemberAndPos(oItem:aProperties:GetName(n) , nMultiPos)
					IF oProp == NULL
						LOOP
					ENDIF
					IF oProp:Name == "Name"
						LOOP
					ENDIF
				ENDIF

				DO CASE
				CASE oProp:Type == PropertyType.Text
					IF cValue:Length >= 2 .and. cValue[0] == '"' .and. cValue[cValue:Length - 1] == '"'
						cValue := cValue:Substring(1 , cValue:Length - 2)
					ENDIF
					oProp:Value := cValue
				CASE oProp:Type == PropertyType.Numeric
					oProp:Value := Convert.ToInt32(cValue)
				OTHERWISE
					oProp:Value := oItem:aProperties:GetValue(n):ToString()
				END CASE
				
				IF oProp:cSpecialClass != NULL
					SELF:ApplyProperty(oDesign , oProp)
				ENDIF
				
			ENDIF
		NEXT
		
//		IF _And( VODefines.GetDefineValue("WS_VISIBLE") , oItem:dStyles) != 0
		IF ( VODefines.GetDefineValue("WS_VISIBLE") & oItem:dStyles) != 0
			oDesign:GetProperty("_Visible"):Value := "NO"
		ENDIF
		IF oItem:nNoSave == 1
			oDesign:GetProperty("_GenCode"):Value := "NO"
		ENDIF

//		System.Windows.Forms.MessageBox.Show(AsHexString(oItem:dStyles))

		LOCAL cUpper AS STRING
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			oProp := (VODesignProperty)oDesign:aProperties[n]
			IF oProp:eVOStyle != VOStyle.None
				cUpper := oProp:Name:ToUpperInvariant()
				IF oProp:eVOStyle == VOStyle.Style
					dStyles := oItem:dStyles
				ELSE
					dStyles := oItem:dExStyles
				ENDIF
				dValue := 0
				IF oProp:cEnumType == "BOOL"
					dValue := VODefines.GetDefineValue(oProp:aEnumValues[0])
					IF (dValue & dStyles) != dValue
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , oProp:Name , 1})
						dValue := 0
					ELSE
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , oProp:Name , 0})
						// TODO: Hack, need to find a better, general way. Below have the default values (no change) so ApplyProperty() is not being invoked
						IF cUpper== "TEXT LEFT" .or. cUpper == "PUSH LIKE"  .or. cUpper == "FLAT"
							SELF:ApplyProperty(oDesign , oProp)
						END IF
					ENDIF
				ELSEIF oProp:aEnumValues:Count != 0 // TODO
					FOR m := 0 UPTO oProp:aEnumValues:Count - 1
						dTryValue := VODefines.GetDefineValue(oProp:aEnumValues[m])
						IF dTryValue > dValue .and. (dTryValue & dStyles) == dTryValue
							dValue := dTryValue
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , oProp:Name , m})
							// TODO: Hack, need to find a better, general way. Below have the default values (no change) so ApplyProperty() is not being invoked
							IF cUpper == "COMBOBOX TYPE"
								SELF:ApplyProperty(oDesign , oProp)
							END IF
						ENDIF
					NEXT
				ENDIF
				
				// Huge HACK for the Type/Tab Key insanity in VO. If we just handled the Type property here, we let 
				// also the Tab Key property read the style. Might be possible never to subtract currently read style
				// from the dStyles/dExStyles var, but other properties might be depending on that...
				IF .not. (cUpper == "TYPE" .and. oItem:cClass != NULL .and. oItem:cClass:StartsWith("FORM:DIALOGWINDOW"))
					IF oProp:eVOStyle == VOStyle.Style
						oItem:dStyles -= dValue
					ELSE
						oItem:dExStyles -= dValue
					ENDIF
				END IF
			END IF
		NEXT
		
		IF oDesign:IsTabControl
			LOCAL oTabControl AS DesignTabControl
			oTabControl := (DesignTabControl)oDesign:Control
			DO WHILE oTabControl:TabPages:Count < oItem:aTabPages:Count
				oTabControl:AddPage()
			END DO
			FOR n := 0 UPTO oItem:aTabPages:Count - 1
				oTabControl:SetTabPageOptions(n + 1 , (VOTabPageOptions)oItem:aTabPages[n])
			NEXT
			
		END IF
	RETURN

	METHOD __ReadVNFrmControl(oReader AS BinaryReader) AS VOWEDItem
		LOCAL nLeft,nRight,nTop,nBottom AS Int
		LOCAL nAt , nPropLength AS INT
		LOCAL cName , cValue AS STRING
		LOCAL cControl AS STRING
		LOCAL oItem AS VOWEDItem
		
		oItem := VOWEDItem{}
		oItem:cName := SELF:__ReadString(oReader , 63)
		oItem:nOrder := oReader:ReadInt16()
		
		cControl := SELF:__ReadString(oReader , 65)
		oItem:cClass := cControl
		nAt := cControl:LastIndexOf(':')
		IF nAt != -1
			cControl := cControl:Substring(nAt + 1)
		ENDIF
		oItem:cControl := cControl
		oItem:cCaption := SELF:__ReadString(oReader , 65)
	
		nLeft := oReader:ReadInt16() +  1
		nTop := oReader:ReadInt16() +  1
		nRight := oReader:ReadInt16() +  1
		nBottom := oReader:ReadInt16() +  1
		oItem:nLeft := nLeft
		oItem:nTop := nTop
		oItem:nWidth := nRight - nLeft
		oItem:nHeight := nBottom - nTop
		
		oItem:nDeleted := oReader:ReadInt16()
		oItem:nBrowseIndex := oReader:ReadInt16()
		oItem:nBrowseSize := oReader:ReadInt16()
		oItem:lSubForm := oReader:ReadByte() == 1
		oItem:nNoSave := oReader:ReadByte()
		oItem:dStyles := oReader:ReadUInt32()
		oItem:dExStyles := oReader:ReadUInt32()
		
		SELF:AdjustStylesOnLoad(oItem)
		
		nPropLength := oReader:ReadUInt16()
		
		cName := "";cValue := ""
		LOCAL aBytes AS BYTE[]
		LOCAL lInValue AS LOGIC
		LOCAL n AS INT
		aBytes := oReader:ReadBytes(nPropLength)
		
		FOR n := 1 UPTO aBytes:Length
			IF aBytes[n] == 0
				oItem:aProperties:Add(cName , cValue)
//				System.Windows.Forms.MessageBox.Show(cName , cValue)
				cName := "";cValue := ""
				lInValue := FALSE
			ELSE
				DO CASE
//				CASE (Char)aBytes[n] == '"'
				CASE (Char)aBytes[n] == '(' .or. (Char)aBytes[n] == ')'
					lInValue := TRUE
				CASE !lInValue
//					cName += ((Char)aBytes[n]):ToString()
					cName += Encoding.Default:GetString(aBytes , n - 1 , 1) // TODO: This really needs improvement
				OTHERWISE
//					cValue += ((Char)aBytes[n]):ToString()
					cValue += Encoding.Default:GetString(aBytes , n - 1 , 1)
				ENDCASE
			ENDIF
		NEXT
		IF cName != ""
			oItem:aProperties:Add(cName , cValue)
//			System.Windows.Forms.MessageBox.Show(cName , cValue)
		END IF
//		oItem:cProperties := SELF:__ReadString(oReader , nPropLength)

		IF oItem:cControl == "TABCONTROL"
//			nPropLength += 26
//			oReader:ReadBytes(24)
			LOCAL nPages AS INT
			LOCAL nByte AS BYTE
			LOCAL nZeroCount AS INT
			LOCAL cTab AS STRING
			LOCAL oPage AS VOTabPageOptions
			nPages := oReader:ReadInt16()
//			System.Windows.Forms.MessageBox.Show(nPages:ToString())
			FOR n := 1 UPTO nPages
				oPage := VOTabPageOptions{}
				oItem:aTabPages:Add(oPage)
				nZeroCount := 0
				cTab := ""
				DO WHILE nZeroCount < 2
					nByte := oReader:ReadByte()
					IF nByte == 0
						nZeroCount ++
						IF nZeroCount == 1
							oPage:cCaption := cTab
						ELSE
							oPage:cName := cTab
						ENDIF
//						System.Windows.Forms.MessageBox.Show(cTab , cTab)
						cTab := ""
					ELSE
//						cTab += Chr((DWORD)nByte)
//						cTab += ((Char)(DWORD)nByte):ToString()
						cTab += Encoding.Default:GetString(<BYTE>{nByte} , 0 , 1) // TODO: This really, really, really needs improvement
					ENDIF
				END DO
				oPage:lDataAware := oReader:ReadByte() == 89
//				oReader:ReadByte()
//				System.Windows.Forms.MessageBox.Show(cTab , oReader:ReadByte():ToString())
			NEXT
			
		ENDIF

	RETURN oItem
	
	METHOD AdjustStylesOnLoad(oItem AS VOWEDItem) AS VOID
		// Workaround to include styles that were not being saved in builds <= 161.1
		DO CASE
		CASE oItem:cClass:StartsWith("CONTROL:SCROLLBAR:VERTICALSCROLLBAR")
			oItem:dStyles := _Or(oItem:dStyles , VODefines.GetDefineValue("SBS_VERT"))
		CASE oItem:cClass:StartsWith("CONTROL:SCROLLBAR:SLIDER:VERTICALSLIDER")
			oItem:dStyles := _Or(oItem:dStyles , VODefines.GetDefineValue("TBS_VERT"))
		CASE oItem:cClass:StartsWith("CONTROL:SCROLLBAR:SPINNER:HORIZONTALSPINNER")
			oItem:dStyles := _Or(oItem:dStyles , VODefines.GetDefineValue("UDS_HORZ"))
		CASE oItem:cClass:StartsWith("CONTROL:TEXTCONTROL:EDIT:MULTILINEEDIT")
			oItem:dStyles := _Or(oItem:dStyles , VODefines.GetDefineValue("ES_MULTILINE"))
		END CASE
	RETURN
	


/*	METHOD SaveVNfrm_unused(oStream AS FileStream) AS VOID
		LOCAL oWriter AS BinaryWriter
		LOCAL oDesign AS DesignWindowItem
		LOCAL aDesign AS ArrayList
		LOCAL n AS INT

		oWriter := BinaryWriter{oStream , System.Text.Encoding.GetEncoding(0)}
		SELF:__WriteFrmControl(SELF:oWindowDesign , oWriter)
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			SELF:__WriteFrmControl(oDesign , oWriter)
		NEXT
		oStream:Flush()
		oStream:Close()
		
	RETURN

	METHOD __WriteFrmControl_unused(oDesign AS DesignWindowItem , oWriter AS BinaryWriter) AS VOID
		LOCAL oItem AS VOWEDItem
		LOCAL oProp AS VODesignProperty
		LOCAL n,m AS INT
		
		oItem := VOWEDItem{}
		oItem:cName := oDesign:Name
		oItem:nOrder := (Int16)oDesign:Order
		oItem:cControl := oDesign:cControl
		oItem:cCaption := oDesign:Caption
	
		oItem:nLeft := (Int16)(INT)oDesign:GetProperty("_Left"):Value - 1
		oItem:nTop := (Int16)(INT)oDesign:GetProperty("_Top"):Value - 1
		oItem:nWidth := (Int16)(INT)oDesign:GetProperty("_Width"):Value
		oItem:nHeight := (Int16)(INT)oDesign:GetProperty("_Height"):Value
		
		oItem:nBrowseIndex := (Int16)oDesign:BrowseIndex
		oItem:nBrowseSize := (Int16)oDesign:BrowseSize

//		oItem:lNoSave := FALSE
		oItem:nNoSave := iif(oDesign:GetProperty("_GenCode"):TextValue:ToUpper() == "NO" , 1 , 0)
		
		oItem:dStyles := oDesign:GetVOStylesValue(VOStyle.Style)
		oItem:dExStyles := oDesign:GetVOStylesValue(VOStyle.ExStyle)
		
		oItem:cProperties := ""
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			oProp := (VODesignProperty)oDesign:aProperties[n]
			IF oProp:eVOStyle == VOStyle.None
				IF !oProp:IsAuto .and. oProp:Type != PropertyType.Callback .and. oProp:Name[0] != '_'
					IF oProp:cSpecialClass == "Font"
						oItem:cProperties += "NewFont("
					ELSE
						oItem:cProperties += oProp:cMember + "("
					ENDIF
					IF oProp:lMultiple
						FOR m := 1 UPTO oProp:nMultiPos - 1
							oItem:cProperties += ","
						NEXT
						oItem:cProperties += oProp:SaveValue
					ELSE
						oItem:cProperties += oProp:SaveValue
					ENDIF
					oItem:cProperties += ")" + ((Char)0):ToString()
				ENDIF
			END IF
		NEXT
		
		oProp := oDesign:GetProperty("_DBInhFrom")
		IF oProp != NULL .and. !oProp:IsAuto
			oItem:cProperties += "DBInhFrom(" + oProp:TextValue + ")" + ((Char)0):ToString()
		END IF
		oProp := oDesign:GetProperty("_DCInhFrom")
		IF oProp != NULL .and. !oProp:IsAuto
			oItem:cProperties += "DCInhFrom(" + oProp:TextValue + ")" + ((Char)0):ToString()
		END IF
		
//		System.Windows.Forms.MessageBox.Show(AsHexString(oItem:dStyles))
		
		SELF:__WriteString(oWriter , oItem:cName , 63)
		oWriter:Write((Int16)oItem:nOrder)
		SELF:__WriteString(oWriter , oItem:cControl , 65)
		SELF:__WriteString(oWriter , oItem:cCaption , 65)
		oWriter:Write((Int16)oItem:nLeft)
		oWriter:Write((Int16)oItem:nTop)
		oWriter:Write((Int16)(oItem:nLeft + oItem:nWidth))
		oWriter:Write((Int16)(oItem:nTop + oItem:nHeight))
	
//		oWriter:Write((Int16)0) // iDeleted
		oWriter:Write((Int16)oDesign:Deleted)

		oWriter:Write((Int16)oItem:nBrowseIndex)
		oWriter:Write((Int16)oItem:nBrowseSize)

		oWriter:Write((BYTE)0) // bSubForm
		oWriter:Write(oItem:nNoSave) // NoSave

		oWriter:Write(oItem:dStyles)
		oWriter:Write(oItem:dExStyles)

//		oWriter:Write((WORD)0) // Proplen
		oWriter:Write((WORD)oItem:cProperties:Length) // Proplen
		SELF:__WriteString(oWriter , oItem:cProperties , oItem:cProperties:Length)

		IF oItem:cControl == "TABCONTROL"
			LOCAL oTabControl AS DesignTabControl
			LOCAL nPages AS INT
			LOCAL oPage AS VOTabPageOptions
			oTabControl := (DesignTabControl)oDesign:Control
			nPages := oTabControl:TabPages:Count
			oWriter:Write((WORD)nPages)
			FOR n := 1 UPTO nPages
				oPage := oTabControl:GetTabPageOptions(n)
				SELF:__WriteString(oWriter , oPage:cCaption , oPage:cCaption:Length)
				oWriter:Write((BYTE)0)
				SELF:__WriteString(oWriter , oPage:cName , oPage:cName:Length)
				oWriter:Write((BYTE)0)
				oWriter:Write(iif( oPage:lDataAware , (BYTE)89 , (BYTE)78) )
			NEXT
		ENDIF

	RETURN
*/

	METHOD SaveToXml(oStream AS FileStream) AS LOGIC
		LOCAL oDocument AS XmlDocument
		LOCAL oDesigners AS XmlNode
		LOCAL oMainNode AS XmlNode
		LOCAL oDesign AS DesignWindowItem
		LOCAL aDesign AS ArrayList
		LOCAL lSuccess AS LOGIC
		LOCAL oNode AS XmlNode
		LOCAL n AS INT
		
		oDocument := XmlDocument{}
		oDocument:AppendChild(oDocument:CreateXmlDeclaration("1.0" , "utf-8" , NULL))

		oDesigners := oDocument:CreateElement("Designers")
		oDocument:AppendChild(oDesigners)

		oMainNode := oDocument:CreateElement("VOWindow")

		oDesign := SELF:oWindowDesign
		Funcs.ApplyNameAttribute(oDocument , oMainNode , oDesign:Name)
		oDesigners:AppendChild(oMainNode)
		
		oNode := oDocument:CreateElement("Window")
		Funcs.ApplyNameAttribute(oDocument , oNode , SELF:oWindowDesign:Name)
		oMainNode:AppendChild(oNode)
		SELF:SaveToXml(oDocument , SELF:oWindowDesign , oNode)
		
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			oNode := oDocument:CreateElement("Control")
			Funcs.ApplyNameAttribute(oDocument , oNode , oDesign:Name)
			oMainNode:AppendChild(oNode)
			SELF:SaveToXml(oDocument , oDesign , oNode)
		NEXT

		oDocument:Save(oStream)
		lSuccess := TRUE

		oStream:Flush()
		oStream:Close()
	RETURN lSuccess

	METHOD SaveToXml(oDocument AS XmlDocument , oDesign AS DesignWindowItem , oParentNode AS XmlNode) AS LOGIC
		LOCAL oGroupNode AS XmlNode
		LOCAL oItem AS VOWEDItem
		LOCAL n AS INT
		
		oItem := oDesign:GetWedItem()
		Funcs.AppendElement(oDocument , oParentNode , "Name" , oItem:cName)
		Funcs.AppendElement(oDocument , oParentNode , "Class" , oItem:cControl)
		Funcs.AppendElement(oDocument , oParentNode , "Caption" , oItem:cCaption)
		Funcs.AppendElement(oDocument , oParentNode , "Order" , oItem:nOrder)
		Funcs.AppendElement(oDocument , oParentNode , "Left" , oItem:nLeft)
		Funcs.AppendElement(oDocument , oParentNode , "Top" , oItem:nTop)
		Funcs.AppendElement(oDocument , oParentNode , "Width" , oItem:nWidth)
		Funcs.AppendElement(oDocument , oParentNode , "Height" , oItem:nHeight)
		Funcs.AppendElement(oDocument , oParentNode , "BrowseIndex" , oItem:nBrowseIndex)
		Funcs.AppendElement(oDocument , oParentNode , "BrowseSize" , oItem:nBrowseSize)
		Funcs.AppendElement(oDocument , oParentNode , "SubForm" , oItem:lSubForm)
		Funcs.AppendElement(oDocument , oParentNode , "NoSave" , oItem:nNoSave == 1)
		Funcs.AppendElement(oDocument , oParentNode , "Deleted" , oItem:nDeleted)
		Funcs.AppendElement(oDocument , oParentNode , "Styles" , oItem:dStyles:ToString())
		Funcs.AppendElement(oDocument , oParentNode , "ExStyles" , oItem:dExStyles:ToString())

		IF oItem:aProperties:Count != 0
			oGroupNode := oDocument:CreateElement("ExProperties")
			oParentNode:AppendChild(oGroupNode)
			FOR n := 0 UPTO oItem:aProperties:Count - 1
				Funcs.AppendElement(oDocument , oGroupNode , oItem:aProperties:Get(n):Name , (STRING)oItem:aProperties:Get(n):Value)
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
				Funcs.ApplyNameAttribute(oDocument , oPageNode , oPage:cName)
				oGroupNode:AppendChild(oPageNode)
				Funcs.AppendElement(oDocument , oPageNode , "Name" , oPage:cName)
				Funcs.AppendElement(oDocument , oPageNode , "Caption" , oPage:cCaption)
				Funcs.AppendElement(oDocument , oPageNode , "DataAware" , oPage:lDataAware)
			NEXT
		END IF

	RETURN TRUE


	METHOD SaveRC(oStream AS EditorStream , oCode AS CodeContents , cVhName AS STRING , lOldTransporter AS LOGIC , lRcInSameFolder AS LOGIC) AS LOGIC
		LOCAL n AS INT

		oStream:Editor:Clear()
		oStream:Editor:AddLine(e"#include \"VOWin32APILibrary.vh\"")
		IF lOldTransporter
			FOR n := 0 UPTO oCode:aDefines:Count - 1
				oStream:Editor:AddLine("#define " + oCode:aDefines[n] + " " + oCode:aDefineValues[n])
			NEXT
		ELSE
			IF lRcInSameFolder
				oStream:Editor:AddLine(e"#include \"" + cVhName + e"\"")
			ELSE
				oStream:Editor:AddLine(e"#include \"..\\" + cVhName + e"\"")
			END IF
		END IF
		oStream:Editor:AddLine("")
		FOR n := 0 UPTO oCode:aResource:Count - 1
			oStream:Editor:AddLine(oCode:aResource[n])
		NEXT
		oStream:Save()
	RETURN TRUE

	METHOD SavePrg(oDest AS EditorStream , oCode AS CodeContents , lOldTransporter AS LOGIC) AS LOGIC
		LOCAL oProp AS VODesignProperty
		LOCAL cClass AS STRING
		LOCAL cName AS STRING
		LOCAL n AS INT

		cName := SELF:oWindowDesign:Name
		cClass := cName
		IF lOldTransporter
			oDest:Editor:ReplaceDefines(oCode:aDefines , oCode:aDefineValues , FALSE)
		ELSE
			oDest:Editor:DeleteDefines(oCode:aDefines) // in case they exist, from old transporter
		ENDIF
		oDest:Editor:ReplaceEntity(cName , cName , EntityType._Class , oCode:aClass)
		oDest:Editor:ReplaceEntity(cName , cName , EntityType._Constructor , oCode:aConstructor)
		
		oProp := SELF:oWindowDesign:GetPropertyByMember("NoAcc")
//		IF oProp != NULL .and. oProp:TextValue:ToUpper() != "YES"
		IF oProp != NULL .and. oProp:TextValue:Trim() == "" .and. oProp:cPage != "_Hidden"
			LOCAL aEntity AS System.Collections.Generic.List<STRING>
			aEntity := System.Collections.Generic.List<STRING>{}
			FOR n := 0 UPTO oCode:aAccessAssign:Count - 1
				cName := oCode:aAccessAssign[n]
				aEntity:Clear()
				aEntity:Add("ACCESS " + cName)
				aEntity:Add("RETURN SELF:FieldGet( #" + cName + " )")
				oDest:Editor:AddEntity(cName , cClass , EntityType._Access , aEntity)
				aEntity:Clear()
				aEntity:Add("ASSIGN " + cName + "( uValue )")
				aEntity:Add("SELF:FieldPut( #" + cName + " , uValue )")
				aEntity:Add("RETURN")
				oDest:Editor:AddEntity(cName , cClass , EntityType._Assign , aEntity)
			NEXT
			
		ENDIF

	RETURN oDest:Save()

	METHOD SaveVh(oStream AS EditorStream , oCode AS CodeContents) AS LOGIC
		IF .not. oStream:IsValid
			RETURN FALSE
		END IF
		oStream:Editor:ReplaceDefines(oCode:aDefines, oCode:aDefineValues, TRUE)
	RETURN oStream:Save()
	
END CLASS

CLASS VODesignersException INHERIT Exception
	CONSTRUCTOR(cMessage AS STRING)
		SUPER(cMessage)
	RETURN
END CLASS

