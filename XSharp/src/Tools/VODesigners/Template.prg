#using System.Collections.Generic
#using System.Collections
#using System.Windows.Forms
#using System.Drawing
#using System.IO

INTERNAL ENUM CavoWedInfSection
	MEMBER None
	MEMBER Control
	MEMBER StdProp
	MEMBER AssignMap
	MEMBER Supplemental
	MEMBER options
END ENUM

STATIC CLASS VOWindowEditorTemplate
	STATIC PRIVATE aControls AS ArrayList
	STATIC PRIVATE aEnumTypes AS Dictionary<STRING,STRING>
	STATIC PRIVATE aAssignMap AS Dictionary<STRING,STRING>
	STATIC PRIVATE lLoaded AS LOGIC
	STATIC EXPORT aPages AS HashTable
	STATIC EXPORT GridX , GridY AS INT
	STATIC CONSTRUCTOR()

		VOWindowEditorTemplate.aControls := ArrayList{}
		VOWindowEditorTemplate.aEnumTypes := Dictionary<STRING , STRING>{}
		VOWindowEditorTemplate.aAssignMap := Dictionary<STRING , STRING>{}
		VOWindowEditorTemplate.aPages := HashTable{}

		VOWindowEditorTemplate.aEnumTypes:Add("YESNO" , "Yes,No")
		VOWindowEditorTemplate.aEnumTypes:Add("__genBOOL" , "True,False")
		VOWindowEditorTemplate.aEnumTypes:Add("__menuTOOLBARSTYLE" , "No Toolbar,Flat Toolbar,Raised Toolbar")
		VOWindowEditorTemplate.aEnumTypes:Add("__menuSHOW" , "Text,Icon,Text and Icon")
		VOWindowEditorTemplate.aEnumTypes:Add("__menuToolBarButtons" , VOMenuEditor.VOMenuToolBarString)

		VOWindowEditorTemplate.aEnumTypes:Add("__fieldspecDataTypes" , "Character,Numeric,Date,Logic,Memo,OLE,Multimedia")

//		VOWindowEditorTemplate.aEnumTypes:Add("DIALOGPOSITON" , "Auto,Center,Center Mouse")

		VOWindowEditorTemplate.aAssignMap:Add("FILLUSING" , "FillUsing")
		VOWindowEditorTemplate.aAssignMap:Add("FONT" , "Font")
		
		VOWindowEditorTemplate.GridX := 8
		VOWindowEditorTemplate.GridY := 8

	RETURN
	STATIC METHOD Load(cDirectory AS STRING) AS LOGIC
		LOCAL cCavoWed AS STRING
      LOCAL cOrigDir AS STRING
		LOCAL lSuccess AS LOGIC
		
		IF VOWindowEditorTemplate.lLoaded
			RETURN TRUE
		ENDIF
		
//		oReader := StreamReader{"C:\CAVOWED.INF"}
//		oReader := StreamReader{"CAVOWED.INF"}
//		oReader := StreamReader{"C:\CAVO28\BIN\CAVOWED.INF"}
//		cCavoWed := "C:\CAVO28\BIN\CAVOWED.INF"
//		cCavoWed := "C:\CAVO28\BIN\CAVOWED.INF"
      cOrigDir := cDirectory
        TRY
        	cCavoWed := cDirectory + "\Properties\CAVOWED.INF"
	        IF !System.IO.File.Exists(cCavoWed)
		        cCavoWed := cDirectory + "\CAVOWED.INF"
				IF !System.IO.File.Exists(cCavoWed)
					cDirectory := Directory.GetParent(cDirectory):FullName
					cCavoWed := cDirectory + "\CAVOWED.INF"
			        IF !System.IO.File.Exists(cCavoWed)
			        	cCavoWed := cDirectory + "\Properties\CAVOWED.INF"
				        IF !System.IO.File.Exists(cCavoWed) .and. Funcs.InstallTemplatesFolder != ""
				        	cCavoWed := Funcs.InstallTemplatesFolder  + "\CAVOWED.INF"
				        ENDIF
			        ENDIF
				ENDIF
			END IF
        END TRY
		IF !System.IO.File.Exists(cCavoWed)
			MessageBox.Show("File Cavowed.inf was not found, please locate it on disk." , Resources.EditorName)
		ENDIF
		DO WHILE !System.IO.File.Exists(cCavoWed)
			LOCAL oDlg AS OpenFileDialog
			oDlg := OpenFileDialog{}
			oDlg:Filter := "CavoWED files (*.inf)|*.inf"
			oDlg:Title := "Open cavowed.inf file"
			IF oDlg:ShowDialog() == DialogResult.OK
				cCavoWed := oDlg:FileName:ToLower()
            TRY
               IF cCavoWed:Contains("cavowed") .and. cCavoWed:EndsWith(".inf")
                  File.Copy(cCavoWed , cOrigDir + "\cavowed.inf" , FALSE)
               ENDIF
            END TRY
			ELSE
				RETURN FALSE
			ENDIF
		END DO
		
		IF VOWindowEditorTemplate.LoadInfFile(cCavoWed)
			VOWindowEditorTemplate.lLoaded := TRUE
			lSuccess := TRUE
		ENDIF
			
	RETURN lSuccess
	INTERNAL STATIC METHOD LoadInfFile(cCavoWed AS STRING) AS LOGIC
		LOCAL oControl AS VOControlTemplate
		LOCAL oProp AS VODesignProperty
		LOCAL oReader AS StreamReader
		LOCAL cLine , cUpper AS STRING
		LOCAL cLeft , cRight AS STRING
		LOCAL cProp , cCaption AS STRING
		LOCAL cParent AS STRING
		LOCAL cPage AS STRING
		LOCAL cType AS STRING
		LOCAL cProperType AS STRING
		LOCAL cEnumValues AS STRING
		LOCAL eSection AS CavoWedInfSection
		LOCAL aPages AS List<STRING>
		LOCAL lSuccess AS LOGIC
		LOCAL nAt AS INT
		
		eSection := CavoWedInfSection.None

		TRY

			oReader := StreamReader{cCavoWed}
			DO WHILE !oReader:Peek() == -1
				cLine := oReader:ReadLine():Trim()
				cUpper := cLine:ToUpper()
				IF cLine:Length == 0
					LOOP
				ENDIF
				cType := ""
				IF cUpper[cUpper:Length - 1] == ')'
					nAt := cUpper:LastIndexOf('(')
					IF nAt != -1
						cProperType := cLine:Substring(nAt + 1 , cLine:Length - nAt - 2)
						cType := cProperType:ToUpper()
						cUpper := cUpper:Substring(0 , nAt)
						cLine := cLine:Substring(0 , nAt)
					ENDIF
				ENDIF
				DO CASE
				CASE cLine[0] == ';' .or. cLine[0] == '/'
					LOOP
				CASE cLine[0] == '['
					cUpper := cUpper:Substring(1 , cUpper:Length - 2)
					DO CASE
					CASE cUpper:IndexOf("CONTROL") == 0 .or. cUpper:IndexOf("FORM") == 0
						eSection := CavoWedInfSection.Control
						oControl := VOControlTemplate{}
						oControl:lForm := cUpper:IndexOf("FORM") == 0
						oControl:cFullClass := cUpper
						IF VOWindowEditorTemplate.GetFromFullClass(oControl:cFullClass) == NULL
							VOWindowEditorTemplate.aControls:Add(oControl)
						END IF
						cParent := ""
						nAt := cUpper:LastIndexOf(':')
						IF nAt == -1
							oControl:aProperties:Add(VODesignProperty{"_Left","Left",NULL,PropertyType.Numeric})
							oControl:aProperties:Add(VODesignProperty{"_Top","Top",NULL,PropertyType.Numeric})
							oControl:aProperties:Add(VODesignProperty{"_Width","Width",NULL,PropertyType.Numeric})
							oControl:aProperties:Add(VODesignProperty{"_Height","Height",NULL,PropertyType.Numeric})
							oControl:aProperties:Add(VODesignProperty{"_Visible","Visible",NULL,"YESNO"})
							oControl:aProperties:Add(VODesignProperty{"_GenCode","Generate Code",NULL,"YESNO"})
//							oControl:aProperties:Add(VODesignProperty{"Inherit From Class","Inherit From Class",PropertyType.Text})
	
							oControl:aProperties:Add(VODesignProperty{"_DBInhFrom","Browser Inherit From",NULL,PropertyType.Text})
							oControl:aProperties:Add(VODesignProperty{"_DCInhFrom","Columns Inherit From",NULL,PropertyType.Text})
	
							oControl:aProperties:Add(VODesignProperty{"_IsDataPage","Page Data Aware",NULL,"YESNO"})
							oControl:aProperties:Add(VODesignProperty{"_PageCaption","Page Caption",NULL,PropertyType.Text})
							oControl:aProperties:Add(VODesignProperty{"_PageName","Page Name",NULL,PropertyType.Text})
						ELSE
							cParent := cUpper:Substring(0 , nAt)
							cUpper := cUpper:Substring(nAt + 1)
/*							nAt := cParent:LastIndexOf(':')
							IF nAt != -1
								cParent := cParent:Substring(nAt + 1)
							ENDIF*/
							oControl:AddFrom(cParent)
						ENDIF
						oControl:cParent := cParent
						oControl:cControl := cUpper
					CASE cUpper == "STDPROPERTIES"
						eSection := CavoWedInfSection.StdProp
					CASE cUpper == "ASSIGNMAP"
						eSection := CavoWedInfSection.AssignMap
					CASE cUpper == "SUPPLEMENTALFILES"
						eSection := CavoWedInfSection.Supplemental
					CASE cUpper == "OPTIONS"
						eSection := CavoWedInfSection.Options
					OTHERWISE
						eSection := CavoWedInfSection.None
					ENDCASE
				CASE eSection == CavoWedInfSection.StdProp
					nAt := cLine:IndexOf('=')
					IF nAt != -1
						cLeft := cLine:Substring(0 , nAt):ToUpper()
						cRight := cLine:Substring(nAt + 1)
						IF cLine:LastIndexOf(')') == -1
							VOWindowEditorTemplate.aEnumTypes:Add(cLeft , cRight)
						ELSE
							nAt := cLine:LastIndexOf(')')
							cRight := cLine:Substring(nAt + 1)
							aPages := Funcs.SplitString(cRight , ',')
/*							aPages := List<STRING>{}
							DO WHILE cRight:Length != 0
								nAt := cRight:IndexOf(',')
								IF nAt == -1
									aPages:Add(cRight)
									cRight := ""
								ELSE
									aPages:Add(cRight:Substring(0 , nAt))
									cRight := cRight:Substring(nAt + 1)
								ENDIF
							END DO*/
							VOWindowEditorTemplate.aPages:Add(cLeft , aPages)
						ENDIF
					ENDIF
				CASE eSection == CavoWedInfSection.Control
					nAt := cLine:IndexOf('=')
					IF nAt != -1
						cLeft := cLine:Substring(0 , nAt):ToUpper()
						cRight := cLine:Substring(nAt + 1)
						DO CASE
						CASE cLeft == "NAME"
							oControl:cName := cRight
							oControl:lUse := TRUE
						CASE cLeft == "TEXT"
							oControl:cStartText := cRight
						CASE cLeft == "CONTTITLE"
							oControl:cTitle := cRight
						CASE cLeft == "CLASSNAME"
							IF oControl:cWinClass == "" // might have been already assigned by OptClassName
								oControl:cWinClass := cRight
							END IF
						CASE cLeft == "OPTCLASSNAME"
							oControl:cWinClass := cRight
						CASE cLeft == "DIALOGTEMPLATE"
							oControl:lCreateResource := cRight == "1"
						CASE cLeft == "INITMETHOD"
/*							nAt := cRight:IndexOf('(')
							IF nAt != -1
								cRight := cRight:Substring(nAt + 1)
								nAt := cRight:LastIndexOf(')')
								IF nAt != -1
									cRight := cRight:Substring(0, nAt - 1)
									oControl:cInitMethod := cRight
								ENDIF
							ENDIF*/
							oControl:cInitMethod := cProperType
						CASE cLeft == "STYLE"
							oControl:aStyles := Funcs.SplitString(cRight , '|')
						CASE cLeft == "EXSTYLE"
							oControl:aExStyles := Funcs.SplitString(cRight , '|')
						CASE cLeft == "SIZE"
							TRY
								nAt := cRight:IndexOf(',')
								oControl:oSize := Size{ Convert.ToInt32(cRight:Substring(0 , nAt)) , Convert.ToInt32(cRight:Substring(nAt + 1)) }
							END TRY
						CASE cLeft == "PROPTABS"
							aPages := oControl:aPages
							DO WHILE cRight:Length != 0
								nAt := cRight:IndexOf(',')
								IF nAt == -1
									cPage := cRight
									cRight := ""
								ELSE
									cPage := cRight:Substring(0 , nAt)
									cRight := cRight:Substring(nAt + 1)
								ENDIF
/*								nAt := cPage:IndexOf(':')
								IF nAt != -1
									cPage := cPage:Substring(nAt + 1)
								ENDIF*/
								aPages:Add(cPage)
							END DO
						CASE cLeft:IndexOf("WINDOWSTYLE") == 0 .or. cLeft:IndexOf("EXWINDOWSTYLE") == 0
							nAt := cRight:IndexOf(',')
							IF nAt != - 1
								cCaption := cRight:Substring(0 , nAt)
								cEnumValues := cRight:Substring(nAt + 1)
								IF cLeft:IndexOf("WINDOWSTYLE") == 0
									oProp := VODesignProperty{cCaption , cType , cEnumValues , VOStyle.Style}
									oControl:aProperties:Add(oProp)
								ELSE
									oProp := VODesignProperty{cCaption , cType , cEnumValues , VOStyle.ExStyle}
									oControl:aProperties:Add(oProp)
								ENDIF
							ENDIF
	
						CASE cLeft:IndexOf("ASSIGN") == 0 .or. cLeft:IndexOf("INHERITCLASSNAME") == 0
							nAt := cRight:IndexOf(',')
							IF nAt != - 1
								cCaption := cRight:Substring(0 , nAt)
								cProp := cRight:Substring(nAt + 1)
								DO CASE
								CASE cProp:ToUpper():IndexOf("HYPERLABEL") != -1 .and. FALSE
									oProp := VODesignProperty{cCaption , cCaption , cCaption , PropertyType.Text}
									oProp:lMultiple := TRUE
									oProp:lNoCode := TRUE
									oProp:lNoAuto := TRUE
									oControl:aProperties:Add(oProp)
								CASE cType == "STRING"
									oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Text}
									oControl:aProperties:Add(oProp)
								CASE cType == "NUMERIC"
									oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Numeric}
									oControl:aProperties:Add(oProp)
								CASE cType:IndexOf("CLASS:") == 0
									oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Type}
									nAt := cType:IndexOf("CLASS:")
									oProp:cType := cType:Substring(nAt + 6)
									oControl:aProperties:Add(oProp)
								CASE cType:IndexOf(',') == -1
									IF cType[0] != '%'
										oProp := VODesignProperty{cCaption , cCaption , cProp , cType}
									ELSE
										cType := cType:Substring(1 , cType:Length - 2)
										oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Text}
										oProp:cSymbolProp := cType
									ENDIF
									oControl:aProperties:Add(oProp)
								CASE cType:IndexOf(',') != -1
									LOCAL nMultiPos AS INT
									nAt := 0
									nMultiPos := 1
									DO WHILE nAt < cType:Length
										IF cType[nAt] == ','
											nMultiPos ++
											nAt ++
										ELSE
											cType := cType:Substring(nAt)
											nAt := cType:IndexOf(',')
											IF nAt != -1
												cType := cType:Substring(0 , nAt)
											ENDIF
											nAt := cType:IndexOf(')')
											IF nAt != -1
												cType := cType:Substring(0 , nAt)
											ENDIF
//											System.Windows.Forms.MessageBox.Show(cType)
											cType := cType:ToUpper()
											DO CASE
											CASE cType:ToUpper() == "NUMERIC"
												oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Numeric}
											CASE cType[0] == '%'
												cType := cType:Substring(1 , cType:Length - 2)
												oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Text}
												oProp:cSymbolProp := cType
											OTHERWISE
												oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Text}
											END CASE
											oProp:lMultiple := TRUE
											oProp:nMultiPos := nMultiPos
											EXIT
										ENDIF
									END DO
									oControl:aProperties:Add(oProp)
								END CASE
							ENDIF
	
						CASE cLeft:IndexOf("METHOD") == 0
							nAt := cRight:IndexOf(',')
							IF nAt != - 1
								cCaption := cRight:Substring(0 , nAt)
								cProp := cRight:Substring(nAt + 1)
								DO CASE
								CASE cType:IndexOf("CODE:") == 0
									IF cType:Length > 5
										cProp := cType:Substring(5):Trim()
									END IF
									oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Callback}
									oControl:aProperties:Add(oProp)
								CASE cType == "STRING"
									oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Text}
									oProp:lMethod := TRUE
									oControl:aProperties:Add(oProp)
								CASE cCaption == "Group Value" .and. cProp == "Value" // DIRTY HACK
									oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Text}
									oProp:lMethod := TRUE
									oControl:aProperties:Add(oProp)
								CASE cType:IndexOf("CLASS:") == 0
									oProp := VODesignProperty{cCaption , cCaption , cProp , PropertyType.Type}
									oProp:lMethod := TRUE
									oControl:aProperties:Add(oProp)
//								CASE cType == "BOOL"
//								CASE cType == "LOGIC"
								OTHERWISE
									oProp := VODesignProperty{cCaption , cCaption , cProp , cType}
									oProp:lMethod := TRUE
									oControl:aProperties:Add(oProp)
								END CASE
							ENDIF
	
						END CASE
					ENDIF
	
				CASE eSection == CavoWedInfSection.AssignMap
					nAt := cLine:IndexOf('=')
					IF nAt != -1
						cLeft := cLine:Substring(0 , nAt):ToUpper()
						cRight := cLine:Substring(nAt + 1)
						IF !VOWindowEditorTemplate.aAssignMap:ContainsKey(cLeft)
							VOWindowEditorTemplate.aAssignMap:Add(cLeft , cRight)
						ENDIF
					ENDIF
	
				CASE eSection == CavoWedInfSection.Supplemental
					nAt := cLine:IndexOf('=')
					IF nAt != -1
						cLeft := cLine:Substring(0 , nAt):ToUpper()
						cRight := cLine:Substring(nAt + 1):Trim()
						IF cRight:Length != 0
							nAt := cRight:LastIndexOf("\")
							IF nAt != -1 .and. nAt < cRight:Length - 1
								cRight := cRight:Substring(nAt + 1):Trim()
							END IF
							cRight := Funcs.GetFileDir(cCavoWed) + "\" + cRight
							IF System.IO.File.Exists(cRight)
								VOWindowEditorTemplate.LoadInfFile(cRight)
							ELSE
								Funcs.ErrorBox("Could not find file " + cRight , Resources.EditorName)
							ENDIF
						ENDIF
					ENDIF
	
				CASE eSection == CavoWedInfSection.Options
					nAt := cLine:IndexOf('=')
					IF nAt != -1
						cLeft := cLine:Substring(0 , nAt):ToUpper()
						cRight := cLine:Substring(nAt + 1):Trim()
						TRY
							DO CASE
							CASE cLeft == "GRIDX"
								VOWindowEditorTemplate.GridX := Int32.Parse(cRight)
							CASE cLeft == "GRIDY"
								VOWindowEditorTemplate.GridY := Int32.Parse(cRight)
							END CASE
						END TRY
					ENDIF
	
				END CASE
				
			END DO

			lSuccess := TRUE
			
		CATCH
			
			Funcs.ErrorBox("Error loading file " + cCavoWed , Resources.EditorName)
		
		FINALLY

			IF oReader != NULL
				oReader:Close()
			END IF
		
		END TRY
		
	RETURN lSuccess


	STATIC ACCESS Loaded AS LOGIC
	RETURN VOWindowEditorTemplate.lLoaded
	STATIC ACCESS Count AS INT
	RETURN VOWindowEditorTemplate.aControls:Count
	STATIC METHOD Get(n AS INT) AS VOControlTemplate
	RETURN (VOControlTemplate)VOWindowEditorTemplate.aControls[n]
	STATIC METHOD Get(cControl AS STRING) AS VOControlTemplate
		LOCAL oTemplate AS VOControlTemplate
		LOCAL n AS INT
		cControl := cControl:ToUpper()
		FOR n := 0 UPTO VOWindowEditorTemplate.aControls:Count - 1
			oTemplate := (VOControlTemplate)VOWindowEditorTemplate.aControls[n]
			IF oTemplate:cControl:ToUpper() == cControl
				RETURN oTemplate
			ENDIF
		NEXT
	RETURN NULL
	STATIC METHOD GetFromFullClass(cFullClass AS STRING) AS VOControlTemplate
		LOCAL oTemplate AS VOControlTemplate
		LOCAL n AS INT
		cFullClass := cFullClass:ToUpper()
		FOR n := 0 UPTO VOWindowEditorTemplate.aControls:Count - 1
			oTemplate := (VOControlTemplate)VOWindowEditorTemplate.aControls[n]
			IF oTemplate:cFullClass:ToUpper() == cFullClass
				RETURN oTemplate
			ENDIF
		NEXT
	RETURN NULL
	STATIC METHOD GetParent(cParent AS STRING) AS VOControlTemplate
		LOCAL oTemplate AS VOControlTemplate
		LOCAL n AS INT
		cParent := cParent:ToUpper()
		FOR n := 0 UPTO VOWindowEditorTemplate.aControls:Count - 1
			oTemplate := (VOControlTemplate)VOWindowEditorTemplate.aControls[n]
			IF oTemplate:cFullClass:ToUpper() == cParent
				RETURN oTemplate
			ENDIF
		NEXT
	RETURN NULL
	STATIC METHOD GetEnumerator(cKey AS STRING) AS STRING
		IF VOWindowEditorTemplate.aEnumTypes:ContainsKey(cKey)
			RETURN VOWindowEditorTemplate.aEnumTypes[cKey]
		ENDIF
		DO WHILE cKey:Length > 3 // TODO looks like the VO WED works this way, for all types
			cKey := cKey:Substring(0, cKey:Length - 1)
			IF VOWindowEditorTemplate.aEnumTypes:ContainsKey(cKey)
				RETURN VOWindowEditorTemplate.aEnumTypes[cKey]
			ENDIF
		END DO
	RETURN NULL
	STATIC METHOD GetAssignMap(cKey AS STRING) AS STRING
		cKey := cKey:ToUpper()
		IF VOWindowEditorTemplate.aAssignMap:ContainsKey(cKey)
			RETURN VOWindowEditorTemplate.aAssignMap[cKey]
		ENDIF
	RETURN NULL
	
END CLASS

CLASS VOControlTemplate
	EXPORT cControl AS STRING
	EXPORT cParent AS STRING
	EXPORT cName AS STRING
	EXPORT cFullClass AS STRING
	EXPORT cStartText AS STRING
	EXPORT oSize AS Size
	EXPORT lForm AS LOGIC
	EXPORT lUse AS LOGIC
	EXPORT cTitle AS STRING
	EXPORT cWinClass AS STRING
	EXPORT aPages AS List<STRING>
	EXPORT aProperties AS ArrayList
	EXPORT aStyles AS List<STRING>
	EXPORT aExStyles AS List<STRING>
	EXPORT cInitMethod AS STRING
	EXPORT lCreateResource AS LOGIC
	CONSTRUCTOR()
		SUPER()
		SELF:cStartText := ""
		SELF:cName := ""
		SELF:cParent := ""
		SELF:cFullClass := ""
		SELF:cControl := ""
		SELF:cTitle := ""
		SELF:cWinClass := ""
		SELF:cInitMethod := ""
		SELF:aProperties := ArrayList{}
		SELF:aPages := List<STRING>{}
	RETURN
	METHOD AddFrom(cControl AS STRING) AS VOID
		LOCAL oTemplate AS VOControlTemplate
		LOCAL n AS INT
//		oTemplate := VOWindowEditorTemplate.Get(cControl)
		oTemplate := VOWindowEditorTemplate.GetParent(cControl)
		IF oTemplate != NULL
			FOR n := 0 UPTO oTemplate:aProperties:Count - 1
				SELF:aProperties:Add(oTemplate:aProperties[n])
				SELF:lCreateResource := oTemplate:lCreateResource
			NEXT
			IF oTemplate:aExStyles != NULL
				SELF:aExStyles := List<STRING>{}
				FOR n := 0 UPTO oTemplate:aExStyles:Count - 1
					SELF:aExStyles:Add(oTemplate:aExStyles[n])
				NEXT
			END IF
		ENDIF
	RETURN
	
END CLASS
