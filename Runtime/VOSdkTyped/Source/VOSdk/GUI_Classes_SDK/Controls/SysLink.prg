
USING System.Collections.Generic

CLASS SysLink INHERIT TextControl

    PROPERTY ControlType AS ControlType GET ControlType.SysLink

    PROPERTY __LinkLabel AS VOLinkLabel GET (VOLinkLabel) oCtrl

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
      LOCAL oLL := (VOLinkLabel) oC AS VOLinkLabel
      oLL:LinkClicked += LinkClicked



	METHOD __StripTags(sHTML REF STRING) AS List<STRING>
		LOCAL iPosOpen, iPosClose AS DWORD
        LOCAL result AS List<STRING>
        result := List<STRING>{}
		iPosOpen := At2("<", sHTML)
		WHILE (iPosOpen > 0)
			iPosClose := At2(">", sHTML)
			IF (iPosClose > 0)
                VAR substr := SubStr3(sHTML,iPosOpen,  iPosClose-iPosOpen+1)
                IF (substr:ToLower():Contains("href"))
                    VAR parts := substr:Split(<CHAR>{'"'})
                    FOREACH part AS STRING IN parts
                        IF part:IndexOfAny(<CHAR>{'=','<','>'}) == -1
                            result:Add(part)
                        ENDIF
                    NEXT
                ENDIF
				sHTML := Stuff(sHTML, iPosOpen, (iPosClose-iPosOpen+1), "")
			ENDIF
			iPosOpen := At2("<", sHTML)
        END
		RETURN result



	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware) 
		LOCAL cClass AS USUAL
		LOCAL lResID AS LOGIC

		Default(@lDataAware, TRUE)
		lResID := IsInstanceOfUsual(xID,#ResourceID)
		IF !lResID
			cClass := "SysLink"
		ENDIF

		SUPER(oOwner, xID, oPoint, oDimension, cClass, SS_Left, lDataAware)

		IF !lResID
			IF !IsNil(cText)
                LOCAL sText := cText AS STRING
				VAR links := SELF:__StripTags(REF sText)
                IF links:Count > 0
                    SELF:__LinkLabel:Links[0].LinkData := links[0]
                ENDIF
				SELF:Caption := sText
				cWindowName := cText
			ENDIF
		ENDIF

		RETURN 

    METHOD LinkClicked(sender AS OBJECT, e AS System.Windows.Forms.LinkLabelLinkClickedEventArgs) AS VOID
       ShellOpen(SELF:Owner, e:Link:LinkData)
       RETURN

END CLASS

