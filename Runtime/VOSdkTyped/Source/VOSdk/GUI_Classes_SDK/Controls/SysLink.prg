#include "VOWin32APILibrary.vh"
//Todo: COmplete SysLink 
CLASS SysLink INHERIT TextControl

    PROPERTY ControlType AS ControlType GET ControlType.SysLink

	METHOD __StripTags(sHTML AS STRING) AS VOID STRICT 
		LOCAL iPosOpen, iPosClose AS DWORD

		iPosOpen := At2("<", sHTML)
		WHILE (iPosOpen > 0)
			iPosClose := At2(">", sHTML)
			IF (iPosClose > 0)
				sHTML := Stuff(sHTML, iPosOpen, (iPosClose-iPosOpen+1), "")
			ENDIF
			iPosOpen := At2("<", sHTML)
		END
		RETURN


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
				IF (cClass == "Static")
					SELF:__StripTags(cText)
				ENDIF
				cWindowName := cText
				SELF:Caption := cText
			ENDIF
		ENDIF

		RETURN 

END CLASS

