// DialogForm.prg

#using System.Windows.Forms
#using System.ComponentModel
CLASS VODialogForm INHERIT VOForm
	PROTECT	oSurfacePanel	AS VOSurfacePanel
	PROTECT oResDlg			AS ResourceDialog
	PROTECT oInitialSize	AS System.Drawing.Size
	PROTECT lShown			AS LOGIC
	PROTECT lMustAdjust		AS LOGIC
	PROPERTY Surface		AS VOPanel	GET oSurfacePanel 
	PROPERTY InitialSize    AS System.Drawing.Size GET oInitialSize SET oInitialSize := Value
	PROPERTY IsShown			AS LOGIC GET lShown
	
	CONSTRUCTOR(oWindow AS Window, oRes AS ResourceDialog)
		SELF:oResDlg := oRes
		SUPER(oWindow)
		SELF:StartPosition	       := FormStartPosition.CenterParent
		SELF:Text                  := "DialogForm"
		IF oRes != NULL
			SELF:Size				:= oRes:Size
			SELF:oInitialSize		:= oRes:Size
			SELF:lMustAdjust		:= _AND(oRes:Style, WS_SYSMENU|WS_MINIMIZEBOX|WS_MAXIMIZEBOX) != 0
		ELSE
			SELF:lMustAdjust := FALSE
			SELF:Size				:= System.Drawing.Size{1,1} 
		ENDIF
		SELF:AutoSizeMode          := AutoSizeMode.GrowAndShrink
		
		oSurfacePanel              := GuiFactory.Instance:CreateSurfacePanel(oWindow)
		oSurfacePanel:Visible      := TRUE
		SELF:oSurfacePanel:Dock    := DockStyle.Fill
		oSurfacePanel:AutoSizeMode := AutoSizeMode.GrowAndShrink
		oSurfacePanel:AutoSize     := TRUE
		oSurfacePanel:Text         := "Surface"
		SELF:Controls:Add(oSurfacePanel)
		
				
	METHOD SetSizable(lSet AS LOGIC) AS VOID
		IF lSet
			SELF:FormBorderStyle := FormBorderStyle.Sizable
		ELSE
			SELF:FormBorderStyle := FormBorderStyle.FixedDialog		
		ENDIF

	VIRTUAL PROTECT METHOD OnShown(e AS EventArgs) AS VOID STRICT
		FOREACH oC AS System.Windows.Forms.Control IN SELF:Surface:Controls
			IF oC is VOButton
				IF ((VoButton) (oC)):DefaultButton
					SELF:AcceptButton := (VoButton) oC
				ENDIF
			ENDIF
		NEXT
		SUPER:OnShown(e)
		LOCAL oFirst AS System.Windows.Forms.Control
		oFirst := SELF:GetFirstEditableControl()
		IF oFirst != NULL_OBJECT
			oFirst:Select()
		ENDIF
		((DialogWindow) Window):PostShowDialog()
	
	VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
		GET
			LOCAL IMPLIED result := SUPER:CreateParams
			IF SELF:oResDlg != NULL_OBJECT 
				SELF:oResDlg:CopyCreateParams(result, TRUE)
			ENDIF
			RETURN result
		END GET
	END PROPERTY


	VIRTUAL PROTECTED METHOD OnClosing( e AS CancelEventargs) AS VOID
		SUPER:OnClosing(e )
		
	VIRTUAL PROTECTED METHOD OnVisibleChanged( e AS System.EventArgs) AS VOID
		SUPER:OnVisibleChanged(e)
		IF SELF:Visible 
			// Fix problem of group boxes that cover other controls
			SELF:oSurfacePanel:Prepare()
			IF ! lShown
				SELF:oSurfacePanel:Dock			:= DockStyle.Fill
				// When the window is painted with style WS_SYSMENU|WS_MINIMIZEBOX|WS_MAXIMIZEBOXsometimes the size is too small
				IF lMustAdjust .and. !SELF:oInitialSize:IsEmpty .and. (SELF:Width != oInitialSize:Width .or. SELF:Height != oInitialSize:Height)
					SELF:Size := oInitialSize
					IF SELF:StartPosition == System.Windows.Forms.FormStartPosition.CenterScreen
						SELF:CenterToScreen()
					ENDIF
				ENDIF
				lShown := TRUE
			ENDIF
		ENDIF
	RETURN 
	
	METHOD GetFirstEditableControl AS System.Windows.Forms.Control
		RETURN GetFirstEditableChild(SELF:oSurfacePanel)

	PROTECTED METHOD OnSizeChanged(e AS EventArgs) AS VOID
		SUPER:OnSizeChanged(e)


END CLASS

