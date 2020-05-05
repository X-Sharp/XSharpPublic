// DataForm.prg
#INCLUDE "VOWin32APILibrary.vh"
#using System.Windows.Forms
#using System.Drawing
#define INSIDEFORMBORDER 5
/*
 For MDI Child windows this looks like:
.--------------------------------------------
| Toolbar Area                              |
.--------------------------------------------
| SurfacePanel                   |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|.................................          |
|FramePanel                                 |
|                                           |
.--------------------------------------------
| Statusbar Area                            |
.--------------------------------------------


Subwindows consist of:

.--------------------------------------------
| SurfacePanel                   |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|                                |          |
|.................................          |
|FramePanel                                 |
.--------------------------------------------

When the surface is smaller than the framepanel, then that panel will fill the window
When the srface is bigger than the Framepanel, then that panel will show the scrollbars

When the datawindow is in browse view, then the browser has the size of the frame panel and no 
scrollbars are shown on the framepanel


*/


CLASS VODataForm INHERIT VOChildAppForm
	PROTECT oSurfacePanel	AS VoSurfacePanel		// This is the place where the controls are drawn
	PROTECT oFramePanel		AS VOFramePanel				// This is a panel on which the Surface sits. When the window is too small this panel will show the scrollbars
	PROTECT oToolBar		AS VOToolBar			
	PROTECT oStatusBar		AS VOStatusStrip
	PROTECT oResDlg			AS ResourceDialog
	PROTECT lSubForm		AS LOGIC
	PROTECT lAutoLayout		AS LOGIC
	PROTECT lInBrowseView   AS LOGIC
	PROTECT oDataBrowser	AS System.Windows.Forms.Control

#region Properties
	PROPERTY Origin			AS XSharp.VO.Point  GET SELF:Location SET SELF:Location := Value
	PROPERTY Surface		AS VoSurfacePanel	GET oSurfacePanel
	PROPERTY Frame          AS VOFramePanel		GET oFramePanel
	PROPERTY AutoLayout	 AS LOGIC GET lAutoLayout SET lAutoLayout := VALUE
	
	PROPERTY DataBrowser AS System.Windows.Forms.Control
		GET
			RETURN oDataBrowser
		END GET
		SET
			IF oDataBrowser != Value
				IF oDataBrowser != NULL_OBJECT
					oDataBrowser:GotFocus -= OnBrowserGotFocus
					oDataBrowser:LostFocus -= OnBrowserLostFocus
				ENDIF				
				oDataBrowser := Value
				IF oDataBrowser != NULL_OBJECT
					oFramePanel:Controls:Add(oDataBrowser)	
					oDataBrowser:Visible := FALSE		
					oDataBrowser:Dock := DockStyle.Fill
					oDataBrowser:BringToFront()
					oDataBrowser:GotFocus += OnBrowserGotFocus
					oDataBrowser:LostFocus += OnBrowserLostFocus
				ENDIF
			ENDIF
			SELF:AdjustSizes()
		END SET
	END PROPERTY
	
	PRIVATE METHOD OnBrowserGotFocus(sender AS OBJECT, e AS EventArgs) AS VOID
		IF SELF:lSubForm .and. SELF:Window != NULL_OBJECT
			SELF:Window:Activate(Event{})
		ENDIF
	PRIVATE METHOD OnBrowserLostFocus(sender AS OBJECT, e AS EventArgs) AS VOID
		IF SELF:lSubForm .and. SELF:Window != NULL_OBJECT
			SELF:Window:Deactivate(Event{})
		ENDIF
	
	PROPERTY DataWindow AS XSharp.VO.DataWindow GET (DataWindow) Window

	// Dirty Access Using Latebound Code for the validation of reccounts after selections in Ranorex. I am sorry. This has to be implemented on the Winforms-Layer and time is a factor.
//	PROPERTY RanorexRecCount AS STRING
//		GET
//			LOCAL cRet:= "-1" AS STRING
//			LOCAL oVoWin := SELF:DataWindow AS DataWindow
//			IF oVoWin!= NULL .AND. oVoWin:Server != NULL .AND. ismethod(oVoWin:Server, "getSqlState") .AND. isAccess(oVoWin:Server, "Connection") .AND. oVoWin:Server:Connection != NULL .AND. isInstanceOf(oVoWin:Server:Connection,"SQLConnection")
//				LOCAL cState := oVoWin:Server:GetSQLState( "count(1)", TRUE, FALSE ) AS STRING
//				LOCAL oConn := oVoWin:Server:Connection AS OBJECT
//				cRet := AsString(oConn:DoSimpleSelect(cState))
//			ENDIF
//			RETURN cRet
//		END GET
//	END PROPERTY
		

	PROPERTY StatusBar AS VOStatusStrip
		GET
			RETURN oStatusBar
		END GET
		SET
			oStatusBar := Value
			IF oStatusBar != NULL_OBJECT
				SELF:Controls:Add(oStatusBar)
				oStatusBar:Dock := DockStyle.Bottom
				oStatusBar:TabIndex := 3
				SELF:PerformLayout()
			ENDIF
		END SET
	END PROPERTY

	PROPERTY SubForm	AS LOGIC	GET lSubForm SET lSubForm := Value

	PROPERTY ToolBar AS VOToolBar
		GET
			RETURN oToolBar
		END GET
		SET
			IF Value != NULL_OBJECT
				oToolBar := Value
				SELF:Controls:Add(oToolBar)
				oToolBar:Dock := DockStyle.Top
				oToolBar:TabIndex := 1
				SELF:PerformLayout()
				SELF:AdjustSizes()
				SELF:AdjustFrameSize()
			ENDIF
		END SET
	END PROPERTY
#endregion

	CONSTRUCTOR(oWindow AS Window, oOwner AS Form, oRes AS ResourceDialog)
		SUPER(oWindow, oOwner)
		SELF:oResDlg := oRes 
		SELF:Text := "DataWinForm"
		oFramePanel := VOFramePanel{SELF, oWindow}							
		SELF:Controls:Add(oFramePanel)
		oSurfacePanel := VoSurfacePanel{oWindow}
		
		oFramePanel:Controls:Add(oSurfacePanel)
		IF oRes != NULL_OBJECT
			oSurfacePanel:Size			  := oRes:Size
			oSurfacePanel:MinimumSize     := oRes:Size
		ELSE
			oSurfacePanel:Size     := SELF:ClientRectangle:Size
		ENDIF
		oFramePanel:Size := oSurfacePanel:Size
		oSurfacePanel:AutoSize := TRUE
		oSurfacePanel:Text := "Surface"
		oFramePanel:Text := "Frame"
		SELF:AdjustFrameSize()




	METHOD AdjustSizes() AS VOID STRICT
		LOCAL oSize AS Size
		// This is called when resizing and/or when toolbar or statusbar is shown
		// it recalculates the size of the framepanel
		SELF:SuspendLayout()
		IF ! lSubForm
			oSize := SELF:ClientRectangle:Size
			IF oToolBar != NULL_OBJECT
				oSize:Height -= oToolBar:Height
				SELF:oFramePanel:Location := System.Drawing.Point{0,oToolBar:Height}
			ELSE
				SELF:oFramePanel:Location := System.Drawing.Point{0,0}
			ENDIF

			IF oStatusBar != NULL_OBJECT //.and. oStatusBar:Visible
				oSize:Height -= oStatusBar:Height
			ENDIF
			IF oFramePanel:Size:Width != oSize:Width .OR. oFramePanel:Size:Height != oSize:Height
				oFramePanel:Size := oSize
			ENDIF
		ENDIF
		IF lInBrowseView
			oDataBrowser:Dock := DockStyle.Fill			
			oFramePanel:AutoScroll := FALSE
		ELSE
			oFramePanel:AutoScroll := TRUE
		ENDIF
		SELF:ResumeLayout()
		RETURN
		
	METHOD AdjustFrameSize() AS VOID STRICT	
		// Adjust outer window so it fits just around the surface
		LOCAL oOuterSize AS Size
		LOCAL oInnerSize AS Size
		LOCAL nBordX, nBordY AS LONG
		LOCAL oLoc       AS System.Drawing.Point
		SELF:SuspendLayout()
		IF SELF:lSubForm
			//IF ! SELF:lInBrowseView			
			//	oInnerSize := oSurfacePanel:Size
			//	SELF:oFramePanel:Size := oInnerSize
			//ENDIF
		ELSE	
			oLoc       := SELF:Location
			oOuterSize := SELF:Size
			oInnerSize := SELF:ClientRectangle:Size
			nBordX     := oOuterSize:Width - oInnerSize:Width
			nBordY     := oOuterSize:Height - oInnerSize:Height 
			// Calculate wanted outer size based on Surface Size
			//oSurfacePanel:MinimumSize := System.Drawing.Size.Empty
			oSurfacePanel:AutoSizeMode := AutoSizeMode.GrowAndShrink
			oSurfacePanel:PerformLayout()
			oInnerSize := oSurfacePanel:Size
			SELF:oFramePanel:Size := oInnerSize
			oOuterSize:Width := oInnerSize:Width + nBordX + INSIDEFORMBORDER
			oOuterSize:Height := oInnerSize:Height + nBordY + INSIDEFORMBORDER
			IF oToolBar != NULL_OBJECT //.and. oToolBar:Visible

				oOuterSize:Height += oToolBar:Height
			ENDIF
			IF oStatusBar != NULL_OBJECT //.and. oStatusBar:Visible
				oOuterSize:Height += oStatusBar:Height
			ENDIF
			SELF:Size := oOuterSize
			SELF:Location := oLoc
		ENDIF
		SELF:ResumeLayout(TRUE)		
		RETURN

	METHOD ChangeFormSize(oDim AS Size) AS VOID
		SELF:Size := oDim
		RETURN


	METHOD CreateSubForm(nResourceID AS LONG, oResDlg AS ResourceDialog) AS LOGIC
		LOCAL oItem AS ResourceDialogItem
		lSubForm := TRUE
		oItem := oResDlg:GetDlgItem(nResourceID)
		IF oItem != NULL_OBJECT
			SELF:oFramePanel:Location	 := oItem:Origin
			SELF:oFramePanel:Size		 := oItem:Size
			SELF:oFramePanel:DefinedSize := oItem:Size
			SELF:oFramePanel:Visible	 := TRUE
			SELF:oFramePanel:AutoScroll  := FALSE
			SELF:oFramePanel:IsSubForm   := TRUE
			IF oItem:TabIndex > 0
				SELF:oFramePanel:TabIndex	:= oItem:TabIndex
			ENDIF
		ENDIF
		RETURN TRUE

	METHOD ResetMinSize() AS VOID
		RETURN 		
		
	VIRTUAL PROTECT METHOD OnShown(e AS EventArgs) AS VOID STRICT
		FOREACH oC AS System.Windows.Forms.Control IN SELF:Surface:Controls
			IF TYPEOF(VOButton):IsAssignableFrom(oC:GetType())
				IF ((VoButton) (oC)):DefaultButton
					SELF:AcceptButton := (VoButton) oC
				ENDIF
			ENDIF
		NEXT
		SUPER:OnShown(e)

	PROTECTED METHOD OnSizeChanged(e AS EventArgs ) AS VOID
		SUPER:OnSizeChanged(e)
		IF SELF:Visible
			AdjustSizes()
		ENDIF
		RETURN

	METHOD HideSubForm () AS VOID STRICT
		SELF:Frame:Visible	:= FALSE
		RETURN 

	
	METHOD ShowSubForm () AS VOID STRICT
		SELF:oFramePanel:Visible	 := TRUE
		IF SELF:lInBrowseView 
			SELF:oDataBrowser:Visible := TRUE
			SELF:oDataBrowser:Dock 	:= DockStyle.Fill
			SELF:oFramePanel:Size 	:= SELF:oFramePanel:DefinedSize
		ELSE
			SELF:oSurfacePanel:Visible := TRUE
		ENDIF
		SELF:Frame:Prepare()		
		RETURN 
			
	METHOD SetFocusToForm ( ) AS VOID STRICT
		IF SELF:lInBrowseView
			oDataBrowser:Focus()
		ELSEIF (oSurfacePanel  != NULL_OBJECT)
			oSurfacePanel:Focus()
		ENDIF
		RETURN
		
	METHOD ViewAs(lBrowse AS LOGIC) AS VOID
		oFramePanel:SuspendLayout()
		IF lBrowse
			IF (oDataBrowser== NULL_OBJECT) 
				RETURN 
			ENDIF
			oSurfacePanel:Visible := FALSE
			IF oFramePanel:Controls:Contains(oSurfacePanel)
				oFramePanel:Controls:Remove(oSurfacePanel)
			ENDIF
			oFramePanel:Controls:Add(oDataBrowser)
			oFramePanel:AutoScroll := FALSE
			oDataBrowser:Visible := TRUE
			oDataBrowser:Dock 	 := DockStyle.Fill
			oDataBrowser:Focus()
		ELSE
			// Show the dialog and hide the GBRowse
			IF (oDataBrowser != NULL_OBJECT)
				oDataBrowser:Visible := FALSE
				IF oFramePanel:Controls:Contains(oDataBrowser)
					oFramePanel:Controls:Remove(oDataBrowser)
				ENDIF
			ENDIF
			oFramePanel:Controls:Add(oSurfacePanel)
			oFramePanel:AutoScroll := TRUE
			oSurfacePanel:Visible := TRUE
			
		ENDIF
		oFramePanel:Prepare()
		oFramePanel:ResumeLayout(TRUE)
		SELF:lInBrowseView := lBrowse		
		SELF:AdjustSizes()
		RETURN 	

	PUBLIC METHOD SuspendRedraw AS VOID
		IF SELF:lInBrowseView
	        Win32.SendMessage(SELF:Frame:Handle, WM_SETREDRAW, 0, 0)
		ELSE
	        Win32.SendMessage(SELF:Surface:Handle, WM_SETREDRAW, 0, 0)
		ENDIF


	PUBLIC METHOD ResumeRedraw AS VOID
		IF SELF:lInBrowseView
	        Win32.SendMessage(SELF:Frame:Handle, WM_SETREDRAW, 1, 0)
		ELSE
	        Win32.SendMessage(SELF:Surface:Handle, WM_SETREDRAW, 1, 0)
		ENDIF
		SELF:Refresh()
    


END CLASS
