//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Forms.prg
// This file contains subclasses of Windows.Form that are used in the VO Compatible
// Unicode GUI Classes
// Each class is used for a particular VO Window
// The VOForm class is the common parent class for all the form classes.
// The VODataWinForm class has a (VO)Panel{} that acts as the surface
// in the standard VO GU classes


USING System.Windows.Forms
USING System.Reflection
USING System.ComponentModel
USING System.Collections.Generic
CLASS VOForm INHERIT Form IMPLEMENTS IVOForm

	#region Fields
	PROTECT oProperties AS VOFormProperties
	PROTECT oToolTip	AS VOToolTip
	PROTECT _lIsClosing AS LOGIC
	STATIC PRIVATE oSmallIconFieldInfo AS FieldInfo
	#endregion


	#region Helper Methods
	PRIVATE STATIC METHOD _GetFieldInfo() AS FieldInfo STRICT
		IF oSmallIconFieldInfo == NULL_OBJECT
			LOCAL oType AS System.Type
			oType := Typeof(System.Windows.Forms.Form)
			oSmallIconFieldInfo := oType:GetField("smallIcon",BindingFlags.Instance|BindingFlags.NonPublic)
		ENDIF
		RETURN oSmallIconFieldInfo

	#endregion

	#region Properties
	PROPERTY Window		AS Window
		GET
		IF oProperties != NULL_OBJECT
			RETURN oProperties:Window
		ELSE
			RETURN NULL_OBJECT
		ENDIF
		END GET
	END PROPERTY
	PROPERTY Properties AS VOFormProperties GET oProperties
	PROPERTY SmallIcon AS System.Drawing.Icon
		GET
			LOCAL oFI AS FieldInfo
			oFI := _GetFieldInfo()
			IF oFI != NULL_OBJECT
				return (System.Drawing.Icon) oFI:GetValue(self)
			ELSE
				RETURN NULL_OBJECT
			ENDIF
		END GET
		SET
			LOCAL oFI AS FieldInfo
			oFI := _GetFieldInfo()
			IF oFI != NULL_OBJECT
				oFI:SetValue(SELF, Value)
			ENDIF
		END SET
	END PROPERTY
	PROPERTY IsAttached		AS LOGIC GET SELF:oProperties != NULL_OBJECT

	#endregion

	CONSTRUCTOR(oWindow AS Window)
		SELF:oProperties := VOFormProperties{SELF, oWindow}
		SUPER()
		SELF:KeyPreview := TRUE		// Process all key events at the form level
		SELF:Text		:= "VoForm"
		SELF:oToolTip	:= VOToolTip{}
		SELF:Margin		:= Padding{0}
		SELF:Disposed   += OnDisposed
		SELF:Cursor := System.Windows.Forms.Cursors.Arrow


    method AddControl (oCtrl as System.Windows.Forms.Control) as void
        self:Controls:Add( oCtrl)

    METHOD SetChildIndex(oCtrl AS IVOControl, nIndex AS LONG) AS VOID
        IF oCtrl IS System.Windows.Forms.Control VAR oC
            SELF:Controls:SetChildIndex(oC,nIndex)
        ENDIF

	METHOD GetAllControls() AS IList<System.Windows.Forms.Control> STRICT
		RETURN SELF:GetAllChildren(TRUE)

	METHOD GetFirstEditableControl AS System.Windows.Forms.Control STRICT
		return SELF:GetFirstEditableChild()

	PUBLIC METHOD NextControl() AS VOID
		SUPER:ProcessTabKey(TRUE)

	PUBLIC METHOD PreviousControl() AS VOID
		SUPER:ProcessTabKey(FALSE)

	PUBLIC METHOD Center AS VOID STRICT
		IF SELF:Parent != NULL_OBJECT
			SELF:CenterToParent()
		ELSE
			SELF:CenterToScreen()
		ENDIF


	// Windows procedure replaces 'Dispatch'
	VIRTUAL PROTECT METHOD WndProc(msg REF Message) AS VOID

		TRY
			LOCAL lCallSuper AS LOGIC
			lCallSuper := TRUE
			IF msg:Msg == WM_SYSCOMMAND
				// This code is necessary because CodeJock intercepts the double click on title bar of
				// a minimized window and closes the window .
				// By disabling the call to super the window behaves as expected
				IF SELF:IsMdiChild .and. SELF:WindowState == FormWindowState.Minimized
					IF msg:WParam:ToInt32() == 0XF012 // Double click on title bar
						lCallSuper := FALSE
					ENDIF
				ENDIF
			ENDIF
			IF lCallSuper
				SUPER:WndProc(REF msg)
			ENDIF
			IF ! SELF:IsDisposed  .and. SELF:IsAttached
				DO CASE
				CASE SELF:Window == NULL_OBJECT
					// Do nothing
                    NOP
				CASE msg:Msg == WM_APPCOMMAND
					SELF:Window:AppCommand(AppCommandEvent{msg})
				CASE msg:Msg == WM_GETMINMAXINFO
					SELF:Window:MinMaxInfo(MinMaxInfoEvent{msg})
				CASE msg:Msg == WM_SIZE
					SELF:Window:Resize(ResizeEvent{msg})
				ENDCASE
			ENDIF
		CATCH  AS Exception
			// Swallow errors in here
			//Debout(SELF:ToString(), __ENTITY__, SELF:IsAttached,e:Message, CRLF)
            NOP
		END TRY
		RETURN


	VIRTUAL PROTECT METHOD OnFormClosing(e AS FormClosingEventArgs) AS VOID
		LOCAL lClose AS LOGIC
		SUPER:OnFormClosing(e)
		IF SELF:IsAttached
			IF ! e:Cancel
				lClose := Window:QueryClose(@@Event{})
				IF !lClose
					e:Cancel := TRUE
				ENDIF
			ENDIF
		ENDIF
		RETURN

	PROTECTED METHOD OnDisposed(sender AS OBJECT, e AS EventArgs) AS VOID STRICT
		// When a window is hidden before it is closed then the FormClosed is not sent
		// but it is directly disposed. We catch that here.
		IF SELF:IsAttached
			SELF:CloseVOForm()
		ENDIF

	PROTECTED METHOD CloseVOForm() AS VOID STRICT
		IF SELF:IsAttached .and. SELF:Window != NULL_OBJECT
			LOCAL oWindow AS Window
			oWindow := SELF:Window
			IF oWindow:__Form == SELF
				SELF:oProperties := NULL_OBJECT
				IF ! oWindow:IsClosing
					oWindow:Close(Event{})
				ENDIF
				oWindow:Destroy()
			ENDIF

		ENDIF

	VIRTUAL PROTECT METHOD OnFormClosed(e AS FormClosedEventArgs) AS VOID STRICT
		IF SELF:IsAttached
			SELF:CloseVOForm()
		ENDIF
		SUPER:OnFormClosed(e)


	PROTECTED METHOD OnPaint (e AS PaintEventargs) AS VOID
		SUPER:OnPaint(e)
		IF SELF:Window != NULL_OBJECT
			LOCAL oWindow AS Window
			oWindow := SELF:Window
			oWindow:Expose(ExposeEvent{e})
		ENDIF
		RETURN

	PUBLIC METHOD SuspendRedraw AS VOID
        GuiWin32.SendMessage(SELF:Handle, WM_SETREDRAW, 0, 0)


	PUBLIC METHOD ResumeRedraw AS VOID
        GuiWin32.SendMessage(SELF:Handle, WM_SETREDRAW, 1, 0)
		SELF:Refresh()


END CLASS

