// Forms.prg
// This file contains subclasses of Windows.Form that are used in the VO Compatible
// Unicode GUI Classes
// Each class is used for a particular VO Window
// The VOForm class is the common parent class for all the form classes.
// The VODataWinForm class has a (VO)Panel{} that acts as the surface 
// in the standard VO GU classes


#USING System.Windows.Forms
#USING System.Reflection
#using System.ComponentModel
#using System.Collections.Generic
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
				RETURN oFI:GetValue(SELF)
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
		

	STATIC METHOD __GetAllControls(oParent AS System.Windows.Forms.Control, iList AS List<System.Windows.Forms.Control>) AS VOID
		FOREACH oC AS System.Windows.Forms.Control IN oParent:Controls
			iList:Add(oC)
			__GetAllControls(oC, iList)
		NEXT
		RETURN
		
	METHOD GetAllControls() AS IList<System.Windows.Forms.Control>
		LOCAL aList AS List<System.Windows.Forms.Control>
		aList := List<System.Windows.Forms.Control>{}
		__GetAllControls(SELF,aList)
		RETURN aList
	
	STATIC METHOD GetFirstEditableChild(oParent AS System.Windows.Forms.Control)
		FOREACH oC AS System.Windows.Forms.Control IN oParent:Controls
			IF oC:CanSelect .AND. !  oc IS VOButton 
				RETURN oC
			ENDIF
			
			IF oC:Controls:Count > 0 .AND. ! oC is VOGroupBox
				LOCAL oC2 AS System.Windows.Forms.Control
				oC2 := GetFirstEditableChild(oC)
				IF oC2 != NULL_OBJECT
					RETURN oC2
				ENDIF
			ENDIF
		NEXT
		RETURN NULL_OBJECT
	
	METHOD GetFirstEditableControl AS System.Windows.Forms.Control
		return GetFirstEditableChild(SELF)
	
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
					oWindow:Close()
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
        Win32.SendMessage(SELF:Handle, WM_SETREDRAW, 0, 0)


	PUBLIC METHOD ResumeRedraw AS VOID
        Win32.SendMessage(SELF:Handle, WM_SETREDRAW, 1, 0)
		SELF:Refresh()
    

END CLASS

