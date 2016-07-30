//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Windows.Forms
using System.Drawing
using System.Collections.Generic
using System.Collections
using System.IO
using System.Text


//#include "C:\dotNet\vonet\VOEdit\Win32-GlobalDefines.vh"
#include "VOWin32APILibrary.vh"


CLASS DesignerBasicAction
	EXPORT eAction AS DesignerBasicActionType
	EXPORT uData AS ActionData
	EXPORT lExecuted AS LOGIC
	EXPORT lGroup AS LOGIC
	EXPORT aUndo,aRedo AS ArrayList
	EXPORT aSelected AS List<STRING>
	CONSTRUCTOR(lIsUndoRedo AS LOGIC)
		IF lIsUndoRedo
			SELF:lExecuted := TRUE
		ELSE
			SELF:aRedo := ArrayList{}
			SELF:aUndo := ArrayList{}
			SELF:aSelected := List<STRING>{}
		END IF
	RETURN
	CONSTRUCTOR(_eAction AS DesignerBasicActionType , lIsUndoRedo AS LOGIC)
		SELF:eAction := _eAction
		IF lIsUndoRedo
			SELF:lExecuted := TRUE
		ELSE
			SELF:aRedo := ArrayList{}
			SELF:aUndo := ArrayList{}
			SELF:aSelected := List<STRING>{}
		END IF
	RETURN
END CLASS


ABSTRACT CLASS DesignerBase
	PROTECT oGrid AS DesignerGrid
	PROTECT oToolBox AS ToolBox
	PROTECT oSurface AS Control
	PROTECT lLoading AS LOGIC
	PROTECT lStandalone AS LOGIC
	PROTECT lReadOnly AS LOGIC

	EXPORT IsDirtyChanged AS EventHandler
	EXPORT TriggerSave AS EventHandler
	EXPORT StatusBarMessage AS StatusBarMessageDelegate
	
	PROTECT aActions AS ArrayList
	PROTECT nAction , nActionDepth , nActionSaved AS INT
	PROTECT lDidAction AS LOGIC
	PROTECT lClearUndoBuffer AS LOGIC

	PROTECT oTimer AS Timer

	CONSTRUCTOR(_oSurface AS Control)
		SUPER()
		SELF:oSurface := _oSurface
		SELF:oTimer := Timer{}
		SELF:oTimer:Interval := 500
		SELF:oTimer:Tick += EventHandler{ SELF , @TimerTicked() }
	RETURN

	VIRTUAL METHOD TimerTicked(o AS OBJECT,e AS EventArgs) AS VOID
		// Check periodically if the WED is active and if not, hide the toolwindows
		IF SELF:oGrid != NULL .and. SELF:oGrid:oActiveDesigner == SELF
			IF !SELF:oSurface:ContainsFocus
				SELF:ShowHideTools(FALSE)
			ENDIF
		ENDIF
	RETURN

	PROPERTY Surface	AS Control GET SELF:oSurface
	PROPERTY Loading	AS LOGIC GET SELF:lLoading

	PROPERTY ReadOnly	AS LOGIC GET lReadOnly		SET lReadOnly := Value
	PROPERTY Standalone AS LOGIC GET lStandalone	SET lStandalone := Value

			
    VIRTUAL METHOD ShowHideTools(lShow AS LOGIC) AS VOID
    	LOCAL oGridForm := NULL AS Form
    	LOCAL oToolForm := NULL AS Form
    	IF SELF:oGrid != NULL
    		oGridForm := SELF:oGrid:FindForm()
    	ENDIF
    	IF SELF:oToolBox != NULL
    		oToolForm := SELF:oToolBox:FindForm()
    	ENDIF
    	IF oGridForm != NULL .and. oToolForm != NULL
    		IF Form.ActiveForm == oGridForm .or. Form.ActiveForm == oToolForm
    			RETURN
    		ENDIF
    		IF lShow
    			oGridForm:Show()
    			oToolForm:Show()
    			SELF:GiveFocus()
    		ELSE
    			IF !SELF:oSurface:ContainsFocus
    				oGridForm:Hide()
    				oToolForm:Hide()
    			END IF
    		END IF
    	ENDIF
    RETURN

	ABSTRACT METHOD GiveFocus() AS VOID
	
	VIRTUAL METHOD BeginAction() AS VOID
	VIRTUAL METHOD EndAction() AS VOID
	VIRTUAL METHOD DoBasicAction(oAction AS DesignerBasicAction , eAction AS DesignerBasicActionType , uData AS ActionData) AS OBJECT
	RETURN NULL
	VIRTUAL METHOD DoAction(eAction AS DesignerActionType) AS VOID
	VIRTUAL METHOD DoAction(eAction AS DesignerActionType , cGuid AS STRING) AS VOID
	VIRTUAL METHOD CanDoAction(eAction AS DesignerActionType) AS LOGIC
	RETURN FALSE

	VIRTUAL METHOD StartAction(eAction AS DesignerBasicActionType , uData AS ActionData) AS OBJECT
		LOCAL oAction AS DesignerBasicAction
		LOCAL oRet AS OBJECT
		
		oAction := DesignerBasicAction{eAction , FALSE}
		SELF:BeginAction()
		oRet := SELF:DoBasicAction(oAction , eAction , uData)
		IF SELF:nAction < SELF:aActions:Count
			SELF:aActions:RemoveRange(SELF:nAction , SELF:aActions:Count - SELF:nAction)
		ENDIF
		SELF:aActions:Add(oAction)
		SELF:nAction ++
		SELF:EndAction()
	RETURN oRet

	VIRTUAL METHOD Undo() AS VOID
		LOCAL oAction,oUndoAction,oTest AS DesignerBasicAction
		LOCAL n AS INT

		IF SELF:lReadOnly .and. !SELF:IsDirty
			RETURN
		ENDIF

		IF SELF:nAction >= 1
			oUndoAction := (DesignerBasicAction)SELF:aActions[SELF:nAction - 1]
			SELF:BeginAction()
			FOR n := oUndoAction:aUndo:Count - 1 DOWNTO 0
				oAction := (DesignerBasicAction)oUndoAction:aUndo[n]
				SELF:DoBasicAction( oUndoAction , oAction:eAction , oAction:uData)
			NEXT
			SELF:nAction --
			IF SELF:nAction != 0
				oTest := (DesignerBasicAction)SELF:aActions[SELF:nAction - 1]
				IF !oTest:lGroup
					SELF:Undo()
				ELSE
					SELF:DoAction(DesignerActionType.DeSelectAll)
					IF oTest:aSelected:Count != 0
						FOR n := 0 UPTO oTest:aSelected:Count - 1
							SELF:DoAction(DesignerActionType.SelectAdd , oTest:aSelected[n])
						NEXT
					ENDIF
				ENDIF
			ENDIF
			SELF:EndAction()
		ENDIF
	RETURN
	
	VIRTUAL METHOD Redo() AS VOID
		LOCAL oAction,oRedoAction AS DesignerBasicAction
		LOCAL n AS INT

		IF SELF:lReadOnly .and. !SELF:IsDirty
			RETURN
		ENDIF

		IF SELF:nAction < SELF:aActions:Count
			SELF:nAction ++
			oRedoAction := (DesignerBasicAction)SELF:aActions[SELF:nAction - 1]
			SELF:BeginAction()
			FOR n := 0 UPTO oRedoAction:aRedo:Count - 1
				oAction := (DesignerBasicAction)oRedoAction:aRedo[n]
				SELF:DoBasicAction( oRedoAction , oAction:eAction , oAction:uData)
			NEXT
			IF !oRedoAction:lGroup
				SELF:Redo()
			ELSE
				SELF:DoAction(DesignerActionType.DeSelectAll)
				IF oRedoAction:aSelected:Count != 0
					FOR n := 0 UPTO oRedoAction:aSelected:Count - 1
						SELF:DoAction(DesignerActionType.SelectAdd , oRedoAction:aSelected[n])
					NEXT
				ENDIF
			ENDIF
			SELF:EndAction()
		ENDIF
	RETURN


	ABSTRACT METHOD Cut() AS VOID
	ABSTRACT METHOD Copy() AS VOID
	ABSTRACT METHOD Paste() AS VOID
	VIRTUAL METHOD SelectAll() AS VOID
	VIRTUAL METHOD TestForm() AS VOID
	VIRTUAL METHOD Delete() AS VOID
	VIRTUAL METHOD ReloadFromFile(cFileName AS STRING) AS VOID
	VIRTUAL METHOD GetName() AS STRING
	RETURN NULL
	VIRTUAL METHOD GetAllDesignItems() AS ArrayList
	RETURN NULL
	VIRTUAL METHOD GetHierarchyItems() AS ArrayList
	RETURN NULL
	VIRTUAL METHOD LoadForm(cFileName AS STRING,cFormName AS STRING) AS VOID
	VIRTUAL METHOD SaveContents(oRead AS System.IO.StreamReader , oStream AS System.IO.StreamWriter) AS VOID
	VIRTUAL METHOD PreSaveContents() AS VOID
	VIRTUAL METHOD PostSaveContents() AS VOID

	VIRTUAL METHOD Open(cFileName AS STRING) AS LOGIC
	RETURN FALSE
	VIRTUAL METHOD Save(cFileName AS STRING) AS LOGIC
	RETURN SELF:Save(cFileName , FALSE)
	VIRTUAL METHOD Save(cFileName AS STRING , lVnfrmOnly AS LOGIC) AS LOGIC
	RETURN FALSE
	VIRTUAL ACCESS IsDirty AS LOGIC
	RETURN FALSE

	PROTECTED SEALED METHOD MakeMenuItem(cText AS STRING , eAction AS DesignerActionType) AS MenuItem
		LOCAL oItem AS MenuItem
		oItem := MenuItem{}
		oItem:Text := cText
		oItem:Tag := eAction
		oItem:Click += EventHandler{SELF,@DesignerBase.ContextMenuClicked()}
	RETURN oItem
	PRIVATE METHOD ContextMenuClicked(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:DoContextAction( (DesignerActionType) ((MenuItem)o):Tag)
	RETURN
	PROTECTED VIRTUAL METHOD DoContextAction(eAction AS DesignerActionType) AS VOID
		SELF:DoAction( eAction )
	RETURN

	METHOD ControlKeyPressedInGrid(eKey AS Keys) AS VOID
		IF eKey == Keys.Control + Keys.S
			IF SELF:TriggerSave != NULL
				SELF:TriggerSave:Invoke(NULL , NULL)
			END IF
		END IF
	RETURN

	STATIC METHOD GetEncoding(oStream AS FileStream) AS Encoding
		LOCAL oBinary AS BinaryReader
		LOCAL oEncoding AS Encoding
		LOCAL aBom AS BYTE[]

		oEncoding := System.Text.Encoding.GetEncoding(0)
		IF oStream:Length != 0
			oBinary := BinaryReader{oStream}
			aBom := oBinary:ReadBytes(Math.Min(4 , (INT)oStream:Length))
			DO CASE
			CASE aBom:Length >= 4 .and. aBom[1] == 0xFF .and. aBom[2] == 0xFE .and. ;
										aBom[3] == 0x00 .and. aBom[4] == 0x00
				oEncoding := Encoding.UTF32
			CASE aBom:Length >= 2 .and. aBom[1] == 0xFF .and. aBom[2] == 0xFE
				oEncoding := Encoding.Unicode
			CASE aBom:Length >= 3 .and. aBom[1] == 0xEF .and. aBom[2] == 0xBB .and. ;
										aBom[3] == 0xBF
				oEncoding := Encoding.UTF8
			END CASE
			oStream:Position := 0
		ENDIF
	RETURN oEncoding
END CLASS


ABSTRACT CLASS WindowDesignerBase INHERIT DesignerBase
	PROTECT oOptions AS WindowDesignerOptions
	PROTECT eCurrent AS WEDAction
	CONSTRUCTOR(_oSurface AS Control , _oOptions AS WindowDesignerOptions)
		SUPER(_oSurface)
		SELF:oOptions := _oOptions
		SELF:eCurrent := WEDAction.None
		
		SELF:oSurface:MouseMove += MouseEventHandler{ SELF , @SurfaceMouseMove() }
		SELF:oSurface:MouseUp += MouseEventHandler{ SELF , @SurfaceMouseUp() }
	RETURN

	VIRTUAL METHOD SelectAll() AS VOID
	VIRTUAL METHOD TestForm() AS VOID
	VIRTUAL METHOD Delete() AS VOID
	VIRTUAL METHOD ReloadFromFile(cFileName AS STRING) AS VOID
	VIRTUAL METHOD GetName() AS STRING
	RETURN NULL
	ABSTRACT INTERNAL METHOD ControlMouseDown(oItem AS DesignWindowItem , eButton AS MouseButtons , oPoint AS Point) AS VOID
	ABSTRACT INTERNAL METHOD ControlMouseUp(oItem AS DesignWindowItem , eButton AS MouseButtons , oPoint AS Point) AS VOID
	ABSTRACT INTERNAL METHOD ControlMouseMove(oItem AS DesignWindowItem , eButton AS MouseButtons , oPoint AS Point) AS VOID
	ABSTRACT INTERNAL METHOD ControlMouseDoubleClick(oItem AS DesignWindowItem , eButton AS MouseButtons , oPoint AS Point) AS VOID
	VIRTUAL METHOD LoadForm(cFileName AS STRING,cFormName AS STRING) AS VOID
	VIRTUAL METHOD SaveContents(oRead AS System.IO.StreamReader , oStream AS System.IO.StreamWriter) AS VOID
	VIRTUAL METHOD PreSaveContents() AS VOID
	VIRTUAL METHOD PostSaveContents() AS VOID

	PROTECTED METHOD SurfaceMouseMove(o AS OBJECT , e AS MouseEventArgs) AS VOID
		LOCAL oPoint AS Point
		oPoint := Point{e:X , e:Y}
		oPoint := SELF:oSurface:PointToScreen(oPoint)
		SELF:ControlMouseMove(NULL , MouseButtons.None , oPoint)
	RETURN
	PROTECTED METHOD SurfaceMouseUp(o AS OBJECT , e AS MouseEventArgs) AS VOID
		LOCAL oPoint AS Point
		oPoint := Point{e:X , e:Y}
		oPoint := SELF:oSurface:PointToScreen(oPoint)
		SELF:ControlMouseUp(NULL , e:Button , oPoint)
	RETURN

	VIRTUAL METHOD PointToGrid(oPoint AS Point) AS Point
		IF SELF:oOptions:lUseGrid
			oPoint:X := SELF:IntToGrid(oPoint:X , SELF:oOptions:oGridSize:Width)
			oPoint:Y := SELF:IntToGrid(oPoint:Y , SELF:oOptions:oGridSize:Height)
		ENDIF
	RETURN oPoint

	VIRTUAL METHOD RectangleToGrid(oRect AS Rectangle) AS Rectangle
		IF SELF:oOptions:lUseGrid
			oRect:X := SELF:IntToGrid(oRect:X , SELF:oOptions:oGridSize:Width)
			oRect:Y := SELF:IntToGrid(oRect:Y , SELF:oOptions:oGridSize:Height)
			oRect:Width := SELF:IntToGrid(oRect:Width , SELF:oOptions:oGridSize:Width)
			oRect:Height := SELF:IntToGrid(oRect:Height , SELF:oOptions:oGridSize:Height)
		ENDIF
	RETURN oRect

	VIRTUAL METHOD SizeToGrid(oSize AS Size) AS Size
		IF SELF:oOptions:lUseGrid
			oSize:Width := SELF:IntToGrid(oSize:Width , SELF:oOptions:oGridSize:Width)
			oSize:Height := SELF:IntToGrid(oSize:Height , SELF:oOptions:oGridSize:Height)
		ENDIF
	RETURN oSize

	VIRTUAL METHOD IntToGridX(x AS INT) AS INT
	RETURN SELF:IntToGrid(x , SELF:oOptions:oGridSize:Width)

	VIRTUAL METHOD IntToGridY(y AS INT) AS INT
    	RETURN SELF:IntToGrid(y , SELF:oOptions:oGridSize:Height)

	VIRTUAL METHOD IntToGrid(n AS INT , nGrid AS INT) AS INT
		IF SELF:oOptions:lUseGrid
			n += nGrid / 2
			n := n - (n % nGrid)
		ENDIF
	RETURN n

	STATIC INTERNAL METHOD HandleWndProc(oDesign AS DesignWindowItem , m REF Message) AS LOGIC
	LOCAL oPoint AS Point
    LOCAL lHandled as LOGIC	
    lHandled := TRUE
	SWITCH m:Msg
	CASE WM_SETFOCUS
		NOP

	CASE WM_NCHITTEST
		m:Result := 0x02

	CASE WM_NCLBUTTONDBLCLK
		oPoint := IntPtrToPoint(m:LParam)
		((VOWindowEditor)oDesign:Designer):ControlMouseDoubleClick(oDesign,MouseButtons.Left,oPoint)
        
	CASE WM_NCMOUSEMOVE
		oPoint := WindowDesignerBase.IntPtrToPoint(m:LParam)
		((VOWindowEditor)oDesign:Designer):ControlMouseMove(oDesign,MouseButtons.None,oPoint)
	
	CASE WM_NCLBUTTONDOWN
		oPoint := WindowDesignerBase.IntPtrToPoint(m:LParam)
		((VOWindowEditor)oDesign:Designer):ControlMouseDown(oDesign,MouseButtons.Left,oPoint)

	CASE WM_NCLBUTTONUP
		oPoint := WindowDesignerBase.IntPtrToPoint(m:LParam)
		((VOWindowEditor)oDesign:Designer):ControlMouseUp(oDesign,MouseButtons.Left,oPoint)
	
	CASE WM_NCRBUTTONDOWN
		oPoint := WindowDesignerBase.IntPtrToPoint(m:LParam)
		((VOWindowEditor)oDesign:Designer):ControlMouseDown(oDesign,MouseButtons.Right,oPoint)

	CASE WM_NCMBUTTONDOWN
		oPoint := WindowDesignerBase.IntPtrToPoint(m:LParam)
		((VOWindowEditor)oDesign:Designer):ControlMouseDown(oDesign,MouseButtons.Middle,oPoint)

	CASE WM_SETCURSOR
		NOP

    OTHERWISE
        lHandled := FALSE
	END SWITCH
	
	RETURN lHandled

    STATIC METHOD IntPtrToPoint(LParam AS IntPtr) AS Point
        LOCAL d := (DWORD)LParam AS DWORD
        LOCAL Hi, Lo AS LONG
        BEGIN UNCHECKED
            Hi := (SHORT) (d >> 16)
            Lo := (SHORT) (d & 0xFFFF)
        END UNCHECKED
        RETURN Point{Lo , Hi}

END CLASS




CLASS DesignItem
	EXPORT aProperties AS ArrayList
	PROTECT oDesigner AS DesignerBase
	EXPORT cGuid AS STRING
	EXPORT lSelected AS LOGIC
	EXPORT lDefault AS LOGIC
	EXPORT lLocked AS LOGIC
	EXPORT aPages AS List<STRING>
	CONSTRUCTOR(_oDesigner AS DesignerBase)
		SUPER()
		SELF:aProperties := ArrayList{}
		SELF:oDesigner := _oDesigner
		SELF:cGuid := Guid.NewGuid():ToString()
	RETURN

	ACCESS Designer() AS DesignerBase
    	RETURN SELF:oDesigner

	METHOD AddProperty(oProp AS DesignProperty) AS VOID
		SELF:aProperties:Add(oProp)
	RETURN

	METHOD GetProperty(nProp AS INT) AS VODesignProperty
	    RETURN (VODesignProperty)SELF:aProperties[nProp]
	
    METHOD GetProperty(cName AS STRING) AS VODesignProperty
		cName := cName:ToUpper()
		FOREACH oProp as VODesignProperty in SELF:aProperties
			IF oProp:Name:ToUpper() == cName
				RETURN oProp
			ENDIF
		NEXT
	RETURN NULL

	METHOD GetPropertyByCaption(cCaption AS STRING) AS VODesignProperty
		cCaption := cCaption:ToUpper()
		FOREACH oProp as VODesignProperty in SELF:aProperties
			IF oProp:Caption:ToUpper() == cCaption
				RETURN oProp
			ENDIF
		NEXT
	RETURN NULL
	
    METHOD GetPropertyByMember(cMember AS STRING) AS VODesignProperty
		cMember := cMember:ToUpper()
		FOREACH oProp as VODesignProperty in SELF:aProperties
			IF oProp:cMember != NULL .and. oProp:cMember:ToUpper() == cMember
				RETURN oProp
			ENDIF
		NEXT
	RETURN NULL
	
    METHOD GetPropertyByMemberAndPos(cMember AS STRING , nMultiPos AS INT) AS VODesignProperty
		cMember := cMember:ToUpper()
		FOREACH oProp as VODesignProperty in SELF:aProperties
			IF oProp:cMember != NULL .and. oProp:cMember:ToUpper() == cMember .and. oProp:nMultiPos == nMultiPos
				RETURN oProp
			ENDIF
		NEXT
	RETURN NULL

END CLASS


