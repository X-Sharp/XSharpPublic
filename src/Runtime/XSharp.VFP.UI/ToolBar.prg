// ToolBar.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible ToolBar class.
	/// Maps to System.Windows.Forms.ToolStrip.
	/// </summary>
	PARTIAL CLASS ToolBar INHERIT System.Windows.Forms.ToolStrip

		// Note: VFPObject.xh and VFPProperties.xh are included via ToolBar.generated.prg
		// (through VFPContainer.xh) — do not include them again here.
		// ControlProperties.xh wires Move(), SetFocus(), standard VFP event chain.
		#include "ControlProperties.xh"

		// ── Buttons backing list ──────────────────────────────────────────────
		PRIVATE _buttons AS List<VFPToolStripButton>

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:_buttons := List<VFPToolStripButton>{}

		// ── Release ───────────────────────────────────────────────────────────
		METHOD Release() AS USUAL CLIPPER
			SELF:Visible := FALSE
			RETURN NIL

		// ── Show / Hide ───────────────────────────────────────────────────────
		METHOD Show() AS VOID STRICT
			SELF:Visible := TRUE

		METHOD Hide() AS VOID STRICT
			SELF:Visible := FALSE

		// ── hWnd ──────────────────────────────────────────────────────────────
		PROPERTY hWnd AS USUAL
			GET
				RETURN SELF:Handle:ToInt32()
			END GET
			SET
				NOP
			END SET
		END PROPERTY

		// ── Movable ───────────────────────────────────────────────────────────
		PROPERTY Movable AS USUAL
			GET
				RETURN SELF:GripStyle != ToolStripGripStyle.Hidden
			END GET
			SET
				SELF:GripStyle := IIF( (LOGIC) VALUE, ToolStripGripStyle.Visible, ToolStripGripStyle.Hidden )
			END SET
		END PROPERTY

		// ── Sizable ───────────────────────────────────────────────────────────
		PROPERTY Sizable AS USUAL AUTO

		// ── KeyPreview ────────────────────────────────────────────────────────
		PROPERTY KeyPreview AS LOGIC AUTO

		// ── LockScreen ────────────────────────────────────────────────────────
		PROPERTY LockScreen AS LOGIC
			GET
				RETURN SELF:_lockScreen
			END GET
			SET
				SELF:_lockScreen := VALUE
				IF VALUE
					SELF:SuspendLayout()
				ELSE
					SELF:ResumeLayout(TRUE)
				ENDIF
			END SET
		END PROPERTY
		PRIVATE _lockScreen AS LOGIC

		// ── DockPosition ──────────────────────────────────────────────────────
		// VFP DockPosition: 0=Top, 1=Left, 2=Right, 3=Bottom, 4=Float (undocked)
		PROPERTY DockPosition AS LONG
			GET
				SWITCH SELF:Dock
				CASE DockStyle.Top    ; RETURN 0
				CASE DockStyle.Left   ; RETURN 1
				CASE DockStyle.Right  ; RETURN 2
				CASE DockStyle.Bottom ; RETURN 3
				OTHERWISE             ; RETURN 4   // floating / none
				END SWITCH
			END GET
			SET
				SWITCH VALUE
				CASE 0 ; SELF:Dock := DockStyle.Top
				CASE 1 ; SELF:Dock := DockStyle.Left
				CASE 2 ; SELF:Dock := DockStyle.Right
				CASE 3 ; SELF:Dock := DockStyle.Bottom
				OTHERWISE
					// Float: remove from dock, allow free positioning
					SELF:Dock := DockStyle.None
					SELF:GripStyle := ToolStripGripStyle.Visible
				END SWITCH
			END SET
		END PROPERTY

		// ── Buttons collection ────────────────────────────────────────────────
		// 1-based indexed property: ToolBar.Buttons[i]
		PROPERTY Buttons[ i AS LONG ] AS VFPToolStripButton
			GET
				RETURN SELF:_buttons[ (INT) i - 1 ]
			END GET
		END PROPERTY

		PROPERTY ButtonCount AS LONG
			GET
				RETURN (LONG) SELF:_buttons:Count
			END GET
		END PROPERTY

		// ── AddObject override ────────────────────────────────────────────────
		// VFP: ToolBar.AddObject(cName, "CommandButton" [, caption])
		// If cClass resolves to a button-style VFP class, create a ToolStripButton
		// and add it to Items + _buttons.  Fall through to base for other classes.
		METHOD AddObject( cName, cClass, cOLEClass, aInit1, aInit2 ) AS USUAL CLIPPER
			LOCAL sName  AS STRING
			LOCAL sClass AS STRING
			sName  := (STRING) cName
			sClass := ((STRING) cClass):ToUpper()
			// Button-like VFP classes → ToolStripButton
			IF sClass == "COMMANDBUTTON" .OR. sClass == "XSHARP.VFP.UI.COMMANDBUTTON"
				LOCAL btn AS VFPToolStripButton
				btn := VFPToolStripButton{}
				btn:Name        := sName
				btn:Text        := sName
				btn:DisplayStyle := ToolStripItemDisplayStyle.ImageAndText
				SELF:Items:Add( btn )
				SELF:_buttons:Add( btn )
				// Also register as a dynamic property so VFP name-based access works
				SELF:AddProperty( sName, btn, PropertyVisibility.Public, "AddObject CommandButton" )
				RETURN TRUE
			ENDIF
			// Separator → ToolStripSeparator
			IF sClass == "SEPARATOR" .OR. sClass == "XSHARP.VFP.UI.SEPARATOR"
				LOCAL sep AS ToolStripSeparator
				sep := ToolStripSeparator{}
				sep:Name := sName
				SELF:Items:Add( sep )
				SELF:AddProperty( sName, sep, PropertyVisibility.Public, "AddObject Separator" )
				RETURN TRUE
			ENDIF
			// Generic fallback — delegate to VFPContainer.xh implementation
			// (inherited via #include in ToolBar.generated.prg → VFPContainer.xh)
			// We re-invoke via the base include by calling the method via SUPER chain;
			// since VFPContainer.xh injects it directly into this partial class, we
			// cannot call SUPER:AddObject.  Instead, duplicate the minimal logic here.
			LOCAL oObject AS USUAL
			LOCAL success  AS LOGIC
			success := TRUE
			TRY
				oObject := CreateInstance( "XSharp.VFP.UI." + (STRING) cClass )
			CATCH
				NOP
			END TRY
			IF oObject == NULL
				TRY
					oObject := CreateInstance( (STRING) cClass )
				CATCH
					success := FALSE
				END TRY
			ENDIF
			IF oObject != NULL
				SELF:AddProperty( sName, oObject, PropertyVisibility.Public, "AddObject " + (STRING) cClass )
				TRY
					Send( oObject, "Init" )
				CATCH
					NOP
				END TRY
			ENDIF
			RETURN success

	END CLASS

	/// <summary>
	/// ToolStripButton extended with VFP Picture/DownPicture/DisabledPicture properties.
	/// </summary>
	CLASS VFPToolStripButton INHERIT System.Windows.Forms.ToolStripButton

		PRIVATE _picture         AS STRING
		PRIVATE _downPicture     AS STRING
		PRIVATE _disabledPicture AS STRING

		PROPERTY Picture AS STRING
			GET
				RETURN SELF:_picture
			END GET
			SET
				SELF:_picture := VALUE
				IF !SELF:Pressed .AND. SELF:Enabled
					SELF:Image := IIF( String.IsNullOrEmpty(VALUE), NULL_OBJECT, VFPTools.ImageFromFile(VALUE) )
				ENDIF
			END SET
		END PROPERTY

		PROPERTY DownPicture AS STRING
			GET
				RETURN SELF:_downPicture
			END GET
			SET
				SELF:_downPicture := VALUE
			END SET
		END PROPERTY

		PROPERTY DisabledPicture AS STRING
			GET
				RETURN SELF:_disabledPicture
			END GET
			SET
				SELF:_disabledPicture := VALUE
			END SET
		END PROPERTY

		// Swap image when enabled state changes
		PROTECTED OVERRIDE METHOD OnAvailableChanged( e AS System.EventArgs ) AS VOID STRICT
			SUPER:OnAvailableChanged( e )
			IF !SELF:Available .AND. !String.IsNullOrEmpty(SELF:_disabledPicture)
				SELF:Image := VFPTools.ImageFromFile(SELF:_disabledPicture)
			ELSEIF SELF:Available .AND. !String.IsNullOrEmpty(SELF:_picture)
				SELF:Image := VFPTools.ImageFromFile(SELF:_picture)
			ENDIF

		// Track pressed state to swap DownPicture
		PROTECTED OVERRIDE METHOD OnClick( e AS System.EventArgs ) AS VOID STRICT
			IF !String.IsNullOrEmpty(SELF:_downPicture)
				SELF:Image := VFPTools.ImageFromFile(SELF:_downPicture)
			ENDIF
			SUPER:OnClick( e )
			// Restore normal picture after click
			IF !String.IsNullOrEmpty(SELF:_picture)
				SELF:Image := VFPTools.ImageFromFile(SELF:_picture)
			ENDIF

	END CLASS

END NAMESPACE
