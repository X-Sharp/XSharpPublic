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
	/// VFP-compatible toolbar that wraps <see cref="System.Windows.Forms.ToolStrip"/>.<br/>
	/// Maintains a 1-based <see cref="Buttons"/> collection of <see cref="VFPToolStripButton"/> items.
	/// Supports VFP properties: <see cref="Movable"/> (grip visibility), <see cref="DockPosition"/>
	/// (0=Top/1=Left/2=Right/3=Bottom/4=Float), <see cref="LockScreen"/> (suspend/resume layout),
	/// <see cref="KeyPreview"/>, and <see cref="Sizable"/> (stub).<br/>
	/// <see cref="AddObject"/> creates <see cref="VFPToolStripButton"/> or <see cref="System.Windows.Forms.ToolStripSeparator"/>
	/// items for the standard VFP class names, and falls back to <c>CreateInstance</c> for others.
	/// </summary>
	PARTIAL CLASS ToolBar INHERIT System.Windows.Forms.ToolStrip

		// Note: VFPObject.xh and VFPProperties.xh are included via ToolBar.generated.prg
		// (through VFPContainer.xh) â€” do not include them again here.
		// ControlProperties.xh wires Move(), SetFocus(), standard VFP event chain.
		#include "ControlProperties.xh"
		#include "FontProperties.xh"

		// â”€â”€ Buttons backing list â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _buttons AS List<VFPToolStripButton>

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:_buttons := List<VFPToolStripButton>{}

		// â”€â”€ Release â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// Hides the toolbar. VFP RELEASE equivalent for toolbars.
		/// </summary>
		METHOD Release() AS USUAL CLIPPER
			SELF:Visible := FALSE
			RETURN NIL

		// â”€â”€ Show / Hide â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// Makes the toolbar visible.
		/// </summary>
		METHOD Show() AS VOID STRICT
			SELF:Visible := TRUE

		/// <summary>
		/// Hides the toolbar without releasing it.
		/// </summary>
		METHOD Hide() AS VOID STRICT
			SELF:Visible := FALSE

		// â”€â”€ hWnd â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// VFP hWnd â€” the native window handle as an integer. The setter is a no-op.
		/// </summary>
		PROPERTY hWnd AS USUAL
			GET
				RETURN SELF:Handle:ToInt32()
			END GET
			SET
				NOP
			END SET
		END PROPERTY

		// â”€â”€ Movable â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// When <c>.T.</c>, shows the toolbar grip so the user can move it; when <c>.F.</c>, hides the grip. Maps to <see cref="System.Windows.Forms.ToolStrip.GripStyle"/>.
		/// </summary>
		PROPERTY Movable AS USUAL
			GET
				RETURN SELF:GripStyle != ToolStripGripStyle.Hidden
			END GET
			SET
				SELF:GripStyle := IIF( (LOGIC) VALUE, ToolStripGripStyle.Visible, ToolStripGripStyle.Hidden )
			END SET
		END PROPERTY

		// â”€â”€ Sizable â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// VFP Sizable stub â€” stored for source compatibility. Not implemented.
		/// </summary>
		PROPERTY Sizable AS USUAL AUTO

		// â”€â”€ KeyPreview â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// VFP KeyPreview stub â€” stored for source compatibility. Not implemented.
		/// </summary>
		PROPERTY KeyPreview AS LOGIC AUTO

		// â”€â”€ LockScreen â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// When <c>.T.</c>, suspends layout updates (<c>SuspendLayout</c>) to batch changes; restoring to <c>.F.</c> resumes layout (<c>ResumeLayout(TRUE)</c>).
		/// </summary>
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

		// â”€â”€ DockPosition â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// VFP dock position: 0=Top, 1=Left, 2=Right, 3=Bottom, 4=Float (undocked). Maps to <see cref="System.Windows.Forms.Control.Dock"/>; Float also shows the grip.
		/// </summary>
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

		// â”€â”€ Buttons collection â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// 1-based indexed access to the button items. Returns the <see cref="VFPToolStripButton"/> at VFP position <paramref name="i"/>.
		/// </summary>
		PROPERTY Buttons[ i AS LONG ] AS VFPToolStripButton
			GET
				RETURN SELF:_buttons[ (INT) i - 1 ]
			END GET
		END PROPERTY

		/// <summary>
		/// Number of <see cref="VFPToolStripButton"/> items currently in <see cref="Buttons"/>.
		/// </summary>
		PROPERTY ButtonCount AS LONG
			GET
				RETURN (LONG) SELF:_buttons:Count
			END GET
		END PROPERTY

		// â”€â”€ AddObject override â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		/// <summary>
		/// Creates a toolbar item at runtime. <c>"CommandButton"</c> â†’ <see cref="VFPToolStripButton"/>;
		/// <c>"Separator"</c> â†’ <see cref="System.Windows.Forms.ToolStripSeparator"/>; other class names
		/// â†’ <c>CreateInstance</c> fallback. The new item is registered as a dynamic property under <paramref name="cName"/>.
		/// </summary>
		METHOD AddObject( cName, cClass, cOLEClass, aInit1, aInit2 ) AS USUAL CLIPPER
			LOCAL sName  AS STRING
			LOCAL sClass AS STRING
			sName  := (STRING) cName
			sClass := ((STRING) cClass):ToUpper()
			// Button-like VFP classes â†’ ToolStripButton
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
			// Separator â†’ ToolStripSeparator
			IF sClass == "SEPARATOR" .OR. sClass == "XSHARP.VFP.UI.SEPARATOR"
				LOCAL sep AS ToolStripSeparator
				sep := ToolStripSeparator{}
				sep:Name := sName
				SELF:Items:Add( sep )
				SELF:AddProperty( sName, sep, PropertyVisibility.Public, "AddObject Separator" )
				RETURN TRUE
			ENDIF
			// Generic fallback â€” delegate to VFPContainer.xh implementation
			// (inherited via #include in ToolBar.generated.prg â†’ VFPContainer.xh)
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
	/// <see cref="System.Windows.Forms.ToolStripButton"/> extended with VFP image-swapping semantics.<br/>
	/// <see cref="Picture"/> loads the normal image; <see cref="DownPicture"/> is shown during a click;
	/// <see cref="DisabledPicture"/> is shown when the button is unavailable. Images are loaded on demand
	/// via <c>VFPTools.ImageFromFile</c>.
	/// </summary>
	CLASS VFPToolStripButton INHERIT System.Windows.Forms.ToolStripButton

		PRIVATE _picture         AS STRING
		PRIVATE _downPicture     AS STRING
		PRIVATE _disabledPicture AS STRING

		/// <summary>
		/// Path to the normal button image. Loaded immediately unless the button is pressed or disabled.
		/// </summary>
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

		/// <summary>
		/// Path to the image shown while the button is being clicked. Loaded transiently in <see cref="OnClick"/>.
		/// </summary>
		PROPERTY DownPicture AS STRING
			GET
				RETURN SELF:_downPicture
			END GET
			SET
				SELF:_downPicture := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Path to the image shown when the button is unavailable (disabled). Applied by <see cref="OnAvailableChanged"/>.
		/// </summary>
		PROPERTY DisabledPicture AS STRING
			GET
				RETURN SELF:_disabledPicture
			END GET
			SET
				SELF:_disabledPicture := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Swaps to <see cref="DisabledPicture"/> when the button becomes unavailable; restores <see cref="Picture"/> when it becomes available again.
		/// </summary>
		PROTECTED OVERRIDE METHOD OnAvailableChanged( e AS System.EventArgs ) AS VOID STRICT
			SUPER:OnAvailableChanged( e )
			IF !SELF:Available .AND. !String.IsNullOrEmpty(SELF:_disabledPicture)
				SELF:Image := VFPTools.ImageFromFile(SELF:_disabledPicture)
			ELSEIF SELF:Available .AND. !String.IsNullOrEmpty(SELF:_picture)
				SELF:Image := VFPTools.ImageFromFile(SELF:_picture)
			ENDIF

		/// <summary>
		/// Temporarily shows <see cref="DownPicture"/> during the click, then restores <see cref="Picture"/> after the base handler fires.
		/// </summary>
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
