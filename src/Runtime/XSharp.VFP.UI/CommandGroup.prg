// CommandGroup.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Drawing.Drawing2D
USING System.Text
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// VFP-compatible command-button group that wraps <see cref="System.Windows.Forms.UserControl"/>.<br/>
	/// Manages a dynamic collection of <see cref="CommandButton"/> controls via <see cref="ButtonCount"/>
	/// (add/remove at runtime); supports a custom factory via <see cref="ButtonFactory"/> for migrated
	/// code that uses a custom <c>MemberClass</c>.<br/>
	/// <see cref="Value"/> holds the 1-based index of the last clicked button (0 initially). Fires
	/// <c>vfpInteractiveChange</c> on button click and <see cref="vfpProgrammaticChange"/> when
	/// <c>Value</c> is set in code. <see cref="BorderColor"/> paints a custom border in <c>OnPaint</c>;
	/// <see cref="BackStyle"/> (0=Transparent, 1=Opaque) controls background rendering.
	/// </summary>
	PUBLIC PARTIAL CLASS CommandGroup ;
	INHERIT System.Windows.Forms.UserControl IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

		#include "ControlProperties.xh"

		/// <summary>
		/// Number of child controls — returns <c>Controls.Count</c>. The setter is a no-op; it exists to satisfy the <c>IVFPOwner</c> interface (ControlCount is read-only in VFP).
		/// </summary>
		PROPERTY ControlCount AS INT
			GET ; RETURN SELF:Controls:Count ; END GET
			SET ; NOP ; END SET
		END PROPERTY

		/// <summary>
		/// Colour of the custom border drawn around the group in <c>OnPaint</c>. Set to <c>Color.Empty</c> to suppress the border.
		/// </summary>
		PRIVATE _borderColor AS System.Drawing.Color
		PROPERTY BorderColor AS System.Drawing.Color
			GET ; RETURN _borderColor ; END GET
			SET ; _borderColor := VALUE ; SELF:Invalidate() ; END SET
		END PROPERTY

		/// <summary>
		/// VFP BackStyle: 0=Transparent (sets <c>BackColor</c> to <c>Transparent</c>), 1=Opaque/default (resets to <c>SystemColors.Control</c>).
		/// </summary>
		PRIVATE _backStyle := 1 AS INT
		PROPERTY BackStyle AS INT
			GET ; RETURN _backStyle ; END GET
			SET
				_backStyle := VALUE
				SELF:BackColor := IIF(VALUE == 0, System.Drawing.Color.Transparent, System.Drawing.SystemColors.Control)
			END SET
		END PROPERTY

		// ── Resize / Moved events ─────────────────────────────────────────────
		PRIVATE _VFPResize AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the group control is resized.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

		PRIVATE _VFPMoved AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the group control is moved.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
			SUPER:OnMove(e)
			IF SELF:_VFPMoved != NULL ; SELF:_VFPMoved:Call() ; ENDIF

		// ── OnPaint — custom BorderColor border ───────────────────────────────
		PROTECTED OVERRIDE METHOD OnPaint(e AS PaintEventArgs) AS VOID
			SUPER:OnPaint(e)
			IF SELF:_borderColor != System.Drawing.Color.Empty
				VAR pen := Pen{_borderColor, (SINGLE)1}
				e:Graphics:DrawRectangle(pen, 0, 0, SELF:ClientSize:Width - 1, SELF:ClientSize:Height - 1)
				pen:Dispose()
			ENDIF

		PRIVATE buttons AS List<CommandButton>

		/// <summary>
		/// Optional factory delegate used to create <see cref="CommandButton"/> instances.<br/>
		/// Set by generated code when the VFP form uses a custom <c>MemberClass</c> for the buttons.
		/// When <c>NULL</c>, <see cref="ButtonCount"/> creates plain <see cref="CommandButton"/> objects.
		/// </summary>
		PUBLIC PROPERTY ButtonFactory AS Func<CommandButton> AUTO

		/// <summary>
		/// Number of <see cref="CommandButton"/> controls in the group.<br/>
		/// Setting a smaller value removes and disposes excess buttons; setting a larger value
		/// creates new ones (using <see cref="ButtonFactory"/> if set). The group control resizes
		/// vertically to fit the new button count.
		/// </summary>
		PUBLIC PROPERTY ButtonCount AS INT
			GET
				RETURN SELF:buttons:Count
			END GET

			SET
				// Remove excess buttons
				IF value < SELF:buttons:Count
					FOR VAR r := SELF:buttons:Count - 1 DOWNTO value
						SELF:gBox:Controls:Remove( SELF:buttons[r] )
						SELF:buttons[r]:Dispose()
					NEXT
					SELF:buttons:RemoveRange( value, SELF:buttons:Count - value )
				// Add missing buttons
				ELSEIF value > SELF:buttons:Count
					FOR VAR i := SELF:buttons:Count + 1 TO value
						LOCAL btn AS CommandButton
						btn := IIF(SELF:ButtonFactory != NULL, SELF:ButtonFactory:Invoke(), CommandButton{})
						btn:AutoSize := TRUE
						btn:Location := System.Drawing.Point{6, 11 + (i-1)*(21+6)}
						btn:Name := "Command" + i:ToString()
						btn:Size := System.Drawing.Size{79, 21}
						btn:TabIndex := i - 1
						btn:TabStop := TRUE
						btn:Text := "Command" + i:ToString()
						btn:UseVisualStyleBackColor := TRUE
						btn:Tag := i
						btn:Click += OnButtonClick
						SELF:buttons:Add( btn )
						SELF:gBox:Controls:Add( btn )
					NEXT
				ENDIF
				SELF:Size := System.Drawing.Size{91, (value)*(21+6)+22}
			END SET

		END PROPERTY

		/// <summary>
		/// 1-based index of the last clicked <see cref="CommandButton"/>, or 0 before any button has been clicked.<br/>
		/// Setting programmatically stores the value and fires <see cref="vfpProgrammaticChange"/>;
		/// it does not visually activate a button (command buttons have no persistent checked state).
		/// </summary>
		PRIVATE _value AS INT
		PUBLIC PROPERTY Value AS INT
			GET
				RETURN SELF:_value
			END GET
			SET
				SELF:_value := VALUE
				// Programmatic change: CommandButton has no CheckedState — just store value
				NOP
				SELF:OnVFPProgrammaticChange(SELF, System.EventArgs.Empty)
			END SET
		END PROPERTY

		// ── ProgrammaticChange ────────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when <see cref="Value"/> is set programmatically.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET _VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange(sender AS OBJECT, e AS System.EventArgs) AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF


		CONSTRUCTOR(  )
			SELF:InitializeComponent()
			SELF:buttons := List<CommandButton>{}
			SELF:_value := 0
			RETURN

		PRIVATE METHOD OnButtonClick( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			LOCAL btn AS System.Windows.Forms.Control
			btn := (System.Windows.Forms.Control) sender
			SELF:_value := (INT) btn:Tag
			SELF:OnVFPInteractiveChange( SELF, e )

		/// <summary>
		/// Returns the <see cref="CommandButton"/> at the given 1-based index. Equivalent to <c>Buttons[i]</c>.
		/// </summary>
		PUBLIC METHOD Button( i AS INT ) AS CommandButton
			RETURN SELF:buttons[ i - 1 ]

		/// <summary>
		/// 1-based indexed property — <c>Buttons[i]</c> returns the <see cref="CommandButton"/> at position <c>i</c>. Alias for <see cref="Button"/>.
		/// </summary>
		PUBLIC PROPERTY Buttons[ i AS INT ] AS CommandButton
			GET
				RETURN SELF:buttons[ i - 1 ]
			END GET
		END PROPERTY

#include "ControlSource.xh"

	END CLASS
END NAMESPACE
