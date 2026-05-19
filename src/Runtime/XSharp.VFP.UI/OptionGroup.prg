// OptionGroup.prg
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
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI
/// <summary>
/// VFP-compatible radio-button group that wraps <see cref="System.Windows.Forms.UserControl"/>.<br/>
/// Manages a dynamic collection of <see cref="OptionButton"/> controls via <see cref="ButtonCount"/>
/// (add/remove buttons at runtime); supports a custom factory via <see cref="ButtonFactory"/> for
/// migrated code that uses a custom <c>MemberClass</c>.<br/>
/// <see cref="Value"/> is 1-based (matches VFP): 1 = first button selected, 2 = second, etc., 0 = none.<br/>
/// Fires <c>vfpInteractiveChange</c> when the user selects a button and <see cref="vfpProgrammaticChange"/>
/// when <c>Value</c> is set in code. <see cref="BorderColor"/> paints a custom border in <c>OnPaint</c>;
/// <see cref="BackStyle"/> (0=Transparent, 1=Opaque) controls background rendering.
/// </summary>
	PUBLIC PARTIAL CLASS OptionGroup ;
			INHERIT System.Windows.Forms.UserControl

		#include "ControlProperties.xh"

		#include "FontProperties.xh"

		/// <summary>Number of child controls — returns <c>Controls.Count</c>. The setter is a no-op (ControlCount is read-only in VFP); it exists to satisfy the <c>IVFPOwner</c> interface.</summary>
		PROPERTY ControlCount AS INT
			GET ; RETURN SELF:Controls:Count ; END GET
			SET ; NOP ; END SET
		END PROPERTY

		/// <summary>Colour of the custom border drawn around the group in <c>OnPaint</c>. Set to <c>Color.Empty</c> to suppress the border.</summary>
		PRIVATE _borderColor AS System.Drawing.Color
		PROPERTY BorderColor AS System.Drawing.Color
			GET ; RETURN _borderColor ; END GET
			SET ; _borderColor := VALUE ; SELF:Invalidate() ; END SET
		END PROPERTY

		/// <summary>VFP BackStyle: 0=Transparent (sets <c>BackColor</c> to <c>Transparent</c>), 1=Opaque/default (resets to <c>SystemColors.Control</c>).</summary>
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
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method called when the group control is resized.</summary>
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

		PRIVATE _VFPMoved AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method called when the group control is moved.</summary>
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

		PRIVATE buttons AS List<OptionButton>

		/// <summary>
		/// Optional factory delegate used to create <see cref="OptionButton"/> instances.<br/>
		/// Set by generated code when the VFP form uses a custom <c>MemberClass</c> for the buttons.
		/// When <c>NULL</c>, <see cref="ButtonCount"/> creates plain <see cref="OptionButton"/> objects.
		/// </summary>
		PUBLIC PROPERTY ButtonFactory AS Func<OptionButton> AUTO

		/// <summary>
		/// Number of <see cref="OptionButton"/> controls in the group.<br/>
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
						LOCAL rb AS OptionButton
						rb := IIF(SELF:ButtonFactory != NULL, SELF:ButtonFactory:Invoke(), OptionButton{})
						rb:AutoSize := TRUE
						// C-7: use actual preferred height so larger fonts don't cause overlap
						VAR btnH := rb:PreferredSize:Height
						IF btnH == 0 ; btnH := 21 ; ENDIF
						rb:Location := System.Drawing.Point{6, 11 + (i-1)*(btnH+6)}
						rb:Name := "Option" + i:ToString()
						rb:TabIndex := i - 1
						rb:TabStop := TRUE
						rb:Text := "Option" + i:ToString()
						rb:UseVisualStyleBackColor := TRUE
						// Wire CheckedChanged so Value tracks the selected button
						rb:Tag := i
						rb:CheckedChanged += OnButtonCheckedChanged
						SELF:buttons:Add( rb )
						SELF:gBox:Controls:Add( rb )
					NEXT
				ENDIF
				VAR sizeH := IF(SELF:buttons:Count > 0, SELF:buttons[0]:PreferredSize:Height, 21)
				IF sizeH == 0 ; sizeH := 21 ; ENDIF
				SELF:Size := System.Drawing.Size{91, value*(sizeH+6)+22}
			END SET

		END PROPERTY

		/// <summary>
		/// 1-based index of the currently selected <see cref="OptionButton"/>, or 0 when none is selected.<br/>
		/// Setting selects the button at that index; out-of-range values are silently ignored.
		/// Typed as <c>USUAL</c> so ControlSource binding (which passes <c>USUAL</c>) works without a cast.
		/// Fires <see cref="vfpProgrammaticChange"/> on assignment.
		/// </summary>
		PRIVATE _value AS INT
		PUBLIC PROPERTY Value AS USUAL
			GET
				RETURN SELF:_value
			END GET
			SET
				LOCAL nVal AS INT
				nVal := (INT) VALUE
				IF nVal >= 1 .AND. nVal <= SELF:buttons:Count
					SELF:_value := nVal
					SELF:buttons[nVal - 1]:Checked := TRUE
				ENDIF
				// Programmatic change (set from code, not user interaction)
				SELF:OnVFPProgrammaticChange(SELF, System.EventArgs.Empty)
			END SET
		END PROPERTY

		// ── ProgrammaticChange ────────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method called when <see cref="Value"/> is set programmatically.</summary>
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET _VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange(sender AS OBJECT, e AS System.EventArgs) AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

		PRIVATE METHOD OnButtonCheckedChanged( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			LOCAL rb AS OptionButton
			rb := (OptionButton) sender
			IF rb:Checked
				SELF:_value := (INT) rb:Tag
				SELF:OnVFPInteractiveChange( SELF, e )
			ENDIF

		/// <summary>1-based indexed property — <c>Buttons[i]</c> returns the <see cref="OptionButton"/> at position <c>i</c>. Alias for <see cref="Button"/>.</summary>
		PUBLIC PROPERTY Buttons[ i AS INT ] AS OptionButton
			GET
				RETURN SELF:buttons[ i - 1 ]
			END GET
		END PROPERTY



		CONSTRUCTOR(  )
			SELF:InitializeComponent()
			SELF:buttons := List<OptionButton>{}
			SELF:_value := 0
			SELF:Size := System.Drawing.Size{91, 67}
			RETURN

		/// <summary>Returns the <see cref="OptionButton"/> at the given 1-based index. Equivalent to <c>Buttons[i]</c>.</summary>
		PUBLIC METHOD Button( i AS INT ) AS OptionButton
			RETURN SELF:buttons[ i - 1 ]

#include "ControlSource.xh"

	END CLASS
END NAMESPACE
