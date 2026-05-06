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
/// The VFP compatible OptionGroup class.
/// </summary>
	PUBLIC PARTIAL CLASS OptionGroup ;
			INHERIT System.Windows.Forms.UserControl

		#include "ControlProperties.xh"

		// ── ControlCount ──────────────────────────────────────────────────────
		// VFP_CONTROLCOUNT_OVERRIDE suppresses the AUTO stub in VFPContainer.xh.
		PROPERTY ControlCount AS INT
			GET ; RETURN SELF:Controls:Count ; END GET
			SET ; NOP ; END SET
		END PROPERTY

		// ── BorderColor ───────────────────────────────────────────────────────
		PRIVATE _borderColor AS LONG
		PROPERTY BorderColor AS LONG
			GET ; RETURN _borderColor ; END GET
			SET ; _borderColor := VALUE ; SELF:Invalidate() ; END SET
		END PROPERTY

		// ── BackStyle ─────────────────────────────────────────────────────────
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
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

		PRIVATE _VFPMoved AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
			SUPER:OnMove(e)
			IF SELF:_VFPMoved != NULL ; SELF:_VFPMoved:Call() ; ENDIF

		// ── OnPaint — custom BorderColor border ───────────────────────────────
		PROTECTED OVERRIDE METHOD OnPaint(e AS PaintEventArgs) AS VOID
			SUPER:OnPaint(e)
			IF SELF:_borderColor != 0
				VAR c   := System.Drawing.Color.FromArgb(_borderColor & 0xFF, (_borderColor >> 8) & 0xFF, (_borderColor >> 16) & 0xFF)
				VAR pen := Pen{c, (SINGLE)1}
				e:Graphics:DrawRectangle(pen, 0, 0, SELF:ClientSize:Width - 1, SELF:ClientSize:Height - 1)
				pen:Dispose()
			ENDIF

		PRIVATE buttons AS List<OptionButton>

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
						rb := OptionButton{}
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

		// Returns 1-based index of the currently selected button, or 0 if none.
		// Setting Value selects the button at that 1-based index.
		// Typed as USUAL so ControlSource binding (which passes USUAL) works.
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

		// Buttons[i] — 1-based alias for Button(i) matching VFP OptionGroup.Buttons syntax
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

		// 1-based access to individual buttons — matches VFP OptionGroup.Buttons[i]
		PUBLIC METHOD Button( i AS INT ) AS OptionButton
			RETURN SELF:buttons[ i - 1 ]

#include "ControlSource.xh"

	END CLASS
END NAMESPACE
