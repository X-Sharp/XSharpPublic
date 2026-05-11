// Spinner.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE  XSharp.VFP.UI

	/// <summary>
    /// The VFP compatible Spinner class.
    /// </summary>
	PARTIAL CLASS Spinner INHERIT System.Windows.Forms.NumericUpDown
		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include "VFPProperties.xh"

		#include "FontProperties.xh"

		#include "ControlProperties.xh"

		#include "ControlSource.xh"

		// ── SelStart / SelLength / SelText ───────────────────────────────────
		// NumericUpDown exposes selection via its internal TextBox (Controls[0]).
		PRIVATE PROPERTY _editBox AS System.Windows.Forms.TextBox
			GET
				IF SELF:Controls:Count > 0 .AND. SELF:Controls[0] IS System.Windows.Forms.TextBox VAR tb
					RETURN tb
				ENDIF
				RETURN NULL
			END GET
		END PROPERTY

		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
        [Bindable(FALSE)];
        [Browsable(FALSE)];
		PROPERTY SelStart AS LONG
			GET
				LOCAL tb := _editBox AS System.Windows.Forms.TextBox
				RETURN IIF(tb != NULL, (LONG)tb:SelectionStart, 0)
			END GET
			SET
				LOCAL tb := _editBox AS System.Windows.Forms.TextBox
				IF tb != NULL ; tb:SelectionStart := VALUE ; ENDIF
			END SET
		END PROPERTY

		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
        [Bindable(FALSE)];
        [Browsable(FALSE)];
		PROPERTY SelLength AS LONG
			GET
				LOCAL tb := _editBox AS System.Windows.Forms.TextBox
				RETURN IIF(tb != NULL, (LONG)tb:SelectionLength, 0)
			END GET
			SET
				LOCAL tb := _editBox AS System.Windows.Forms.TextBox
				IF tb != NULL ; tb:SelectionLength := VALUE ; ENDIF
			END SET
		END PROPERTY

		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
        [Bindable(FALSE)];
        [Browsable(FALSE)];
		PROPERTY SelText AS STRING
			GET
				LOCAL tb := _editBox AS System.Windows.Forms.TextBox
				RETURN IIF(tb != NULL, tb:SelectedText, "")
			END GET
			SET
				LOCAL tb := _editBox AS System.Windows.Forms.TextBox
				IF tb != NULL ; tb:SelectedText := VALUE ; ENDIF
			END SET
		END PROPERTY

		// ── ReadOnly ──────────────────────────────────────────────────────────
		// Prevents keyboard input; arrow-button spin still works.
		PRIVATE _readOnly AS LOGIC
		PROPERTY ReadOnly AS LOGIC
			GET ; RETURN _readOnly ; END GET
			SET ; _readOnly := VALUE ; END SET
		END PROPERTY

		PROPERTY SpinnerHighValue AS FLOAT GET (FLOAT) SUPER:Maximum SET SUPER:Maximum := (System.Decimal) VALUE
		PROPERTY SpinnerLowValue AS FLOAT GET (FLOAT) SUPER:Minimum SET SUPER:Minimum := (System.Decimal) VALUE

		// KeyboardHighValue / KeyboardLowValue are independent from the spinner arrow limits.
		// They constrain what the user can type; enforced in OnValidating.
		PRIVATE _keyboardHigh AS System.Decimal
		PRIVATE _keyboardLow  AS System.Decimal

		PROPERTY KeyboardHighValue AS FLOAT
			GET
				RETURN (FLOAT) SELF:_keyboardHigh
			END GET
			SET
				SELF:_keyboardHigh := (System.Decimal) VALUE
			END SET
		END PROPERTY

		PROPERTY KeyboardLowValue AS FLOAT
			GET
				RETURN (FLOAT) SELF:_keyboardLow
			END GET
			SET
				SELF:_keyboardLow := (System.Decimal) VALUE
			END SET
		END PROPERTY

		// ── Alignment ────────────────────────────────────────────────────────
		// VFP: 0=Left (default), 1=Right, 2=Center — maps to NumericUpDown.TextAlign.
		PROPERTY Alignment AS LONG
			GET
				SWITCH SELF:TextAlign
				CASE HorizontalAlignment.Right  ; RETURN 1
				CASE HorizontalAlignment.Center ; RETURN 2
				OTHERWISE                       ; RETURN 0
				END SWITCH
			END GET
			SET
				SWITCH VALUE
				CASE 1 ; SELF:TextAlign := HorizontalAlignment.Right
				CASE 2 ; SELF:TextAlign := HorizontalAlignment.Center
				OTHERWISE ; SELF:TextAlign := HorizontalAlignment.Left
				END SWITCH
			END SET
		END PROPERTY

		// Fire vfpKeyPress and enforce ReadOnly (suppress typing; arrows still spin).
		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SELF:OnVFPKeyPress(SELF, e)
			IF _readOnly
				e:Handled := TRUE
				RETURN
			ENDIF
			SUPER:OnKeyPress(e)

		PROTECTED OVERRIDE METHOD OnValidating( e AS System.ComponentModel.CancelEventArgs ) AS VOID STRICT
			SUPER:OnValidating( e )
			// Clamp typed value to keyboard limits (if they differ from arrow limits)
			IF SUPER:Value > SELF:_keyboardHigh .AND. SELF:_keyboardHigh > SELF:_keyboardLow
				SUPER:Value := SELF:_keyboardHigh
			ELSEIF SUPER:Value < SELF:_keyboardLow
				SUPER:Value := SELF:_keyboardLow
			ENDIF

		// ── Value wrapper ────────────────────────────────────────────────────
		// VFP Value is USUAL; WinForms NumericUpDown.Value is Decimal.
		// This wrapper accepts numeric values (INT, FLOAT, DECIMAL) and .T./.F.

		NEW PROPERTY Value AS USUAL
			GET
				RETURN (USUAL)(FLOAT) SUPER:Value
			END GET
			SET
				LOCAL dec AS System.Decimal
				DO CASE
				CASE IsLogic(VALUE)
					dec := IIF( (LOGIC)VALUE, 1m, 0m )
				CASE IsFloat(VALUE)
					dec := (System.Decimal)(FLOAT) VALUE
				CASE IsLong(VALUE)
					dec := (System.Decimal)(LONG) VALUE
				OTHERWISE
					TRY
						dec := System.Decimal.Parse( Str(VALUE) )
					CATCH
						RETURN
					END TRY
				END CASE
				IF dec < SUPER:Minimum
					dec := SUPER:Minimum
				ELSEIF dec > SUPER:Maximum
					dec := SUPER:Maximum
				ENDIF
				_isProgrammatic := TRUE
				SUPER:Value := dec
				_isProgrammatic := FALSE
				SELF:OnVFPProgrammaticChange()
			END SET
		END PROPERTY

		// ── Format / InputMask ──────────────────────────────────────────────
		// Both drive NumericUpDown.DecimalPlaces and ThousandsSeparator.
		// "9999.99"  → DecimalPlaces=2
		// "9,999.99" → DecimalPlaces=2, ThousandsSeparator=TRUE

		PRIVATE _format    AS STRING
		PRIVATE _inputMask AS STRING

		PROPERTY Format AS STRING
			GET
				RETURN _format
			END GET
			SET
				_format := VALUE
				SELF:_ApplyNumericMask(VALUE)
			END SET
		END PROPERTY

		PROPERTY InputMask AS STRING
			GET
				RETURN _inputMask
			END GET
			SET
				_inputMask := VALUE
				SELF:_ApplyNumericMask(VALUE)
			END SET
		END PROPERTY

		PRIVATE METHOD _ApplyNumericMask(mask AS STRING) AS VOID
			IF String.IsNullOrEmpty(mask)
				RETURN
			ENDIF
			VAR dotPos := mask:IndexOf(".")
			IF dotPos >= 0
				SELF:DecimalPlaces := mask:Length - dotPos - 1
			ELSE
				SELF:DecimalPlaces := 0
			ENDIF
			SELF:ThousandsSeparator := mask:IndexOf(",") >= 0

		// ── UpClick / DownClick / InteractiveChange / ProgrammaticChange ────────

		PRIVATE _lastSpinnerValue  AS System.Decimal
		PRIVATE _isProgrammatic    AS LOGIC

		PROTECTED METHOD OnValueChanged( e AS System.EventArgs ) AS VOID
			SUPER:OnValueChanged( e )
			IF SUPER:Value > _lastSpinnerValue
				SELF:UpClick()
			ELSEIF SUPER:Value < _lastSpinnerValue
				SELF:DownClick()
			ENDIF
			_lastSpinnerValue := SUPER:Value
			IF !_isProgrammatic
				SELF:OnVFPInteractiveChange( SELF, e )
			ENDIF
		END METHOD

		// ── vfpUpClick ───────────────────────────────────────────────────────
		PRIVATE _VFPUpClick AS VFPOverride
		[Category("VFP Events"), Description("Occurs when the user clicks the up arrow of a Spinner.")];
		[DefaultValue(NULL)];
		PROPERTY vfpUpClick AS STRING GET _VFPUpClick?:SendTo SET SELF:_VFPUpClick := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPUpClick() AS VOID
			IF SELF:_VFPUpClick != NULL
				SELF:_VFPUpClick:Call()
			ENDIF

		METHOD UpClick() AS VOID STRICT
			SELF:OnVFPUpClick()
		END METHOD

		// ── vfpDownClick ─────────────────────────────────────────────────────
		PRIVATE _VFPDownClick AS VFPOverride
		[Category("VFP Events"), Description("Occurs when the user clicks the down arrow of a Spinner.")];
		[DefaultValue(NULL)];
		PROPERTY vfpDownClick AS STRING GET _VFPDownClick?:SendTo SET SELF:_VFPDownClick := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPDownClick() AS VOID
			IF SELF:_VFPDownClick != NULL
				SELF:_VFPDownClick:Call()
			ENDIF

		METHOD DownClick() AS VOID STRICT
			SELF:OnVFPDownClick()
		END METHOD

		// ── vfpProgrammaticChange ────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		[Category("VFP Events"), Description("Occurs when the value of a control is changed through code.")];
		[DefaultValue(NULL)];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET SELF:_VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange() AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

		// ── DisabledBackColor / DisabledForeColor ────────────────────────────

		PROTECTED OVERRIDE METHOD OnEnabledChanged(e AS System.EventArgs) AS VOID
			SUPER:OnEnabledChanged(e)
			IF !SELF:Enabled
				IF SELF:DisabledBackColor != System.Drawing.Color.Empty
					SELF:BackColor := SELF:DisabledBackColor
				ENDIF
				IF SELF:DisabledForeColor != System.Drawing.Color.Empty
					SELF:ForeColor := SELF:DisabledForeColor
				ENDIF
			ELSE
				SELF:ResetBackColor()
				SELF:ResetForeColor()
			ENDIF
		END METHOD

		// ── SelectOnEntry ────────────────────────────────────────────────────

		PROTECTED OVERRIDE METHOD OnGotFocus(e AS System.EventArgs) AS VOID
			SUPER:OnGotFocus(e)
			IF SELF:SelectOnEntry
				SELF:Select(0, SELF:Text:Length)
			ENDIF

		CONSTRUCTOR()
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, true)
            SELF:BackColor := Color.Transparent
            SELF:Size := Size{100,24}
            _lastSpinnerValue := SUPER:Value
            // Initialise keyboard limits to match arrow limits (0..100)
            SELF:_keyboardHigh := SUPER:Maximum
            SELF:_keyboardLow  := SUPER:Minimum

	END CLASS
END NAMESPACE // xsVFPLibrary
