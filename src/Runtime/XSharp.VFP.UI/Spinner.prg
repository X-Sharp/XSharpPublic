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
	/// VFP-compatible numeric spinner control.<br/>
	/// Extends <see cref="System.Windows.Forms.NumericUpDown"/> with VFP properties:
	/// Format/InputMask (DecimalPlaces + ThousandsSeparator), SpinnerHighValue/SpinnerLowValue
	/// (arrow limits), KeyboardHighValue/KeyboardLowValue (typed-entry limits), Alignment,
	/// SelectOnEntry, and the VFP events UpClick, DownClick, InteractiveChange, ProgrammaticChange.<br/>
	/// Format codes K (select on entry) and Z (blank when zero) are supported.
	/// </summary>
	PARTIAL CLASS Spinner INHERIT System.Windows.Forms.NumericUpDown
		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include "VFPProperties.xh"

		#include "FontProperties.xh"

		#include "ControlProperties.xh"

		#include "ControlSource.xh"

		// ── SelStart / SelLength / SelText ───────────────────────────────────
		/// <summary>
		/// Accessor for the internal <see cref="System.Windows.Forms.TextBox"/> embedded inside <see cref="System.Windows.Forms.NumericUpDown"/>. Used to implement <see cref="SelStart"/>, <see cref="SelLength"/>, and <see cref="SelText"/>.
		/// </summary>
		PRIVATE PROPERTY _editBox AS System.Windows.Forms.TextBox
			GET
				IF SELF:Controls:Count > 0 .AND. SELF:Controls[0] IS System.Windows.Forms.TextBox VAR tb
					RETURN tb
				ENDIF
				RETURN NULL
			END GET
		END PROPERTY

		/// <summary>
		/// VFP SelStart — zero-based caret position within the spinner's internal text box.
		/// </summary>
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

		/// <summary>
		/// VFP SelLength — number of selected characters in the spinner's internal text box.
		/// </summary>
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

		/// <summary>
		/// VFP SelText — the currently selected text in the spinner's internal text box.
		/// </summary>
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

		PRIVATE _readOnly AS LOGIC
		/// <summary>
		/// When <c>.T.</c>, prevents the user from typing a value directly.
		/// The up/down arrow buttons continue to work normally.
		/// </summary>
		PROPERTY ReadOnly AS LOGIC
			GET ; RETURN _readOnly ; END GET
			SET ; _readOnly := VALUE ; END SET
		END PROPERTY

		/// <summary>
		/// Upper bound for the spinner arrow buttons. Maps to <see cref="System.Windows.Forms.NumericUpDown.Maximum"/>.
		/// </summary>
		PROPERTY SpinnerHighValue AS FLOAT GET (FLOAT) SUPER:Maximum SET SUPER:Maximum := (System.Decimal) VALUE
		/// <summary>
		/// Lower bound for the spinner arrow buttons. Maps to <see cref="System.Windows.Forms.NumericUpDown.Minimum"/>.
		/// </summary>
		PROPERTY SpinnerLowValue AS FLOAT GET (FLOAT) SUPER:Minimum SET SUPER:Minimum := (System.Decimal) VALUE

		PRIVATE _keyboardHigh AS System.Decimal
		PRIVATE _keyboardLow  AS System.Decimal

		/// <summary>
		/// Upper bound for values typed directly by the user — independent of <see cref="SpinnerHighValue"/>.<br/>
		/// Enforced in <c>OnValidating</c>: if the typed value exceeds this limit it is clamped silently.
		/// Defaults to <see cref="SpinnerHighValue"/> at construction time.
		/// </summary>
		PROPERTY KeyboardHighValue AS FLOAT
			GET
				RETURN (FLOAT) SELF:_keyboardHigh
			END GET
			SET
				SELF:_keyboardHigh := (System.Decimal) VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Lower bound for values typed directly by the user — independent of <see cref="SpinnerLowValue"/>.<br/>
		/// Enforced in <c>OnValidating</c>: if the typed value falls below this limit it is clamped silently.
		/// Defaults to <see cref="SpinnerLowValue"/> at construction time.
		/// </summary>
		PROPERTY KeyboardLowValue AS FLOAT
			GET
				RETURN (FLOAT) SELF:_keyboardLow
			END GET
			SET
				SELF:_keyboardLow := (System.Decimal) VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// VFP text alignment: 0=Left (default), 1=Right, 2=Center.
		/// Maps to <see cref="System.Windows.Forms.NumericUpDown.TextAlign"/>.
		/// </summary>
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

		/// <summary>
		/// Fires <c>vfpKeyPress</c> first, then suppresses all keystrokes when <see cref="ReadOnly"/> is <c>.T.</c> so the user cannot type directly (arrow buttons continue to work).
		/// </summary>
		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SELF:OnVFPKeyPress(SELF, e)
			IF _readOnly
				e:Handled := TRUE
				RETURN
			ENDIF
			SUPER:OnKeyPress(e)

		/// <summary>
		/// Clamps the typed value to [<see cref="KeyboardLowValue"/>, <see cref="KeyboardHighValue"/>]
		/// when those limits differ from the arrow limits. No error is shown — the value is silently adjusted.
		/// </summary>
		PROTECTED OVERRIDE METHOD OnValidating( e AS System.ComponentModel.CancelEventArgs ) AS VOID STRICT
			SUPER:OnValidating( e )
			// Clamp typed value to keyboard limits (if they differ from arrow limits)
			IF SUPER:Value > SELF:_keyboardHigh .AND. SELF:_keyboardHigh > SELF:_keyboardLow
				SUPER:Value := SELF:_keyboardHigh
			ELSEIF SUPER:Value < SELF:_keyboardLow
				SUPER:Value := SELF:_keyboardLow
			ENDIF

		/// <summary>
		/// The current numeric value of the spinner as a VFP USUAL (returned as FLOAT).<br/>
		/// Accepts INT, FLOAT, DECIMAL and LOGIC (.T.=1, .F.=0) on set; the value is silently
		/// clamped to [<see cref="SpinnerLowValue"/>, <see cref="SpinnerHighValue"/>].
		/// Fires <c>vfpProgrammaticChange</c> after each programmatic assignment.
		/// </summary>
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

		PRIVATE _format        AS STRING
		PRIVATE _inputMask     AS STRING
		PRIVATE _blankWhenZero AS LOGIC

		/// <summary>
		/// VFP Format function codes for the spinner (no leading @ required).<br/>
		/// In addition to driving <see cref="System.Windows.Forms.NumericUpDown.DecimalPlaces"/> and
		/// <see cref="System.Windows.Forms.NumericUpDown.ThousandsSeparator"/> (same as InputMask),
		/// the following codes are honoured:
		/// <list type="table">
		/// <item><term>K</term><description>Select all text when the control receives focus.</description></item>
		/// <item><term>Z</term><description>Display blank instead of zero when the control loses focus; restore on re-entry.</description></item>
		/// </list>
		/// </summary>
		PROPERTY Format AS STRING
			GET
				RETURN _format
			END GET
			SET
				_format := Upper(VALUE)
				SELF:_ApplyNumericMask(_format)
				SELF:SelectOnEntry   := !String.IsNullOrEmpty(_format) .AND. _format:Contains("K")
				SELF:_blankWhenZero  := !String.IsNullOrEmpty(_format) .AND. _format:Contains("Z")
			END SET
		END PROPERTY

		/// <summary>
		/// VFP positional numeric mask (e.g. <c>"9999.99"</c>, <c>"9,999.99"</c>).<br/>
		/// The number of digits after the decimal point sets <see cref="System.Windows.Forms.NumericUpDown.DecimalPlaces"/>;
		/// a comma in the mask enables <see cref="System.Windows.Forms.NumericUpDown.ThousandsSeparator"/>.
		/// </summary>
		PROPERTY InputMask AS STRING
			GET
				RETURN _inputMask
			END GET
			SET
				_inputMask := VALUE
				SELF:_ApplyNumericMask(VALUE)
			END SET
		END PROPERTY

		/// <summary>
		/// Parses a VFP numeric mask string: counts digits after the decimal point to set <see cref="System.Windows.Forms.NumericUpDown.DecimalPlaces"/>, and enables <see cref="System.Windows.Forms.NumericUpDown.ThousandsSeparator"/> when a comma is present.
		/// </summary>
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

		/// <summary>
		/// Detects the direction of change to dispatch <see cref="UpClick"/> or <see cref="DownClick"/>, then fires <c>vfpInteractiveChange</c> for user-initiated edits.
		/// </summary>
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
		/// <summary>
		/// Name of the VFP method called when the spinner's up arrow is clicked. Fired by <see cref="UpClick"/>.
		/// </summary>
		[Category("VFP Events"), Description("Occurs when the user clicks the up arrow of a Spinner.")];
		[DefaultValue(NULL)];
		PROPERTY vfpUpClick AS STRING GET _VFPUpClick?:SendTo SET SELF:_VFPUpClick := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPUpClick() AS VOID
			IF SELF:_VFPUpClick != NULL
				SELF:_VFPUpClick:Call()
			ENDIF

		/// <summary>
		/// Fires the <c>vfpUpClick</c> event. Called automatically when the value increases via the up arrow.
		/// </summary>
		METHOD UpClick() AS VOID STRICT
			SELF:OnVFPUpClick()
		END METHOD

		// ── vfpDownClick ─────────────────────────────────────────────────────
		PRIVATE _VFPDownClick AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the spinner's down arrow is clicked. Fired by <see cref="DownClick"/>.
		/// </summary>
		[Category("VFP Events"), Description("Occurs when the user clicks the down arrow of a Spinner.")];
		[DefaultValue(NULL)];
		PROPERTY vfpDownClick AS STRING GET _VFPDownClick?:SendTo SET SELF:_VFPDownClick := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPDownClick() AS VOID
			IF SELF:_VFPDownClick != NULL
				SELF:_VFPDownClick:Call()
			ENDIF

		/// <summary>
		/// Fires the <c>vfpDownClick</c> event. Called automatically when the value decreases via the down arrow.
		/// </summary>
		METHOD DownClick() AS VOID STRICT
			SELF:OnVFPDownClick()
		END METHOD

		// ── vfpProgrammaticChange ────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when <see cref="Value"/> is changed programmatically. Fired by the <see cref="Value"/> setter via <see cref="VFPOverride"/>.
		/// </summary>
		[Category("VFP Events"), Description("Occurs when the value of a control is changed through code.")];
		[DefaultValue(NULL)];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET SELF:_VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange() AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

		// ── DisabledBackColor / DisabledForeColor ────────────────────────────

		/// <summary>
		/// Applies <see cref="DisabledBackColor"/> and <see cref="DisabledForeColor"/> when the control is disabled; resets to system colors when re-enabled.
		/// </summary>
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

		// ── SelectOnEntry / Z (blank when zero) ─────────────────────────────────

		/// <summary>
		/// Restores the numeric display if it was blanked by the Z format code, then
		/// selects all text when <see cref="SelectOnEntry"/> is <c>.T.</c>.
		/// </summary>
		PROTECTED OVERRIDE METHOD OnGotFocus(e AS System.EventArgs) AS VOID
			SUPER:OnGotFocus(e)
			// Z: restore numeric display when the field receives focus (was blanked on leave)
			IF SELF:_blankWhenZero
				LOCAL tb := SELF:_editBox AS System.Windows.Forms.TextBox
				IF tb != NULL .AND. String.IsNullOrEmpty(tb:Text)
					SELF:UpdateEditText()
				ENDIF
			ENDIF
			IF SELF:SelectOnEntry
				SELF:Select(0, SELF:Text:Length)
			ENDIF

		/// <summary>
		/// When Format code Z is active and the current value is zero, blanks the
		/// display text so the field appears empty. The underlying value remains 0.
		/// </summary>
		PROTECTED OVERRIDE METHOD OnLostFocus(e AS System.EventArgs) AS VOID
			SUPER:OnLostFocus(e)
			// Z: blank the display when value is zero and the field loses focus
			IF SELF:_blankWhenZero .AND. SUPER:Value == 0
				LOCAL tb := SELF:_editBox AS System.Windows.Forms.TextBox
				IF tb != NULL
					tb:Text := ""
				ENDIF
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
