// TextBox.prg
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

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// VFP-compatible single-line text entry control.<br/>
	/// Extends the WinForms TextBox with VFP properties (Format, InputMask, Value, ControlSource,
	/// Alignment, Style, CursorPos) and full InputMask-based keystroke validation via
	/// <see cref="InputMaskHandler"/>.
	/// </summary>
	PARTIAL CLASS TextBox INHERIT System.Windows.Forms.TextBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include "VFPProperties.xh"

		PRIVATE _format        AS STRING
		PRIVATE _blankWhenZero AS LOGIC
		PRIVATE _forceAlpha    AS LOGIC
		PRIVATE _noTrailingPad AS LOGIC
		PRIVATE _trimBlanks    AS LOGIC
		PRIVATE _leadingZeros  AS LOGIC

		/// <summary>
		/// VFP Format function codes that control display and entry behaviour (no leading @ required).<br/>
		/// Supported codes for TextBox and Column:
		/// <list type="table">
		/// <item><term>!</term><description>Force uppercase display (maps to CharacterCasing.Upper).</description></item>
		/// <item><term>A</term><description>Accept alphabetic characters only when no InputMask is active.</description></item>
		/// <item><term>F</term><description>Trim trailing spaces from the stored value (Varchar fields).</description></item>
		/// <item><term>K</term><description>Select all text when the control receives focus.</description></item>
		/// <item><term>L</term><description>Pad numeric display with leading zeros up to MaxLength.</description></item>
		/// <item><term>R</term><description>Strip mask literals from the stored value (default behaviour — no flag needed).</description></item>
		/// <item><term>S&lt;n&gt;</term><description>Scroll width: sets MaxLength to n.</description></item>
		/// <item><term>T</term><description>Trim leading and trailing blanks from the stored value.</description></item>
		/// <item><term>Z</term><description>Display blank instead of zero for numeric values.</description></item>
		/// </list>
		/// </summary>
		PROPERTY Format AS STRING
			GET
				RETURN _format
			END GET
			SET
				_format := Upper( VALUE )
				IF !String.IsNullOrEmpty(_format)
					SELF:CharacterCasing   := IIF(_format:Contains("!"), CharacterCasing.Upper, CharacterCasing.Normal)
					SELF:SelectOnEntry     := _format:Contains("K")
					SELF:_blankWhenZero   := _format:Contains("Z")
					SELF:_forceAlpha      := _format:Contains("A")
					SELF:_noTrailingPad   := _format:Contains("F")
					SELF:_trimBlanks      := _format:Contains("T")
					SELF:_leadingZeros    := _format:Contains("L")
					// S<n>: set MaxLength to the number following S
					LOCAL sIdx := _format:IndexOf("S") AS INT
					IF sIdx >= 0
						LOCAL numStr := System.Text.StringBuilder{} AS System.Text.StringBuilder
						LOCAL j := sIdx + 1 AS INT
						DO WHILE j < _format:Length .AND. Char.IsDigit(_format[j])
							numStr:Append(_format[j])
							j++
						END DO
						IF numStr:Length > 0
							SELF:MaxLength := Int32.Parse(numStr:ToString())
						ENDIF
					ENDIF
				ELSE
					SELF:CharacterCasing  := CharacterCasing.Normal
					SELF:_blankWhenZero  := FALSE
					SELF:_forceAlpha     := FALSE
					SELF:_noTrailingPad  := FALSE
					SELF:_trimBlanks     := FALSE
					SELF:_leadingZeros   := FALSE
				ENDIF
			END SET
		END PROPERTY

		PRIVATE _inputMask AS STRING

		/// <summary>
		/// VFP positional input mask (e.g. <c>"999-99-9999"</c>, <c>"AAAAAAAAAA"</c>).<br/>
		/// Each character in the mask constrains the corresponding keystroke:
		/// 9=digit, A=letter, X=any, N=alphanumeric, H=hex, L/Y=logical, U=upper letter, W=lower letter.
		/// Literal characters (dashes, parentheses, spaces) are inserted automatically and skipped during entry.<br/>
		/// Setting this property creates an <see cref="InputMaskHandler"/> that intercepts KeyPress,
		/// KeyDown and TextChanged to enforce the mask at runtime.
		/// </summary>
		PROPERTY InputMask AS STRING
			GET
				IF SELF:_maskHandler != NULL .AND. SELF:_maskHandler:Pattern != NULL
					RETURN SELF:_maskHandler:Pattern:MaskString
				ENDIF
				RETURN _inputMask
			END GET
			SET
				_inputMask := VALUE
				IF !SELF:DesignMode
					IF String.IsNullOrEmpty(_inputMask)
						SELF:_maskHandler := NULL
					ELSE
						SELF:_maskHandler := InputMaskHandler{}
						SELF:_maskHandler:SetPattern(_inputMask)
						// Re-apply current value through the new mask
						IF !IsNil(SELF:_uValue)
							SELF:_maskHandler:InitializeTextBox(SELF)
						ENDIF
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// The active <see cref="InputMaskHandler"/> instance; <c>NULL</c> when no <see cref="InputMask"/> is set.
		/// </summary>
		INTERNAL PROPERTY _maskHandler AS InputMaskHandler AUTO




		CONSTRUCTOR(  ) STRICT
			SUPER()

			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent
			// Default work
			SELF:_format     := NULL
			SELF:_inputMask  := NULL
			SELF:_maskHandler := NULL
            SELF:_valueType  := "C"
            SELF:Size        := Size{100,21}


		/// <summary>
		/// Delegates to <see cref="InputMaskHandler.HandleKeyDown"/> when a mask is active; falls through to the base handler otherwise.
		/// </summary>
		OVERRIDE PROTECTED METHOD OnKeyDown( e AS KeyEventArgs ) AS VOID
			IF SELF:_maskHandler == NULL
				SUPER:OnKeyDown(e)
				RETURN
			ENDIF
			SELF:_maskHandler:HandleKeyDown(e, SELF)
			IF !e:Handled
				SUPER:OnKeyDown(e)
			ENDIF

		/// <summary>
		/// Fires the base <c>OnKeyUp</c> so VFP vfpKeyUp subscribers receive the event; mask state is updated in <see cref="OnTextChanged"/> instead.
		/// </summary>
		OVERRIDE PROTECTED METHOD OnKeyUp( e AS KeyEventArgs ) AS VOID
			IF SELF:_maskHandler != NULL
				// mask state updated in TextChanged; still call base so vfpKeyUp subscribers fire
				NOP
			ENDIF
			SUPER:OnKeyUp(e)

		/// <summary>
		/// Fires the VFP <c>vfpKeyPress</c> event first, then enforces Format code <c>A</c> (letters only)
		/// or delegates to <see cref="InputMaskHandler.HandleKeyPress"/> when a mask is active.
		/// </summary>
        OVERRIDE PROTECTED METHOD OnKeyPress( e AS KeyPressEventArgs) AS VOID
            // Fire VFP KeyPress event once, before default processing
            SELF:OnVFPKeyPress( SELF, e )
            //
            // TODO : Don't forget that if NODEFAULT has been called previously we should mark the Event as handled and return
            //
			IF SELF:_maskHandler == NULL
				// @A: letters only when no InputMask is active (InputMask handles its own filtering)
				IF SELF:_forceAlpha .AND. !Char.IsControl(e:KeyChar) .AND. !Char.IsLetter(e:KeyChar)
					e:Handled := TRUE
					System.Media.SystemSounds.Beep:Play()
					RETURN
				ENDIF
                SUPER:OnKeyPress(e)
            ELSE
				IF !SELF:ReadOnly
					SELF:_maskHandler:HandleKeyPress(e, SELF)
					IF !e:Handled
						SUPER:OnKeyPress(e)
					ENDIF
				ELSE
					e:Handled := TRUE
				ENDIF
            ENDIF


		/// <summary>
		/// Fires <c>vfpInteractiveChange</c> when the change originated from the user (<c>Modified = .T.</c>), then forwards to the active <see cref="InputMaskHandler"/> for mask reformatting.
		/// </summary>
		OVERRIDE PROTECTED METHOD OnTextChanged ( e AS EventArgs) AS VOID
			// TRUE is changed by the user
			IF SELF:Modified
				// VFP fallback Code ??
                SELF:OnVFPInteractiveChange( SELF, e )
			ENDIF
			// Let the mask handler reformat the display after each edit
			IF SELF:_maskHandler != NULL
				SELF:_maskHandler:HandleTextChanged(SELF)
			ENDIF
			//
			RETURN

		/// <summary>
		/// VFP text alignment: 0=Left (default), 1=Right, 2=Center.<br/>
		/// Maps to <see cref="System.Windows.Forms.TextBox.TextAlign"/> — note that WinForms uses a
		/// different ordinal for Center (1) and Right (2), so the mapping is adjusted here.
		/// </summary>
		PROPERTY Alignment AS INT
			GET
				SWITCH SELF:TextAlign
				CASE HorizontalAlignment.Left   ; RETURN 0
				CASE HorizontalAlignment.Right  ; RETURN 1
				OTHERWISE                        ; RETURN 2  // Center
				END SWITCH
			END GET
			SET
				SWITCH VALUE
				CASE 0 ; SELF:TextAlign := HorizontalAlignment.Left
				CASE 1 ; SELF:TextAlign := HorizontalAlignment.Right
				CASE 2 ; SELF:TextAlign := HorizontalAlignment.Center
				END SWITCH
			END SET
		END PROPERTY

		INTERNAL _uValue AS USUAL
		PRIVATE _valueType AS STRING

		/// <summary>
		/// The typed value of the control — may be Character, Numeric, Date or Logical.<br/>
		/// Setting Value formats the value through the active InputMask (if any) and updates the display.
		/// Getting Value returns the last stored value; if NullDisplay is set and the field is empty,
		/// returns NullDisplay instead.<br/>
		/// Format flags Z (blank when zero) and L (leading zeros) are applied on set.
		/// The clean value (mask literals stripped) is written back to this property on LostFocus.
		/// </summary>
			[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
				[EditorBrowsable(EditorBrowsableState.Never)];
				[Bindable(FALSE)];
				[Browsable(FALSE)];
		PROPERTY Value AS USUAL
			GET
				IF IsNil(_uValue) .AND. !String.IsNullOrEmpty(SELF:NullDisplay)
					RETURN (USUAL) SELF:NullDisplay
				ENDIF
				RETURN _uValue
			END GET
			SET
				IF ValType(VALUE) == "C" .AND. !String.IsNullOrEmpty(SELF:NullDisplay) .AND. (STRING)VALUE == SELF:NullDisplay
					_uValue := NIL
					SELF:Text := SELF:NullDisplay
					RETURN
				ENDIF
				IF !IsNil( VALUE )
					// Set uValue first, as setting Text will call TextChanged
					_uValue := VALUE
					_valueType := ValType(_uValue )
					// @Z: show blank when the numeric value is zero
					IF SELF:_blankWhenZero .AND. IsNumeric(VALUE) .AND. (REAL8)VALUE == 0
						SELF:Text := ""
						SELF:OnVFPProgrammaticChange()
						RETURN
					ENDIF
					IF SELF:_maskHandler != NULL
						// Format the value through the mask and display it
						VAR strVal := ((OBJECT)VALUE):ToString()
						SELF:Text := strVal
						SELF:_maskHandler:HandleTextChanged(SELF)
					ELSE
						VAR strVal := ((OBJECT)VALUE):ToString()
						// L: pad numeric value with leading zeros up to MaxLength
						IF SELF:_leadingZeros .AND. IsNumeric(VALUE) .AND. SELF:MaxLength > 0
							strVal := strVal:PadLeft(SELF:MaxLength, c'0')
						ENDIF
						SELF:Text := strVal
					ENDIF
					SELF:OnVFPProgrammaticChange()
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// VFP SelStart — zero-based character index of the insertion point or the start of the selection. Maps to <see cref="System.Windows.Forms.TextBox.SelectionStart"/>.
		/// </summary>
				[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
			[EditorBrowsable(EditorBrowsableState.Never)];
			[Bindable(FALSE)];
			[Browsable(FALSE)];
		PROPERTY SelStart AS INT GET SELF:SelectionStart SET SELF:SelectionStart := VALUE

		PRIVATE _style AS INT
		/// <summary>
		/// VFP Style: 0=Standard editable text box (default), 1=Read-only display (equivalent to a Label —
		/// the user cannot edit the content). Maps to <see cref="System.Windows.Forms.TextBox.ReadOnly"/>.
		/// </summary>
		PROPERTY Style AS INT
			GET
				RETURN _style
			END GET
			SET
				_style := VALUE
				SELF:ReadOnly := (VALUE == 1)
			END SET
		END PROPERTY

		/// <summary>
		/// When <c>.T.</c>, selects all text when the control receives focus — equivalent to VFP Format code <c>K</c>.
		/// Also set automatically when <see cref="Format"/> contains <c>"K"</c>.
		/// </summary>
		PROPERTY SelectOnEntry AS LOGIC AUTO

		// ── ProgrammaticChange ───────────────────────────────────────────────

		PRIVATE _VFPProgrammaticChange AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when <see cref="Value"/> is changed programmatically (not by the user). Fires via <see cref="VFPOverride"/>.
		/// </summary>
		[Category("VFP Events"), Description("Occurs when the value of a control is changed through code.")];
		[DefaultValue(NULL)];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET SELF:_VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange() AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

			// This will include a call to VFPAfterWhen() if When() is True, and before VFPGotFocus()
			// It has to be set BEFORE the #include "TextControlProperties.xh"
		#define VFPAfterWhenCall
		PRIVATE METHOD VFPAfterWhen( sender AS OBJECT, e AS System.EventArgs) AS VOID
			//
			IF SELF:SelectOnEntry
				SELF:SelectAll()
			ENDIF
			RETURN

		/// <summary>
		/// One-based character position of the insertion point. VFP equivalent of <c>CursorPos</c>.<br/>
		/// Getting returns <c>SelectionStart + 1</c>; setting moves the caret and clears any selection.
		/// </summary>
				[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
			[EditorBrowsable(EditorBrowsableState.Never)];
			[Bindable(FALSE)];
			[Browsable(FALSE)];
		PROPERTY CursorPos AS LONGINT
			GET
				LOCAL liStart AS LONGINT
				liStart := SELF:SelectionStart+1
				RETURN liStart
			END GET

			SET
				IF (VALUE > 0)
					SELF:SelectionStart := VALUE-1
				ELSE
					SELF:SelectionStart := 0
				ENDIF
				SELF:SelectionLength := 0
			END SET

		END PROPERTY

		/// <summary>
		/// Enforces VFP validation order: date semantics → range → vfpValid → LostFocus.<br/>
		/// When an InputMask is active and the value type is Date, calls
		/// <see cref="InputMaskHandler.CheckDateSemantics"/> first. Then calls
		/// <see cref="InputMaskHandler.CheckRange"/> regardless of type.
		/// Either failure sets <c>e.Cancel = TRUE</c>, keeping focus on the control and
		/// preventing vfpValid from firing on an already-invalid value.
		/// </summary>
		OVERRIDE PROTECTED METHOD OnValidating( e AS System.ComponentModel.CancelEventArgs ) AS VOID
			IF SELF:_maskHandler != NULL
				LOCAL _dateFmt := SELF:_VFPDateFormatPattern() AS STRING
				IF SELF:_valueType == "D"
					IF !SELF:_maskHandler:CheckDateSemantics(SELF, _dateFmt)
						e:Cancel := TRUE
						RETURN
					ENDIF
				ENDIF
				IF !SELF:_maskHandler:CheckRange(SELF, _dateFmt)
					e:Cancel := TRUE
					RETURN
				ENDIF
			ENDIF
			SUPER:OnValidating( e )
		END METHOD

		/// <summary>
		/// Writes the final typed value back to <see cref="Value"/> when the control loses focus.<br/>
		/// With an active InputMask, calls <see cref="InputMaskHandler.GetDataValue"/> to strip
		/// mask literals. Without a mask, applies Format flags F (TrimEnd) and T (Trim) before storing.
		/// </summary>
		OVERRIDE PROTECTED METHOD OnLostFocus( e AS EventArgs ) AS VOID
			IF SELF:_maskHandler != NULL
				// Extract clean data value from masked display
				SELF:_uValue := SELF:_maskHandler:GetDataValue(SELF:Text)
			ELSE
				IF !String.IsNullOrEmpty(SELF:NullDisplay) .AND. SELF:Text == SELF:NullDisplay
					SELF:_uValue := NIL
				ELSE
					VAR stored := SELF:Text
					// F: trim trailing spaces (Varchar — don't store padding)
					IF SELF:_noTrailingPad
						stored := stored:TrimEnd()
					ENDIF
					// T: trim leading and trailing blanks
					IF SELF:_trimBlanks
						stored := stored:Trim()
					ENDIF
					SELF:_uValue := stored
				ENDIF
			ENDIF
			SUPER:OnLostFocus( e )
		END METHOD

		/// <summary>
		/// Builds a .NET <c>DateTime</c> format string from the VFP <c>DateFormat</c>, <c>DateMark</c>, and <c>Century</c> properties. Used by <see cref="OnValidating"/> to pass a locale-aware pattern to the mask handler.
		/// </summary>
		PRIVATE METHOD _VFPDateFormatPattern() AS STRING
			VAR sep := IIF(String.IsNullOrEmpty(SELF:DateMark), "/", SELF:DateMark)
			VAR yr  := IIF(SELF:Century == 1, "yyyy", "yy")
			SWITCH (INT) SELF:DateFormat
				CASE 1  // ANSI: yy.mm.dd
					RETURN yr + "." + "MM" + "." + "dd"
				CASE 2  // British/French: dd/mm/yy
				CASE 3
				CASE 9  // DMY
					RETURN "dd" + sep + "MM" + sep + yr
				CASE 4  // German: dd.mm.yy
					RETURN "dd.MM." + yr
				CASE 5  // Italian: dd-mm-yy
					RETURN "dd-MM-" + yr
				CASE 6  // Japan: yy/mm/dd
				CASE 10 // YMD
					RETURN yr + sep + "MM" + sep + "dd"
				CASE 7  // USA: mm-dd-yy
					RETURN "MM-dd-" + yr
				OTHERWISE // 0=American, 8=MDY: mm/dd/yy
					RETURN "MM" + sep + "dd" + sep + yr
			END SWITCH
		END METHOD


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

		/// <summary>
		/// VFP SelLength — number of selected characters. Maps to <see cref="System.Windows.Forms.TextBox.SelectionLength"/>.
		/// </summary>
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY SelLength AS INT GET SELF:SelectionLength SET SELF:SelectionLength := VALUE

		/// <summary>
		/// VFP SelText — the currently selected text. Maps to <see cref="System.Windows.Forms.TextBox.SelectedText"/>.
		/// </summary>
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY SelText AS STRING GET SELF:SelectedText SET SELF:SelectedText := VALUE

		#include "TextControlProperties.xh"

		#include "FontProperties.xh"

		#include "ControlSource.xh"

		PRIVATE _controlSource AS STRING
		/// <summary>
		/// VFP ControlSource — <c>"alias.fieldname"</c> or <c>"fieldname"</c> string that binds this
		/// control to a data field. Setting this property calls <c>SetBinding()</c> which registers
		/// the control with the form's binding infrastructure (<c>DoBindings</c> / <c>PopulateBindings</c>).
		/// </summary>
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY ControlSource AS STRING
			GET
				RETURN SELF:_controlSource
			END GET
			SET
				SELF:_controlSource := VALUE
				IF !String.IsNullOrEmpty(VALUE)
					SELF:SetBinding(SELF, VALUE)
				ENDIF
			END SET
		END PROPERTY

	END CLASS

END NAMESPACE
