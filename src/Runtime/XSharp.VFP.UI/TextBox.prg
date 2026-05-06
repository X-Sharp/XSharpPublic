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
	/// The VFP compatible TextBox class.
	/// </summary>
	PARTIAL CLASS TextBox INHERIT System.Windows.Forms.TextBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include "VFPProperties.xh"

		// Format: VFP @-clause picture string (e.g. "@K", "@!") — independent of InputMask
		PRIVATE _format AS STRING
		PROPERTY Format AS STRING
			GET
				RETURN _format
			END GET
			SET
				_format := Upper( VALUE )
				IF !String.IsNullOrEmpty(_format)
					SELF:CharacterCasing := IIF(_format:Contains("!"), CharacterCasing.Upper, CharacterCasing.Normal)
					IF _format:Contains("K")
						SELF:SelectOnEntry := TRUE
					ENDIF
				ELSE
					SELF:CharacterCasing := CharacterCasing.Normal
				ENDIF
			END SET
		END PROPERTY

		// InputMask: positional mask string (e.g. "999-99-9999")
		// Wired to InputMaskHandler for key routing and display formatting.
		PRIVATE _inputMask AS STRING
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

		// The active InputMaskHandler; NULL when no InputMask is set
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


		OVERRIDE PROTECTED METHOD OnKeyDown( e AS KeyEventArgs ) AS VOID
			IF SELF:_maskHandler == NULL
				SUPER:OnKeyDown(e)
				RETURN
			ENDIF
			SELF:_maskHandler:HandleKeyDown(e, SELF)
			IF !e:Handled
				SUPER:OnKeyDown(e)
			ENDIF

		OVERRIDE PROTECTED METHOD OnKeyUp( e AS KeyEventArgs ) AS VOID
			IF SELF:_maskHandler == NULL
				SUPER:OnKeyUp(e)
				RETURN
			ENDIF
			// Nothing extra needed — base handles caret; mask state updated in TextChanged

        OVERRIDE PROTECTED METHOD OnKeyPress( e AS KeyPressEventArgs) AS VOID
            // Fire VFP KeyPress event once, before default processing
            SELF:OnVFPKeyPress( SELF, e )
            //
            // TODO : Don't forget that if NODEFAULT has been called previously we should mark the Event as handled and return
            //
			IF SELF:_maskHandler == NULL
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

		PROPERTY Alignment AS INT
			GET
				RETURN (INT)SELF:TextAlign
			END GET
			SET
				IF VALUE >= 0 .AND. VALUE < 3
					SELF:TextAlign := (HorizontalAlignment) VALUE
				ENDIF
			END SET
		END PROPERTY

		// RvdH Value in VFP may have ANY type!
		// Therefore store the value in its own slot
		INTERNAL _uValue AS USUAL
		PRIVATE _valueType AS STRING

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
					IF SELF:_maskHandler != NULL
						// Format the value through the mask and display it
						VAR strVal := ((OBJECT)VALUE):ToString()
						SELF:Text := strVal
						SELF:_maskHandler:HandleTextChanged(SELF)
					ELSE
						SELF:Text := ((OBJECT)VALUE):ToString()
					ENDIF
					SELF:OnVFPProgrammaticChange()
				ENDIF
			END SET
		END PROPERTY

				[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
			[EditorBrowsable(EditorBrowsableState.Never)];
			[Bindable(FALSE)];
			[Browsable(FALSE)];
		PROPERTY SelStart AS INT GET SELF:SelectionStart SET SELF:SelectionStart := VALUE

		// VFP Style: 0=Standard (editable), 1=Read-only display (like a label).
		PRIVATE _style AS INT
		PROPERTY Style AS INT
			GET
				RETURN _style
			END GET
			SET
				_style := VALUE
				SELF:ReadOnly := (VALUE == 1)
			END SET
		END PROPERTY

		PROPERTY SelectOnEntry AS LOGIC AUTO

		// ── ProgrammaticChange ───────────────────────────────────────────────

		PRIVATE _VFPProgrammaticChange AS VFPOverride
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
		/// Position of the Cursor in the edit zone of the TextBox. Value is One-Based
		/// </summary>
		/// <value></value>
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

		OVERRIDE PROTECTED METHOD OnLostFocus( e AS EventArgs ) AS VOID
			IF SELF:_maskHandler != NULL
				// Extract clean data value from masked display
				SELF:_uValue := SELF:_maskHandler:GetDataValue(SELF:Text)
			ELSE
				IF !String.IsNullOrEmpty(SELF:NullDisplay) .AND. SELF:Text == SELF:NullDisplay
					SELF:_uValue := NIL
				ELSE
					SELF:_uValue := SELF:Text
				ENDIF
			ENDIF
			SUPER:OnLostFocus( e )
		END METHOD


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

		#include "TextControlProperties.xh"

		#include "FontProperties.xh"

		#include "ControlSource.xh"

		// ── ControlSource ─────────────────────────────────────────────────────
		// VFP ControlSource: "alias.fieldname" or "fieldname" string that registers
		// this control for data binding via Form.DoBindings / Form.PopulateBindings.
		// Setting it calls SetBinding(SELF, VALUE) which populates BindingDefinition.
		PRIVATE _controlSource AS STRING
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
