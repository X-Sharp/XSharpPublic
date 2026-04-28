// TextBox.prg
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
    /// VFP TextBox Control - Single-line text input
    /// Maps VFP TextBox properties and methods to WinForms TextBox
    ///
    /// Implements: IVFPObject, IVFPControl, IVFPText, IVFPEditable
    /// Includes: VFPTextBoxText.xh, VFPObject.xh, VFPProperties.xh
    ///           TextControlProperties.xh, FontProperties.xh, ControlSource.xh
    ///
    /// Base Class: System.Windows.Forms.TextBox
    /// </summary>
    PARTIAL CLASS TextBox INHERIT System.Windows.Forms.TextBox IMPLEMENTS IVFPObject, IVFPControl, IVFPText, IVFPEditable

        // ============================================================================
        // Include VFP TextBox-specific text control properties and methods
        // Provides: _vfpValue, _format, _inputMask
        //           Value, Format, InputMask, SelStart, SelLength, SelectAll()
        // ============================================================================
        #include "Headers/VFPTextBoxText.xh"

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

        // ============================================================================
        // Include shared VFP property constants/helpers
        // ============================================================================
        #include "XSharp/VFPProperties.xh"

        // ============================================================================
        // PRIVATE FIELDS
        // ============================================================================

        PRIVATE _selectOnEntry AS LOGIC
        PRIVATE _dragMode AS INT
        PRIVATE _dragIcon AS STRING
        PRIVATE _baseClass AS STRING
        PRIVATE _class AS STRING
        PRIVATE _classLibrary AS STRING
        PRIVATE _comment AS STRING
        PRIVATE _helpContextID AS LONG
        PRIVATE _whatsThisHelpID AS LONG
        PRIVATE _disabledBackColor AS LONG
        PRIVATE _disabledForeColor AS LONG

        // InputMask support - both MaskProvider (runtime) and InputMaskHandler (Next architecture)
        INTERNAL maskCheck AS MaskProvider
        PRIVATE _inputMaskHandler AS InputMaskHandler
        PRIVATE _maskInitialized AS LOGIC

        // Value type tracking for MaskProvider
        PRIVATE _valueType AS STRING

        // Internal value storage (VFP Value can be any type)
        INTERNAL _uValue AS USUAL

        // ============================================================================
        // PROPERTIES
        // ============================================================================

        /// <summary>Format - Display/input format string</summary>
        PRIVATE _format AS STRING
        PROPERTY Format AS STRING
            GET
                IF SELF:DesignMode
                    RETURN _format
                ELSE
                    IF SELF:maskCheck != NULL
                        RETURN SELF:maskCheck:Format
                    ENDIF
                ENDIF
                RETURN _format
            END GET
            SET
                _format := Upper(VALUE)
                IF !SELF:DesignMode
                    IF SELF:maskCheck == NULL
                        IF SELF:_format != NULL
                            SELF:maskCheck := MaskProvider{SELF, SELF:_inputMask, SELF:_format, SELF:_valueType}
                        ENDIF
                    ELSE
                        IF SELF:_inputMask == NULL .AND. SELF:_format == NULL
                            SELF:maskCheck := NULL
                        ELSE
                            VAR savedValue := SELF:Value
                            SELF:maskCheck:Format := SELF:_format
                            SELF:maskCheck:Value := savedValue
                        ENDIF
                    ENDIF
                ENDIF
            END SET
        END PROPERTY

        /// <summary>InputMask - Input validation mask</summary>
        PRIVATE _inputMask AS STRING
        PROPERTY InputMask AS STRING
            GET
                IF SELF:DesignMode
                    RETURN _inputMask
                ELSE
                    IF SELF:maskCheck != NULL
                        RETURN SELF:maskCheck:Mask
                    ENDIF
                ENDIF
                RETURN _inputMask
            END GET
            SET
                _inputMask := VALUE
                IF !SELF:DesignMode
                    VAR savedValue := SELF:Value
                    IF SELF:maskCheck == NULL .AND. (!String.IsNullOrEmpty(SELF:_inputMask) .OR. !String.IsNullOrEmpty(SELF:_format))
                        SELF:maskCheck := MaskProvider{SELF, SELF:_inputMask, SELF:_format, SELF:_valueType}
                        IF !IsNil(savedValue)
                            SELF:maskCheck:Value := savedValue
                        ENDIF
                    ELSE
                        IF String.IsNullOrEmpty(SELF:_inputMask) .AND. String.IsNullOrEmpty(SELF:_format)
                            SELF:maskCheck := NULL
                        ELSE
                            SELF:maskCheck := MaskProvider{SELF, SELF:_inputMask, SELF:_format, SELF:_valueType}
                            IF !IsNil(savedValue)
                                SELF:maskCheck:Value := savedValue
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            END SET
        END PROPERTY

        /// <summary>Value - VFP value (may be any type)</summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
        [EditorBrowsable(EditorBrowsableState.Never)];
        [Bindable(FALSE)];
        [Browsable(FALSE)];
        PROPERTY Value AS USUAL
            GET
                RETURN _uValue
            END GET
            SET
                IF !IsNil(VALUE)
                    _uValue := VALUE
                    _valueType := ValType(_uValue)
                    IF SELF:maskCheck != NULL
                        SELF:maskCheck:Value := _uValue
                    ELSE
                        SELF:Text := ((OBJECT)VALUE):ToString()
                    ENDIF
                ENDIF
            END SET
        END PROPERTY

        /// <summary>Alignment - Text alignment (0=Left, 1=Center, 2=Right)</summary>
        PROPERTY Alignment AS INT
            GET
                RETURN (INT)SELF:TextAlign
            END GET
            SET
                IF VALUE >= 0 .AND. VALUE < 3
                    SELF:TextAlign := (HorizontalAlignment)VALUE
                ENDIF
            END SET
        END PROPERTY

        /// <summary>SelStart - Cursor/selection start position (1-based)</summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
        [EditorBrowsable(EditorBrowsableState.Never)];
        [Bindable(FALSE)];
        [Browsable(FALSE)];
        PROPERTY SelStart AS INT
            GET
                RETURN SELF:SelectionStart
            END GET
            SET
                SELF:SelectionStart := VALUE
            END SET
        END PROPERTY

        /// <summary>Style - VFP TextBox style (placeholder)</summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
        [EditorBrowsable(EditorBrowsableState.Never)];
        [Bindable(FALSE)];
        [Browsable(FALSE)];
        PROPERTY Style AS INT AUTO

        /// <summary>SelectOnEntry - Auto-select all text on focus</summary>
        PROPERTY SelectOnEntry AS LOGIC
            GET
                RETURN SELF:_selectOnEntry
            END GET
            SET
                SELF:_selectOnEntry := VALUE
            END SET
        END PROPERTY

        /// <summary>CursorPos - Position of cursor in edit zone (1-based)</summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
        [EditorBrowsable(EditorBrowsableState.Never)];
        [Bindable(FALSE)];
        [Browsable(FALSE)];
        PROPERTY CursorPos AS LONGINT
            GET
                RETURN SELF:SelectionStart + 1
            END GET
            SET
                IF VALUE > 0
                    SELF:SelectionStart := VALUE - 1
                ELSE
                    SELF:SelectionStart := 0
                ENDIF
                SELF:SelectionLength := 0
            END SET
        END PROPERTY

        /// <summary>SelText - Currently selected text</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.Description("Selected text")];
        [System.ComponentModel.DesignerSerializationVisibility(System.ComponentModel.DesignerSerializationVisibility.Hidden)];
        PROPERTY SelText AS STRING
            GET
                RETURN SELF:SelectedText
            END GET
            SET
                IF SELF:SelectionLength > 0
                    SELF:SelectedText := VALUE
                ENDIF
            END SET
        END PROPERTY

        /// <summary>DisabledBackColor - Background color when disabled</summary>
        [Category("VFP Text")];
        [Description("Background color when disabled")];
        PROPERTY DisabledBackColor AS LONG
            GET
                RETURN SELF:_disabledBackColor
            END GET
            SET
                SELF:_disabledBackColor := VALUE
            END SET
        END PROPERTY

        /// <summary>DisabledForeColor - Foreground color when disabled</summary>
        [Category("VFP Text")];
        [Description("Foreground color when disabled")];
        PROPERTY DisabledForeColor AS LONG
            GET
                RETURN SELF:_disabledForeColor
            END GET
            SET
                SELF:_disabledForeColor := VALUE
            END SET
        END PROPERTY

        // ============================================================================
        // IVFPControl Implementation
        // ============================================================================

        [Category("VFP Behavior")];
        [Description("Drag icon path")];
        [DefaultValue("")];
        PROPERTY DragIcon AS STRING
            GET
                RETURN SELF:_dragIcon
            END GET
            SET
                SELF:_dragIcon := VALUE
            END SET
        END PROPERTY

        [Category("VFP Behavior")];
        [Description("Drag mode (0=manual, 1=automatic)")];
        [DefaultValue(0)];
        PROPERTY DragMode AS LONG
            GET
                RETURN SELF:_dragMode
            END GET
            SET
                SELF:_dragMode := VALUE
            END SET
        END PROPERTY

        PUBLIC METHOD Drag(nAction) AS USUAL CLIPPER
            RETURN NIL
        END METHOD

        PUBLIC METHOD SetFocus() AS VOID STRICT
            SELF:Focus()
        END METHOD

        // ============================================================================
        // VFPAfterWhen hook - called after When() returns TRUE, before GotFocus
        // Must be defined BEFORE TextControlProperties.xh
        // ============================================================================
        #define VFPAfterWhenCall
        PRIVATE METHOD VFPAfterWhen(sender AS OBJECT, e AS System.EventArgs) AS VOID
            IF SELF:SelectOnEntry
                SELF:SelectAll()
            ENDIF
            RETURN

        // ============================================================================
        // EVENT HANDLERS
        // ============================================================================

        /// <summary>Key down - delegate to MaskProvider if active</summary>
        OVERRIDE PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
            IF SELF:maskCheck == NULL
                SUPER:OnKeyDown(e)
                RETURN
            ENDIF
            SELF:maskCheck:OnKeyDown(e)
        END METHOD

        /// <summary>Key up - delegate to MaskProvider if active</summary>
        OVERRIDE PROTECTED METHOD OnKeyUp(e AS KeyEventArgs) AS VOID
            IF SELF:maskCheck == NULL
                SUPER:OnKeyUp(e)
                RETURN
            ENDIF
            // MaskProvider handles internally
        END METHOD

        /// <summary>
        /// Key press - fire VFP KeyPress event ONCE, then process mask
        /// BUG-05 FIX: Current code called OnVFPKeyPress twice; only call once here
        /// </summary>
        OVERRIDE PROTECTED METHOD OnKeyPress(e AS KeyPressEventArgs) AS VOID
            // Fire VFP KeyPress event (once only — BUG-05 fix)
            SELF:OnVFPKeyPress(SELF, e)
            // If NODEFAULT was called, e.Handled = TRUE - skip base processing
            IF e:Handled
                RETURN
            ENDIF
            IF SELF:maskCheck == NULL
                SUPER:OnKeyPress(e)
            ELSE
                IF !SELF:ReadOnly
                    IF SELF:CursorPos == 1
                        SELF:maskCheck:MoveToFirstEditingPosition()
                    ENDIF
                    SELF:maskCheck:OnKeyPress(e)
                ELSE
                    e:Handled := TRUE
                ENDIF
            ENDIF
        END METHOD

        /// <summary>Text changed - update VFP value and fire InteractiveChange</summary>
        OVERRIDE PROTECTED METHOD OnTextChanged(e AS EventArgs) AS VOID
            IF SELF:Modified
                SELF:OnVFPInteractiveChange(SELF, e)
            ENDIF
            _uValue := SELF:Text
            RETURN
        END METHOD

        /// <summary>Lost focus - sync value from MaskProvider or text</summary>
        OVERRIDE PROTECTED METHOD OnLostFocus(e AS EventArgs) AS VOID
            IF SELF:maskCheck != NULL
                SELF:maskCheck:UpdateValue()
            ELSE
                SELF:_uValue := SELF:Text
            ENDIF
            SUPER:OnLostFocus(e)
        END METHOD

        // ============================================================================
        // METHODS
        // ============================================================================

        /// <summary>Copy selected text to clipboard</summary>
        PUBLIC METHOD CopyText() AS VOID
            IF SELF:SelectionLength > 0
                SUPER:Copy()
            ENDIF
        END METHOD

        /// <summary>Cut selected text to clipboard</summary>
        PUBLIC METHOD CutText() AS VOID
            IF SELF:SelectionLength > 0
                SUPER:Cut()
            ENDIF
        END METHOD

        /// <summary>Paste clipboard content</summary>
        PUBLIC METHOD PasteText() AS VOID
            SUPER:Paste()
        END METHOD

        /// <summary>Undo last edit</summary>
        PUBLIC METHOD Undo() AS VOID
            IF SELF:CanUndo
                SUPER:Undo()
            ENDIF
        END METHOD

        // ============================================================================
        // Include text control VFP event wiring (When/Valid/GotFocus/LostFocus etc.)
        // ============================================================================
        #include "Headers/TextControlProperties.xh"

        // ============================================================================
        // Include font properties
        // ============================================================================
        #include "Headers/FontProperties.xh"

        // ============================================================================
        // Include ControlSource data binding support
        // ============================================================================
        #include "Headers/ControlSource.xh"

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR() STRICT
            SUPER()
            SELF:SetStyle(ControlStyles.SupportsTransparentBackColor, TRUE)
            SELF:BackColor := Color.Transparent
            SELF:_format := NULL
            SELF:_inputMask := NULL
            SELF:_valueType := "C"
            SELF:_uValue := NIL
            SELF:_selectOnEntry := FALSE
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "TextBox"
            SELF:_class := "TextBox"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:_disabledBackColor := 0
            SELF:_disabledForeColor := 0
            SELF:_inputMaskHandler := InputMaskHandler{}
            SELF:_maskInitialized := FALSE
            SELF:Multiline := FALSE
            SELF:Size := Size{100, 21}

    END CLASS

END NAMESPACE
