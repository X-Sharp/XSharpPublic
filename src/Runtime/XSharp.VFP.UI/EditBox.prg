// EditBox.prg
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
    /// VFP EditBox Control - Multi-line text editor
    /// Maps VFP EditBox properties and methods to WinForms RichTextBox
    ///
    /// Implements: IVFPObject, IVFPControl, IVFPText, IVFPEditable
    /// Includes: VFPEditBoxText.xh (EditBox-specific, no InputMask)
    ///           TextControlProperties.xh, FontProperties.xh, ControlSource.xh
    ///
    /// Base Class: System.Windows.Forms.RichTextBox
    /// Note: Uses RichTextBox (Next's approach), NOT TextBox (Current's approach)
    /// </summary>
    PARTIAL CLASS EditBox INHERIT System.Windows.Forms.RichTextBox IMPLEMENTS IVFPObject, IVFPControl, IVFPText, IVFPEditable

        // ============================================================================
        // Include VFP EditBox-specific text control properties and methods
        // Note: EditBox does NOT support InputMask per VFP documentation
        // ============================================================================
        #include "Headers/VFPEditBoxText.xh"

        // ============================================================================
        // Include VFPObject base implementation (IVFPObject, IVFPHelp)
        // ============================================================================
        #include "Headers/VFPObject.xh"

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

        // ============================================================================
        // PROPERTIES
        // ============================================================================

        /// <summary>AddLineFeeds - Insert linefeed after carriage return in Value</summary>
        PROPERTY AddLineFeeds AS LOGIC AUTO

        /// <summary>AllowTabs - Allow tab characters in the EditBox</summary>
        PROPERTY AllowTabs AS LOGIC AUTO

        /// <summary>LineCount - Number of lines (read-only)</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.DesignerSerializationVisibility(System.ComponentModel.DesignerSerializationVisibility.Hidden)];
        PROPERTY LineCount AS INT
            GET
                RETURN SELF:Lines:Length
            END GET
        END PROPERTY

        /// <summary>Wrap - Word wrap (0=Off, 1=On)</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.DefaultValue(1)];
        PROPERTY Wrap AS INT
            GET
                RETURN IIF(SELF:WordWrap, 1, 0)
            END GET
            SET
                SELF:WordWrap := (VALUE != 0)
            END SET
        END PROPERTY

        /// <summary>ScrollBars - Scrollbar visibility (0=None, 1=Vert, 2=Horiz, 3=Both)</summary>
        [System.ComponentModel.Category("VFP Properties")];
        [System.ComponentModel.DefaultValue(3)];
        PROPERTY ScrollBars AS INT
            GET
                SWITCH SUPER:ScrollBars
                    CASE RichTextBoxScrollBars.None
                        RETURN 0
                    CASE RichTextBoxScrollBars.Vertical
                        RETURN 1
                    CASE RichTextBoxScrollBars.Horizontal
                        RETURN 2
                    CASE RichTextBoxScrollBars.Both
                        RETURN 3
                    OTHERWISE
                        RETURN 3
                END SWITCH
            END GET
            SET
                SWITCH VALUE
                    CASE 0
                        SUPER:ScrollBars := RichTextBoxScrollBars.None
                    CASE 1
                        SUPER:ScrollBars := RichTextBoxScrollBars.Vertical
                    CASE 2
                        SUPER:ScrollBars := RichTextBoxScrollBars.Horizontal
                    CASE 3
                        SUPER:ScrollBars := RichTextBoxScrollBars.Both
                END SWITCH
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
        // IVFPText Implementation
        // ============================================================================

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
        // IVFPEditable Implementation
        // ============================================================================

        [Category("VFP Behavior")];
        [Description("Auto-select text on entry")];
        [DefaultValue(FALSE)];
        PROPERTY SelectOnEntry AS LOGIC
            GET
                RETURN SELF:_selectOnEntry
            END GET
            SET
                SELF:_selectOnEntry := VALUE
            END SET
        END PROPERTY

        // ============================================================================
        // METHODS
        // ============================================================================

        /// <summary>SelectAll - Select all text</summary>
        PUBLIC OVERRIDE METHOD SelectAll() AS VOID
            SUPER:SelectAll()
        END METHOD

        /// <summary>Clear - Clear all text</summary>
        PUBLIC METHOD Clear() AS VOID
            SUPER:Clear()
            SELF:_vfpValue := NIL
        END METHOD

        /// <summary>Copy - Copy selection to clipboard</summary>
        PUBLIC METHOD CopyText() AS VOID
            IF SELF:SelectionLength > 0
                SUPER:Copy()
            ENDIF
        END METHOD

        /// <summary>Cut - Cut selection to clipboard</summary>
        PUBLIC METHOD CutText() AS VOID
            IF SELF:SelectionLength > 0
                SUPER:Cut()
            ENDIF
        END METHOD

        /// <summary>Paste - Paste from clipboard</summary>
        PUBLIC METHOD PasteText() AS VOID
            SUPER:Paste()
        END METHOD

        /// <summary>Undo - Undo last edit</summary>
        PUBLIC METHOD Undo() AS VOID
            IF SELF:CanUndo
                SUPER:Undo()
            ENDIF
        END METHOD

        /// <summary>Redo - Redo last undo</summary>
        PUBLIC METHOD Redo() AS VOID
            IF SELF:CanRedo
                SUPER:Redo()
            ENDIF
        END METHOD

        /// <summary>FindText - Find text starting at position</summary>
        PUBLIC METHOD FindText(searchStr AS STRING, startPos AS INT) AS INT
            IF startPos < 0
                startPos := 0
            ENDIF
            VAR pos := SELF:Text:IndexOf(searchStr, startPos)
            RETURN pos
        END METHOD

        /// <summary>GetLine - Get text of specific line (1-based)</summary>
        PUBLIC METHOD GetLine(lineNum AS INT) AS STRING
            IF lineNum > 0 .AND. lineNum <= SELF:Lines:Length
                RETURN SELF:Lines[lineNum - 1]
            ENDIF
            RETURN ""
        END METHOD

        // ============================================================================
        // EVENT HANDLERS
        // ============================================================================

        PROTECTED OVERRIDE METHOD OnTextChanged(e AS System.EventArgs) AS VOID
            SUPER:OnTextChanged(e)
            SELF:_vfpValue := SELF:Text
        END METHOD

        // ============================================================================
        // Include text control VFP event wiring
        // ============================================================================
        #include "Headers/TextControlProperties.xh"

        // ============================================================================
        // Include font properties
        // ============================================================================
        #include "Headers/FontProperties.xh"

        // ============================================================================
        // Include ControlSource data binding
        // ============================================================================
        #include "Headers/ControlSource.xh"

        // ============================================================================
        // CONSTRUCTOR
        // ============================================================================

        CONSTRUCTOR()
            SUPER()
            SELF:_vfpValue := NIL
            SELF:_format := ""
            SELF:_selectOnEntry := FALSE
            SELF:_dragMode := 0
            SELF:_dragIcon := ""
            SELF:_baseClass := "EditBox"
            SELF:_class := "EditBox"
            SELF:_classLibrary := ""
            SELF:_comment := ""
            SELF:_helpContextID := 0
            SELF:_whatsThisHelpID := 0
            SELF:_disabledBackColor := 0
            SELF:_disabledForeColor := 0
            SELF:Multiline := TRUE
            SELF:WordWrap := TRUE
            SELF:ScrollBars := RichTextBoxScrollBars.Both
            SELF:Size := Size{100, 75}

    END CLASS

END NAMESPACE
