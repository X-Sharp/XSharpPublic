USING System.ComponentModel

// Class EditBox  BaseClass   Editbox  Class  Editbox
BEGIN NAMESPACE XSharp.VFP.UI
    PARTIAL CLASS EditBox IMPLEMENTS IVFPControl, IVFPText, IVFPEditable
        #include "VFPControl.xh"

        // Alignment — inherited from TextBox (real GET/SET wired to TextAlign)
        // BorderStyle — inherited from WinForms TextBox
        // SelectOnEntry — inherited from TextBox
        // NullDisplay — inherited from TextBox

        [DefaultValue(0)];
        PROPERTY BorderColor AS LONG AUTO

        [DefaultValue(0)];
        PROPERTY DisabledBackColor AS LONG AUTO

        [DefaultValue(0)];
        PROPERTY DisabledForeColor AS LONG AUTO

        PROPERTY EnableHyperLinks AS LOGIC AUTO

        // Format is inherited from TextBox — do not re-declare here

        // HideSelection: WinForms TextBox.HideSelection (LOGIC) is inherited — do not re-declare

        PROPERTY IMEMode AS LONG AUTO

        PROPERTY IntegralHeight AS INT AUTO

        PROPERTY Margin AS LONG AUTO

        // MaxLength: WinForms TextBox.MaxLength (INT) is inherited — do not re-declare as LONG (type mismatch)

        PROPERTY OLEDropTextInsertion AS LONG AUTO

        // PasswordChar: WinForms TextBox.PasswordChar is CHAR — do not re-declare as STRING (type mismatch)

        [DefaultValue(0)];
        PROPERTY SelectedBackColor AS LONG AUTO

        [DefaultValue(0)];
        PROPERTY SelectedForeColor AS LONG AUTO

        // SelLength, SelText — mapped to WinForms SelectionLength/SelectedText
        // SelStart is inherited from TextBox — do not re-declare here
        PROPERTY SelLength AS LONG
            GET
                RETURN SELF:SelectionLength
            END GET
            SET
                SELF:SelectionLength := VALUE
            END SET
        END PROPERTY

        // SelStart is inherited from TextBox — do not re-declare here

        PROPERTY SelText AS STRING
            GET
                RETURN SELF:SelectedText
            END GET
            SET
                SELF:SelectedText := VALUE
            END SET
        END PROPERTY

        PROPERTY TerminateRead AS LOGIC AUTO

    END CLASS
END NAMESPACE
