USING System.ComponentModel

// Class EditBox  BaseClass   Editbox  Class  Editbox
BEGIN NAMESPACE XSharp.VFP.UI
    PARTIAL CLASS EditBox IMPLEMENTS IVFPControl, IVFPText, IVFPEditable
        #include "VFPControl.xh"

        // Alignment — inherited from TextBox (real GET/SET wired to TextAlign)
        // BorderStyle — inherited from WinForms TextBox
        // SelectOnEntry — inherited from TextBox
        // NullDisplay — inherited from TextBox

        PROPERTY BorderColor AS System.Drawing.Color AUTO

        PROPERTY DisabledBackColor AS System.Drawing.Color AUTO

        PROPERTY DisabledForeColor AS System.Drawing.Color AUTO

        PROPERTY EnableHyperLinks AS LOGIC AUTO

        // Format is inherited from TextBox — do not re-declare here

        // HideSelection: WinForms TextBox.HideSelection (LOGIC) is inherited — do not re-declare

        PROPERTY IMEMode AS LONG AUTO

        PROPERTY IntegralHeight AS INT AUTO

        PROPERTY Margin AS LONG AUTO

        // MaxLength: WinForms TextBox.MaxLength (INT) is inherited — do not re-declare as LONG (type mismatch)

        PROPERTY OLEDropTextInsertion AS LONG AUTO

        // PasswordChar: WinForms TextBox.PasswordChar is CHAR — do not re-declare as STRING (type mismatch)

        PROPERTY SelectedBackColor AS System.Drawing.Color AUTO

        PROPERTY SelectedForeColor AS System.Drawing.Color AUTO

        // SelStart, SelLength, SelText — inherited from XSharp.VFP.UI.TextBox; do not re-declare here

        PROPERTY TerminateRead AS LOGIC AUTO

    END CLASS
END NAMESPACE
