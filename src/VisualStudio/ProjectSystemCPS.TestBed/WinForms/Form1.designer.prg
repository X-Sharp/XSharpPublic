BEGIN NAMESPACE WinFormsTestBed

    PARTIAL CLASS Form1

        PRIVATE components := NULL AS System.ComponentModel.IContainer
        PRIVATE button1 AS System.Windows.Forms.Button

        PROTECTED OVERRIDE METHOD Dispose(disposing AS LOGIC) AS VOID STRICT
            IF (disposing .AND. (components != NULL))
                components:Dispose()
            ENDIF
            SUPER:Dispose(disposing)
            RETURN
        END METHOD

        #region Windows Form Designer generated code
        PRIVATE METHOD InitializeComponent() AS VOID STRICT
            SELF:button1 := System.Windows.Forms.Button{}
            SELF:SuspendLayout()
            SELF:button1:Location := System.Drawing.Point{12, 12}
            SELF:button1:Name := "button1"
            SELF:button1:Size := System.Drawing.Size{120, 30}
            SELF:button1:TabIndex := 0
            SELF:button1:Text := "Hello"
            SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
            SELF:ClientSize := System.Drawing.Size{300, 160}
            SELF:Controls:Add(SELF:button1)
            SELF:Name := "Form1"
            SELF:Text := "X# CPS WinForms"
            SELF:ResumeLayout(FALSE)
            RETURN
        END METHOD
        #endregion
    END CLASS

END NAMESPACE
