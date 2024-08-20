using System.Windows.Forms
#pragma options("enforceself", on)
FUNCTION Start( ) AS VOID
    var oForm := frmGotoPage{"Goto"}
    oForm:ShowDialog()
    ? oForm:PageNo, oForm:DialogResult:ToString()
RETURN

USING System.Windows.Forms


CLASS frmGotoPage INHERIT System.Windows.Forms.Form
    PRIVATE OkBtn AS System.Windows.Forms.Button
    PRIVATE CancelBtn AS System.Windows.Forms.Button
    PRIVATE FixedText AS System.Windows.Forms.Label
    PRIVATE PageNoEdit AS System.Windows.Forms.TextBox
    EXPORT PageNo AS System.Int32
    /// <summary>
    /// Required designer variable.
    /// </summary>
    CONSTRUCTOR(cCaption AS STRING)

    SUPER()
    SELF:InitializeComponent()
    SELF:Text				:= cCaption
    SELF:PageNoEdit:Text 	:= ""

    //CONSTRUCTOR()
    //  SUPER()
    //  SELF:InitializeComponent()
    //  RETURN

   /// <summary>
   /// Clean up any resources being used.
   /// </summary>
   /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    PROTECTED METHOD Dispose( disposing AS LOGIC ) AS VOID
      SUPER:Dispose( disposing )
      RETURN

   /// <summary>
   /// Required method for Designer support - do not modify
   /// the contents of this method with the code editor.
   /// </summary>
    PRIVATE METHOD InitializeComponent() AS System.Void
        SELF:FixedText := System.Windows.Forms.Label{}
        SELF:OkBtn := System.Windows.Forms.Button{}
        SELF:CancelBtn := System.Windows.Forms.Button{}
        SELF:PageNoEdit := System.Windows.Forms.TextBox{}
        SELF:SuspendLayout()
        //
        // FixedText
        //
        SELF:FixedText:Location := System.Drawing.Point{39, 33}
        SELF:FixedText:Name := "FixedText"
        SELF:FixedText:Size := System.Drawing.Size{139, 23}
        SELF:FixedText:TabIndex := 3
        SELF:FixedText:Text := "Goto Page"
        //
        // OkBtn
        //
        SELF:OkBtn:Location := System.Drawing.Point{65, 96}
        SELF:OkBtn:Name := "OkBtn"
        SELF:OkBtn:Size := System.Drawing.Size{75, 23}
        SELF:OkBtn:TabIndex := 1
        SELF:OkBtn:Text := "Ok"
        SELF:OkBtn:UseVisualStyleBackColor := TRUE
        SELF:OkBtn:Click += System.EventHandler{ SELF, @OkBtn_Click() }
        //
        // CancelBtn
        //
        SELF:CancelBtn:DialogResult := System.Windows.Forms.DialogResult.Cancel
        SELF:CancelBtn:Location := System.Drawing.Point{201, 96}
        SELF:CancelBtn:Name := "CancelBtn"
        SELF:CancelBtn:Size := System.Drawing.Size{75, 23}
        SELF:CancelBtn:TabIndex := 2
        SELF:CancelBtn:Text := "Cancel"
        SELF:CancelBtn:UseVisualStyleBackColor := TRUE
        //
        // PageNoEdit
        //
        SELF:PageNoEdit:Location := System.Drawing.Point{201, 33}
        SELF:PageNoEdit:MaxLength := 5
        SELF:PageNoEdit:Name := "PageNoEdit"
        SELF:PageNoEdit:Size := System.Drawing.Size{75, 20}
        SELF:PageNoEdit:TabIndex := 0
        //
        // frmGotoPage
        //
        SELF:AcceptButton := SELF:OkBtn
        SELF:AutoScaleDimensions := System.Drawing.SizeF{((Single) 6), ((Single) 13)}
        SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
        SELF:CancelButton := SELF:CancelBtn
        SELF:ClientSize := System.Drawing.Size{309, 160}
        SELF:Controls:Add(SELF:PageNoEdit)
        SELF:Controls:Add(SELF:CancelBtn)
        SELF:Controls:Add(SELF:OkBtn)
        SELF:Controls:Add(SELF:FixedText)
        SELF:FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog
        SELF:MaximizeBox := FALSE
        SELF:MinimizeBox := FALSE
        SELF:Name := "frmGotoPage"
        SELF:StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen
        SELF:Text := "Goto Page"
        SELF:ResumeLayout(FALSE)
        SELF:PerformLayout()
    PRIVATE METHOD OkBtn_Click( sender AS System.Object, e AS System.EventArgs ) AS System.Void
        IF !String.IsNullOrEmpty(SELF:PageNoEdit:Text)
            SELF:PageNo:=Int32.Parse(SELF:PageNoEdit:Text)
            IF SELF:PageNo<1
                SELF:PageNo := 1
            ELSEIF SELF:PageNo > 32000
                SELF:PageNo := 32000
            ENDIF
            SELF:DialogResult := DialogResult.OK
        ELSE
            SELF:DialogResult := DialogResult.Cancel
        ENDIF

        RETURN

END CLASS

