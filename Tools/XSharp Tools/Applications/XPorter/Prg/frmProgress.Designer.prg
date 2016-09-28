
PARTIAL CLASS frmProgress INHERIT System.Windows.Forms.Form

PROTECT otbMessage AS System.Windows.Forms.TextBox
PROTECT obtnClose AS System.Windows.Forms.Button
// User code starts here (DO NOT remove this line)  ##USER##
METHOD InitializeForm() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:otbMessage := System.Windows.Forms.TextBox{}
	SELF:obtnClose := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:BackColor := System.Drawing.SystemColors.InactiveCaption
	SELF:ClientSize := System.Drawing.Size{784 , 456}
	SELF:ControlBox := FALSE
	SELF:FormBorderStyle := System.Windows.Forms.FormBorderStyle.Sizable
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:MaximizeBox := FALSE
	SELF:MinimizeBox := FALSE
	SELF:Name := "frmProgress"
	SELF:StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen
	SELF:Text := "Export Progress"

	SELF:otbMessage:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Right + System.Windows.Forms.AnchorStyles.Bottom
	SELF:otbMessage:BackColor := System.Drawing.SystemColors.InactiveCaption
	SELF:otbMessage:Location := System.Drawing.Point{8 , 16}
	SELF:otbMessage:Multiline := TRUE
	SELF:otbMessage:Name := "tbMessage"
	SELF:otbMessage:ScrollBars := System.Windows.Forms.ScrollBars.Both
	SELF:otbMessage:Size := System.Drawing.Size{768 , 392}
	SELF:otbMessage:TabIndex := 0
	SELF:Controls:Add(SELF:otbMessage)
	
	SELF:obtnClose:Anchor := System.Windows.Forms.AnchorStyles.Right + System.Windows.Forms.AnchorStyles.Bottom
	SELF:obtnClose:Click += SELF:btnCloseClick
	SELF:obtnClose:Location := System.Drawing.Point{696 , 424}
	SELF:obtnClose:Name := "btnClose"
	SELF:obtnClose:Size := System.Drawing.Size{75 , 23}
	SELF:obtnClose:TabIndex := 1
	SELF:obtnClose:Text := "&Close"
	SELF:Controls:Add(SELF:obtnClose)
	
	SELF:ResumeLayout()

RETURN
END CLASS
