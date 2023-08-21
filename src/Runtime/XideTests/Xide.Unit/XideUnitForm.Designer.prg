// Application : UnitLibrary
// UnitForm.prg , Created : 6/6/2018   11:06 μμ
// User : Cpc

PARTIAL CLASS UnitForm INHERIT System.Windows.Forms.Form

	PROTECT oInfoText AS System.Windows.Forms.TextBox
	PROTECT oRunSelectedButton AS System.Windows.Forms.Button
	PROTECT oRunAllButton AS System.Windows.Forms.Button
	PROTECT oTestsList AS System.Windows.Forms.ListView
	// User code starts here (DO NOT remove this line)  ##USER##

METHOD InitializeForm() AS VOID
	
	// IDE generated code (please DO NOT modify)
	
		SELF:oInfoText := System.Windows.Forms.TextBox{}
		SELF:oRunSelectedButton := System.Windows.Forms.Button{}
		SELF:oRunAllButton := System.Windows.Forms.Button{}
		SELF:oTestsList := System.Windows.Forms.ListView{}

		SELF:SuspendLayout()

		SELF:ClientSize := System.Drawing.Size{904 , 456}
		SELF:Location := System.Drawing.Point{100 , 100}
		SELF:Name := "UnitForm"
		SELF:Text := "Xide Unit Tests"

		SELF:oInfoText:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Right + System.Windows.Forms.AnchorStyles.Bottom
		SELF:oInfoText:Location := System.Drawing.Point{528 , 40}
		SELF:oInfoText:Multiline := True
		SELF:oInfoText:Name := "InfoText"
		SELF:oInfoText:ScrollBars := System.Windows.Forms.ScrollBars.Vertical
		SELF:oInfoText:Size := System.Drawing.Size{368 , 408}
		SELF:oInfoText:TabIndex := 3
		SELF:Controls:Add(SELF:oInfoText)
		
		SELF:oRunSelectedButton:Click += SELF:RunSelectedButtonClick
		SELF:oRunSelectedButton:Location := System.Drawing.Point{656 , 8}
		SELF:oRunSelectedButton:Name := "RunSelectedButton"
		SELF:oRunSelectedButton:Size := System.Drawing.Size{130 , 30}
		SELF:oRunSelectedButton:TabIndex := 2
		SELF:oRunSelectedButton:Text := "Run selected tests"
		SELF:Controls:Add(SELF:oRunSelectedButton)
		
		SELF:oRunAllButton:Click += SELF:RunAllButtonClick
		SELF:oRunAllButton:Location := System.Drawing.Point{528 , 8}
		SELF:oRunAllButton:Name := "RunAllButton"
		SELF:oRunAllButton:Size := System.Drawing.Size{112 , 30}
		SELF:oRunAllButton:TabIndex := 1
		SELF:oRunAllButton:Text := "Run all tests"
		SELF:Controls:Add(SELF:oRunAllButton)
		
		SELF:oTestsList:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Bottom
		SELF:oTestsList:FullRowSelect := True
		SELF:oTestsList:HideSelection := False
		SELF:oTestsList:Location := System.Drawing.Point{8 , 8}
		SELF:oTestsList:Name := "TestsList"
		SELF:oTestsList:SelectedIndexChanged += SELF:TestsList_SelectedIndexChanged
		SELF:oTestsList:Size := System.Drawing.Size{514 , 440}
		SELF:oTestsList:TabIndex := 0
		SELF:oTestsList:View := System.Windows.Forms.View.Details
		SELF:Controls:Add(SELF:oTestsList)
		
		SELF:ResumeLayout()

	RETURN

END CLASS

