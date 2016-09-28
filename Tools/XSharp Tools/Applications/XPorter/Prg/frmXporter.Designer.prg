
PARTIAL CLASS frmXporter INHERIT System.Windows.Forms.Form

PROTECT oToolTip1 AS System.Windows.Forms.ToolTip
PROTECT olblTitle AS System.Windows.Forms.Label
PROTECT oFileButton AS System.Windows.Forms.Button
PROTECT oDescription AS System.Windows.Forms.TextBox
PROTECT otbFileName AS System.Windows.Forms.TextBox
PROTECT olblFileName AS System.Windows.Forms.Label
PROTECT ogrpExport AS System.Windows.Forms.GroupBox
PROTECT orbProject AS System.Windows.Forms.RadioButton
PROTECT orbSolution AS System.Windows.Forms.RadioButton
PROTECT oOKButton AS System.Windows.Forms.Button
PROTECT oCancelButton AS System.Windows.Forms.Button
// User code starts here (DO NOT remove this line)  ##USER##
METHOD InitializeForm() AS VOID

// IDE generated code (please DO NOT modify)

	LOCAL oResourceManager AS System.Resources.ResourceManager

	oResourceManager := System.Resources.ResourceManager{ "Designers" , System.Reflection.Assembly.GetExecutingAssembly() }

	SELF:oToolTip1 := System.Windows.Forms.ToolTip{}
	SELF:olblTitle := System.Windows.Forms.Label{}
	SELF:oFileButton := System.Windows.Forms.Button{}
	SELF:oDescription := System.Windows.Forms.TextBox{}
	SELF:otbFileName := System.Windows.Forms.TextBox{}
	SELF:olblFileName := System.Windows.Forms.Label{}
	SELF:ogrpExport := System.Windows.Forms.GroupBox{}
	SELF:orbProject := System.Windows.Forms.RadioButton{}
	SELF:orbSolution := System.Windows.Forms.RadioButton{}
	SELF:oOKButton := System.Windows.Forms.Button{}
	SELF:oCancelButton := System.Windows.Forms.Button{}


	SELF:SuspendLayout()

	SELF:BackColor := System.Drawing.SystemColors.InactiveCaption
	SELF:BackgroundImageLayout := System.Windows.Forms.ImageLayout.Stretch
	SELF:ClientSize := System.Drawing.Size{464 , 296}
	SELF:Icon := (System.Drawing.Icon)oResourceManager:GetObject( "favicon.ico" )
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:MaximizeBox := False
	SELF:MinimizeBox := False
	SELF:Name := "frmXporter"
	SELF:Shown += SELF:BasicFormShown
	SELF:StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen
	SELF:Text := "XSharp XPorter"

	SELF:AcceptButton := SELF:oOKButton
	SELF:CancelButton := SELF:oCancelButton
	SELF:olblTitle:Location := System.Drawing.Point{8 , 8}
	SELF:olblTitle:Name := "lblTitle"
	SELF:olblTitle:Size := System.Drawing.Size{448 , 23}
	SELF:olblTitle:TabIndex := 7
	SELF:olblTitle:Text := "This program helps you to convert your Vulcan.NET projects and solutions to X#"
	SELF:Controls:Add(SELF:olblTitle)
	
	SELF:oFileButton:Click += SELF:FileButtonClick
	SELF:oFileButton:Location := System.Drawing.Point{432 , 144}
	SELF:oFileButton:Name := "FileButton"
	SELF:oFileButton:Size := System.Drawing.Size{24 , 22}
	SELF:oFileButton:TabIndex := 6
	SELF:oFileButton:Text := "..."
	SELF:oToolTip1:SetToolTip(SELF:oFileButton ,"Click here to choose the source file with a standard Windows File Dialog")
	SELF:Controls:Add(SELF:oFileButton)
	
	SELF:oDescription:BackColor := System.Drawing.SystemColors.InactiveCaption
	SELF:oDescription:BorderStyle := System.Windows.Forms.BorderStyle.None
	SELF:oDescription:Location := System.Drawing.Point{8 , 184}
	SELF:oDescription:Margin := System.Windows.forms.Padding{5 , 5 , 5 , 5}
	SELF:oDescription:Multiline := True
	SELF:oDescription:Name := "Description"
	SELF:oDescription:ReadOnly := True
	SELF:oDescription:Size := System.Drawing.Size{344 , 104}
	SELF:oDescription:TabIndex := 5
	SELF:Controls:Add(SELF:oDescription)
	
	SELF:otbFileName:Location := System.Drawing.Point{80 , 144}
	SELF:otbFileName:Name := "tbFileName"
	SELF:otbFileName:Size := System.Drawing.Size{344 , 20}
	SELF:otbFileName:TabIndex := 4
	SELF:otbFileName:TextChanged += SELF:tbFileNameTextChanged
	SELF:oToolTip1:SetToolTip(SELF:otbFileName ,"Enter the source file name")
	SELF:Controls:Add(SELF:otbFileName)
	
	SELF:olblFileName:Location := System.Drawing.Point{8 , 144}
	SELF:olblFileName:Name := "lblFileName"
	SELF:olblFileName:Size := System.Drawing.Size{64 , 23}
	SELF:olblFileName:TabIndex := 3
	SELF:olblFileName:Text := "File Name:"
	SELF:Controls:Add(SELF:olblFileName)
	
	SELF:ogrpExport:SuspendLayout()
	SELF:ogrpExport:BackColor := System.Drawing.SystemColors.InactiveCaption
	SELF:ogrpExport:FlatStyle := System.Windows.Forms.FlatStyle.Standard
	SELF:ogrpExport:Location := System.Drawing.Point{8 , 32}
	SELF:ogrpExport:Name := "grpExport"
	SELF:ogrpExport:Size := System.Drawing.Size{448 , 96}
	SELF:ogrpExport:TabIndex := 2
	SELF:ogrpExport:Text := "What to convert"
	SELF:Controls:Add(SELF:ogrpExport)
	

	SELF:orbProject:Click += SELF:rbProjectClick
	SELF:orbProject:Location := System.Drawing.Point{16 , 56}
	SELF:orbProject:Name := "rbProject"
	SELF:orbProject:Size := System.Drawing.Size{320 , 24}
	SELF:orbProject:TabIndex := 1
	SELF:orbProject:Text := "Single Vulcan.NET project"
	SELF:oToolTip1:SetToolTip(SELF:orbProject ,"Click this button to convert a single Vulcan.NET project")
	SELF:ogrpExport:Controls:Add(SELF:orbProject)
	
	SELF:orbSolution:Checked := True
	SELF:orbSolution:Click += SELF:rbSolutionClick
	SELF:orbSolution:Location := System.Drawing.Point{16 , 24}
	SELF:orbSolution:Name := "rbSolution"
	SELF:orbSolution:Size := System.Drawing.Size{320 , 24}
	SELF:orbSolution:TabIndex := 0
	SELF:orbSolution:Text := "Visual Studio Solution with Vulcan.NET projects"
	SELF:oToolTip1:SetToolTip(SELF:orbSolution ,"Click this button to convert a Visual Studio Solution")
	SELF:ogrpExport:Controls:Add(SELF:orbSolution)
	
	SELF:oOKButton:Click += SELF:OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{360 , 224}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{96 , 24}
	SELF:oOKButton:TabIndex := 1
	SELF:oOKButton:Text := "E&xport"
	SELF:oToolTip1:SetToolTip(SELF:oOKButton ,"Click to start the conversion process")
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:oCancelButton:Click += SELF:CancelButtonClick
	SELF:oCancelButton:Location := System.Drawing.Point{360 , 264}
	SELF:oCancelButton:Name := "CancelButton"
	SELF:oCancelButton:Size := System.Drawing.Size{96 , 24}
	SELF:oCancelButton:TabIndex := 0
	SELF:oCancelButton:Text := "&Close"
	SELF:oToolTip1:SetToolTip(SELF:oCancelButton ,"Click to close the window")
	SELF:Controls:Add(SELF:oCancelButton)
	
	SELF:ogrpExport:ResumeLayout()
	SELF:ResumeLayout()

RETURN
END CLASS
