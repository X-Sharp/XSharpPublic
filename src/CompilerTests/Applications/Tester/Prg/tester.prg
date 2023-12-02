USING System.Collections.Generic
USING System.Security.Cryptography
USING System.Threading.Tasks
USING System.Windows.Forms

FUNCTION Start() AS VOID
	VAR main := MainForm{}
	Application.Run(main)
RETURN

CLASS MainForm INHERIT System.Windows.Forms.Form

PROTECT oButton2 AS System.Windows.Forms.Button
PROTECT oListView1 AS System.Windows.Forms.ListView
PROTECT oButton1 AS System.Windows.Forms.Button
PROTECT oTextBox1 AS System.Windows.Forms.TextBox
// User code starts here (DO NOT remove this line)  ##USER##
CONSTRUCTOR()

	SUPER()

	SELF:InitializeForm()

RETURN

METHOD InitializeForm() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oButton2 := System.Windows.Forms.Button{}
	SELF:oListView1 := System.Windows.Forms.ListView{}
	SELF:oButton1 := System.Windows.Forms.Button{}
	SELF:oTextBox1 := System.Windows.Forms.TextBox{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{904 , 552}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "MainForm"
	SELF:Text := "Form"

	SELF:oButton2:Click += System.EventHandler{ SELF , @Button2Click() }
	SELF:oButton2:Location := System.Drawing.Point{8 , 96}
	SELF:oButton2:Name := "Button2"
	SELF:oButton2:Size := System.Drawing.Size{128 , 24}
	SELF:oButton2:TabIndex := 3
	SELF:oButton2:Text := "Generate Async!"
	SELF:Controls:Add(SELF:oButton2)

	SELF:oListView1:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Right + System.Windows.Forms.AnchorStyles.Bottom
	SELF:oListView1:Location := System.Drawing.Point{144 , 8}
	SELF:oListView1:Name := "ListView1"
	SELF:oListView1:Size := System.Drawing.Size{752 , 536}
	SELF:oListView1:TabIndex := 2
	SELF:oListView1:View := System.Windows.Forms.View.List
	SELF:Controls:Add(SELF:oListView1)

	SELF:oButton1:Click += System.EventHandler{ SELF , @Button1Click() }
	SELF:oButton1:Location := System.Drawing.Point{8 , 72}
	SELF:oButton1:Name := "Button1"
	SELF:oButton1:Size := System.Drawing.Size{129 , 21}
	SELF:oButton1:TabIndex := 1
	SELF:oButton1:Text := "Generate!"
	SELF:Controls:Add(SELF:oButton1)

	SELF:oTextBox1:Location := System.Drawing.Point{8 , 48}
	SELF:oTextBox1:Name := "TextBox1"
	SELF:oTextBox1:Size := System.Drawing.Size{128 , 20}
	SELF:oTextBox1:TabIndex := 0
	SELF:oTextBox1:Text := "100000"
	SELF:Controls:Add(SELF:oTextBox1)

	SELF:ResumeLayout()

RETURN

METHOD Button1Click(sender AS System.Object , e AS System.EventArgs) AS VOID
	VAR number := System.Int32.Parse(SELF:oTextBox1:Text)

	VAR rng := RandomNumberGenerator.Create()
	FOR VAR i := 1 UPTO number
		VAR bytes := BYTE[]{16}
		rng.GetBytes(bytes)
		VAR data := BitConverter.ToString(bytes)
		SELF:oListView1:Items:Add(data)
	NEXT
RETURN


ASYNC METHOD Button2Click(sender AS System.Object , e AS System.EventArgs) AS VOID
	VAR number := System.Int32.Parse(SELF:oTextBox1:Text)

	FOREACH AWAIT VAR data IN GenerateNumbersAsync(number)
		SELF:oListView1:Items:Add(data)
	NEXT
RETURN


END CLASS

// In .NET framework we need the nuget packages:
// https://www.nuget.org/packages/Microsoft.Bcl.AsyncInterfaces/7.0.0
// https://www.nuget.org/packages/System.Threading.Tasks.Extensions/4.5.4
// https://www.nuget.org/packages/System.Runtime.CompilerServices.Unsafe/4.5.3
ASYNC FUNCTION GenerateNumbersAsync(number AS INT) AS IAsyncEnumerable<STRING>
	VAR rng := RandomNumberGenerator.Create()

	LOCAL FUNCTION GetRnadomData() AS STRING
		VAR bytes := BYTE[]{16}
		rng.GetBytes(bytes)
		RETURN BitConverter.ToString(bytes)
	END FUNCTION

	FOR VAR i := 1 UPTO number
		YIELD RETURN AWAIT Task.Run(GetRnadomData)
	NEXT



