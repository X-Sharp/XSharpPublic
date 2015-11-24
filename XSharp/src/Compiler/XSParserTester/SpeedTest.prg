#using System.Windows.Forms
#using System.Drawing

FUNCTION Start() AS VOID
LOCAL o AS BasicForm1
o := BasicForm1{}
o:ShowDialog()
RETURN

CLASS BasicForm1 INHERIT System.Windows.Forms.Form

EXPORT oListBox1 AS System.Windows.Forms.ListBox
EXPORT oOKButton AS System.Windows.Forms.Button
// User code starts here (DO NOT remove this line)  ##USER##

CONSTRUCTOR()

	SUPER()
	SELF:InitializeForm()

RETURN

METHOD InitializeForm() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN


METHOD InitializeForm9() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm8() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm7() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm6() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm5() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm4() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm3() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm2() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD InitializeForm1() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()
	SELF:oListBox1 := System.Windows.Forms.ListBox{}
	SELF:oOKButton := System.Windows.Forms.Button{}

	SELF:SuspendLayout()

	SELF:ClientSize := System.Drawing.Size{392 , 264}
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:Name := "BasicForm"
	SELF:Text := "Basic form application"

	SELF:oListBox1:Location := System.Drawing.Point{88 , 72}
	SELF:oListBox1:Name := "ListBox1"
	SELF:oListBox1:Size := System.Drawing.Size{166 , 100}
	SELF:oListBox1:TabIndex := 1
	SELF:Controls:Add(SELF:oListBox1)
	
	SELF:oOKButton:Click += OKButtonClick
	SELF:oOKButton:Location := System.Drawing.Point{280 , 232}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{104 , 24}
	SELF:oOKButton:TabIndex := 0
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:ResumeLayout()

RETURN

METHOD OKButtonClick(sender AS OBJECT , e AS System.EventArgs) AS VOID

	SELF:Close()

RETURN


END CLASS

