USING System
USING System.Windows.Forms
USING WinFormsTestBed

[STAThread] ;
FUNCTION Start() AS VOID
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(FALSE)
    Application.Run(Form1{})
    RETURN
