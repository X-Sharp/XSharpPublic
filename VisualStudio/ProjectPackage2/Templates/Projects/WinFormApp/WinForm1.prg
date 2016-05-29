USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text
$if$ ($targetframeworkversion$ >= 4.5)USING System.Threading.Tasks
$endif$USING System.Windows.Forms

BEGIN NAMESPACE $safeprojectname$
    
    
    PUBLIC PARTIAL CLASS $safeitemrootname$ INHERIT System.Windows.Forms.Form
        
        PRIVATE components AS System.ComponentModel.IContainer
        
        PUBLIC CONSTRUCTOR() 
            InitializeComponent()
			return

        
        PROTECTED METHOD Dispose(disposing AS logic) AS void
            if (disposing .AND. (components != null))
                components:Dispose()
            endif
            Super:Dispose(disposing)
			return


        
        PRIVATE METHOD InitializeComponent() AS void
            SELF:SuspendLayout()
            // 
            // WinForm1
            // 
            SELF:ClientSize := System.Drawing.Size{379, 316}
            SELF:Name := e"$safeitemrootname$"
            SELF:Text := e"$safeitemrootname$"
            SELF:ResumeLayout(false)
    
    END CLASS
END NAMESPACE


