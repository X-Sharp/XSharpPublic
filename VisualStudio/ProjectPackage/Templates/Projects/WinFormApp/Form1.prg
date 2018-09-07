USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$
USING System.Text
$if$ ($targetframeworkversion$ >= 4.5)using System.Threading.Tasks
$endif$
USING System.Windows.Forms

BEGIN NAMESPACE $safeprojectname$

    PUBLIC PARTIAL CLASS Form1 INHERIT System.Windows.Forms.Form
    
        PUBLIC CONSTRUCTOR()   STRICT//$safeitemrootname$
            InitializeComponent()
			RETURN
        
    END CLASS
END NAMESPACE
