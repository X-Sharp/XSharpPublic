USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text
USING System.Windows.Forms

BEGIN NAMESPACE $safeprojectname$
   PARTIAL CLASS Form1 INHERIT System.Windows.Forms.Form
       
      CONSTRUCTOR()
         SUPER()
         SELF:InitializeComponent()
         RETURN
        
   END CLASS
END NAMESPACE
