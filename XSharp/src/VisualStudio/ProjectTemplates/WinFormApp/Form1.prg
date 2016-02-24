#using System
#using System.Collections.Generic
#using System.ComponentModel
#using System.Data
#using System.Drawing
$if$ ($targetframeworkversion$ >= 3.5)#using System.Linq
$endif$#using System.Text
#using System.Windows.Forms

BEGIN NAMESPACE $safeprojectname$
   PARTIAL CLASS Form1 INHERIT System.Windows.Forms.Form
       
      CONSTRUCTOR()
         SUPER()
         SELF:InitializeComponent()
         RETURN
        
   END CLASS
END NAMESPACE
