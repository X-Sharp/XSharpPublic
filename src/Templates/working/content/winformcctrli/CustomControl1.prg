USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Linq
USING System.Text
using System.Threading.Tasks
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.WinFormsCCtrlI

    PUBLIC PARTIAL CLASS CustomControl1 INHERIT System.Windows.Forms.UserControl

        PUBLIC CONSTRUCTOR()   STRICT
            SELF:InitializeComponent()
            RETURN
        END CONSTRUCTOR

        
        PROTECTED OVERRIDE METHOD OnPaint(pe AS System.Windows.Forms.PaintEventArgs) AS VOID  STRICT
            SUPER:OnPaint(pe)
            RETURN 
        END METHOD

    END CLASS
END NAMESPACE
