USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text

USING System.Windows.Forms

BEGIN NAMESPACE XSharp.Debugger

    PUBLIC PARTIAL CLASS VariablesWindow ;
        INHERIT System.Windows.Forms.Form

        INTERNAL CONSTRUCTOR() STRICT 
            SELF:InitializeComponent()
            SELF:LoadValues()
         RETURN
        VIRTUAL PROTECTED METHOD LoadValues() AS VOID
            RETURN
        PRIVATE METHOD btnClose_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
            SELF:Close()
            RETURN
    END CLASS 
END NAMESPACE
