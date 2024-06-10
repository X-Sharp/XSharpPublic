USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text

USING System.Windows.Forms

#command DO FORM <FormName> [NAME <VarName> [LINKED]] [WITH] [ <(p1)> ]   [, <(pn)> ]  [TO <ToVarName>] [<noread:NOREAD>] [<noshow:NOSHOW>] ;
=> XSharp.VFP.UI.__VFPDoForm.InitParam();
[;XSharp.VFP.UI.__VFPDoForm.Param( <p1> )] ;
[;XSharp.VFP.UI.__VFPDoForm.Param( <pn> )] ;
;XSharp.VFP.UI.__VFPDoForm.Create( <(FormName)>, [<(VarName)>], [<(ToVarName)>], [<.noread.>], [<.NOSHOW.>])

BEGIN NAMESPACE XSharp.VFP.UI_Test
	
	PUBLIC PARTIAL CLASS MainWindow	;
		INHERIT XSharp.VFP.UI.MainWindow
		
        public constructor() strict
            SUPER()                
			InitializeComponent()
			return
		end constructor
		PRIVATE METHOD quitToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			SELF:Close()
			RETURN
		END METHOD
		PRIVATE METHOD textBoxToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			VAR dlg := TestVFPTextBox{}
			dlg:ShowDialog()
			RETURN
		END METHOD
		PRIVATE METHOD miscControlsToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			VAR dlg := MiscControl{}
			dlg:ShowDialog()
			RETURN
		END METHOD
PRIVATE METHOD dOFORMToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
		DO FORM TestVFPForm
			RETURN
		END METHOD
	END CLASS 
END NAMESPACE
