USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text

USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI_Test

    PUBLIC PARTIAL CLASS TestVFPTextBox	;
		INHERIT System.Windows.Forms.Form

        public constructor() strict
            InitializeComponent()
			SELF:vfpTextBox2:Value := __Usual{0}
			SELF:vfpTextBox2:InputMask := "99,999.99"
			
            return
        end constructor
PRIVATE METHOD TestVFPTextBox_Load(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			RETURN
		END METHOD
PRIVATE METHOD vfpTextBox1_Leave(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
	SELF:vfpTextBox2:InputMask := SELF:vfpTextBox1:Text
			RETURN
		END METHOD
    END CLASS 
END NAMESPACE
