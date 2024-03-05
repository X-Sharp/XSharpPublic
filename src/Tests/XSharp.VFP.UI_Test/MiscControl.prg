USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING System.Windows.Forms
USING XSharp.VFP.UI

BEGIN NAMESPACE XSharp.VFP.UI_Test

    PUBLIC PARTIAL CLASS MiscControl	;
		INHERIT Form

        PUBLIC CONSTRUCTOR()   STRICT//Form1
            USE Customer
            //
            InitializeComponent()
            //
            RETURN

        PRIVATE METHOD vfpCommandGroup1_Load(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
            this.vfpGrid1.AddProperty( "curid",0)
			RETURN

    END CLASS
END NAMESPACE
