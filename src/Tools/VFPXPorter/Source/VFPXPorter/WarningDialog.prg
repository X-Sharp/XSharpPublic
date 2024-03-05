USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text
USING System.IO
USING System.Windows.Forms

BEGIN NAMESPACE VFPXPorter

	PUBLIC PARTIAL CLASS WarningDialog	;
			INHERIT System.Windows.Forms.Form

		public constructor() strict
			InitializeComponent()
			return
		end constructor
		PRIVATE METHOD buttonAgreed_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			SELF:Close()
			RETURN
		END METHOD
		PRIVATE METHOD WarningDialog_Load(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			IF File.Exists( "Warning.rtf")
				SELF:rtfWarning:LoadFile("Warning.rtf")
			ELSE
				SELF:rtfWarning:Text := "In X#, we have made every effort to make the compiler as compatible with VFP as possible, "
				SELF:rtfWarning:Text += "in order to make it easier to port your existing VFP code to X#. "
				SELF:rtfWarning:Text += "Unfortunately, 100% compatibility is not possible to achieve, due to important design differences with the .Net framework, "
				SELF:rtfWarning:Text += "so, depending on the size and type of your application(s), a small or larger amount of changes need to be made to your code, "
				SELF:rtfWarning:Text += "in order to allow it to compile and execute properly in X#."
				SELF:buttonAgreed:Enabled := TRUE
				SELF:checkDontShow:Enabled := TRUE
			ENDIF
			RETURN
		END METHOD
		PRIVATE METHOD rtfWarning_VScroll(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
			VAR p := Point{ SELF:rtfWarning:Width, SELF:rtfWarning:Height }
			VAR CharIndex := SELF:rtfWarning:GetCharIndexFromPosition(p)

			if (CharIndex > SELF:rtfWarning:TextLength - 100)
				SELF:buttonAgreed:Enabled := TRUE
				SELF:checkDontShow:Enabled := TRUE
			ENDIF
			RETURN
		END METHOD
	END CLASS
END NAMESPACE
