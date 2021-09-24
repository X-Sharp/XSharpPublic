USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing

USING System.Text

USING System.Windows.Forms

BEGIN NAMESPACE UDCTesterApp

    PUBLIC PARTIAL CLASS Settings ;
        INHERIT System.Windows.Forms.Form

        PUBLIC CONSTRUCTOR() STRICT 
            InitializeComponent()
            SELF:tbOutputPath:Text      := oSettings:OutputPath
            SELF:chkWriteToPPO:Checked  := oSettings:WritePPO
            SELF:chkLexerTokens:Checked := oSettings:WriteLexTokens
            SELF:chkPreprocessorTokens:Checked := oSettings:WritePPTokens
            SELF:chkNoStandardDefs:Checked    := oSettings:DefaultNoStdDefs
            SELF:chkHideComments:Checked    := oSettings:HideComments
            SELF:chkHideWhitespace:Checked    := oSettings:HideWhitespace
            SELF:comboDialect:Text := oSettings:DefaultDialect
            RETURN
        PRIVATE METHOD btnOk_Click(sender AS System.Object, e AS System.EventArgs) AS VOID STRICT
            oSettings:OutputPath := SELF:tbOutputPath:Text      
            oSettings:WritePPO := SELF:chkWriteToPPO:Checked  
            oSettings:WriteLexTokens := SELF:chkLexerTokens:Checked 
            oSettings:WritePPTokens := SELF:chkPreprocessorTokens:Checked  
            oSettings:DefaultNoStdDefs := SELF:chkNoStandardDefs:Checked
            oSettings:HideComments := SELF:chkHideComments:Checked
            oSettings:HideWhitespace := SELF:chkHideWhitespace:Checked    
            oSettings:DefaultDialect := SELF:comboDialect:Text
            oSettings:WriteSettings()
            SELF:DialogResult := DialogResult.OK
            SELF:Close()
        
            RETURN


    END CLASS 
END NAMESPACE
