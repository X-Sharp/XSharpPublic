USING System
USING System.Windows.Forms
BEGIN NAMESPACE XSharp.VFP

    PUBLIC PARTIAL CLASS AssertDialog ;
        INHERIT System.Windows.Forms.Form
                    PROPERTY Result as AssertResult AUTO






    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    public constructor() strict
        SELF:InitializeComponent()
        SELF:cmdDebug:Enabled := System.Diagnostics.Debugger.IsAttached
        SELF:Result := AssertResult.None
        RETURN

    PRIVATE METHOD cmdDebug_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
        SELF:Result := AssertResult.Debug
        SELF:Close()
        RETURN
    PRIVATE METHOD cmdCancel_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
        SELF:Result := AssertResult.Cancel
        SELF:Close()
        RETURN
    PRIVATE METHOD cmdIgnore_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
        SELF:Result := AssertResult.Ignore
        SELF:Close()
        RETURN
    PRIVATE METHOD cmdIgnoreAll_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
        SELF:Result := AssertResult.IgnoreAll
        SELF:Close()
        RETURN

    PROPERTY Message as STRING GET lblMessage:Text SET lblMessage:Text := value


END CLASS
END NAMESPACE
