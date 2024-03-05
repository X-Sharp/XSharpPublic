USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Text
USING System.Windows.Forms
BEGIN NAMESPACE VFPXPorter
	PUBLIC PARTIAL CLASS AboutDialog	;
		INHERIT System.Windows.Forms.Form
		PUBLIC CONSTRUCTOR() STRICT //AboutDialog
			InitializeComponent()
			RETURN
		PRIVATE METHOD AboutDialog_Load(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			LOCAL currentAsm AS FabAssemblyInfo
			//
			currentAsm := FabAssemblyInfo{ Application.ExecutablePath }
			SELF:labelInfo:Text += Environment.NewLine +  "Version : " + currentAsm:VersionMajor + "." + currentAsm:VersionMinor + "." + currentAsm:VersionBuild + "." + currentAsm:VersionRevision
			RETURN
		PRIVATE METHOD okButton_Click(sender AS OBJECT, e AS System.EventArgs) AS VOID STRICT
			SELF:Close()
			RETURN
			
	END CLASS 
END NAMESPACE
