
// Class Spinner BaseClass   Spinner Class  Spinner
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Spinner  IMPLEMENTS IVFPControl, IVFPText, IVFPEditable
        #include "VFPControl.xh"

        PROPERTY BackStyle AS INT AUTO
        PROPERTY BorderColor AS System.Drawing.Color AUTO
        PROPERTY DisabledBackColor AS System.Drawing.Color AUTO
        PROPERTY DisabledForeColor AS System.Drawing.Color AUTO
        PROPERTY FontOutline AS LOGIC AUTO
        PROPERTY FontShadow  AS LOGIC AUTO
		PROPERTY NullDisplay AS String AUTO
        PROPERTY OLEDropTextInsertion AS LONG AUTO
        PROPERTY SelectedBackColor AS System.Drawing.Color AUTO
        PROPERTY SelectedForeColor AS System.Drawing.Color AUTO
		PROPERTY SelectOnEntry AS LOGIC AUTO
	END CLASS
	END NAMESPACE      
