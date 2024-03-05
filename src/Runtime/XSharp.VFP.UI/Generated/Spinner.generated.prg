
// Class Spinner BaseClass   Spinner Class  Spinner
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Spinner  IMPLEMENTS IVFPControl, IVFPText, IVFPEditable
#include "VFPControl.xh"
		PROPERTY BorderColor AS LONG AUTO
		PROPERTY DisabledBackColor AS LONG AUTO
		PROPERTY DisabledForeColor AS LONG AUTO
		PROPERTY Format AS STRING AUTO
		PROPERTY InputMask AS STRING AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY OLEDropTextInsertion AS LONG AUTO
		PROPERTY SelectedBackColor AS LONG AUTO
		PROPERTY SelectedForeColor AS LONG AUTO
		PROPERTY SelectOnEntry AS LOGIC AUTO
			//SELF:Focus()


	END CLASS
	END NAMESPACE      