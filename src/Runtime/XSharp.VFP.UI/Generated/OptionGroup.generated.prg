
// Class OptionGroup  BaseClass   Optiongroup  Class  Optiongroup
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS OptionGroup  IMPLEMENTS IVFPControl, IVFPOwner
#include "VFPControl.xh"
#include "VFPContainer.xh"

		PROPERTY BORDERCOLOR AS LONG AUTO
		METHOD SetFocus() AS VOID STRICT
			SELF:Focus()



	END CLASS
	END NAMESPACE      