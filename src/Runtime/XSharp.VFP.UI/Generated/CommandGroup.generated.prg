

BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS CommandGroup IMPLEMENTS IVFPControl, IVFPOwner, IVFPGroup
#define VFP_CONTROLCOUNT_OVERRIDE
#include "VFPControl.xh"
#include "VFPContainer.xh"
#include "VFPPropertiesDynamic.xh"
#include "VFPGroup.xh"

		// BorderColor, BackStyle, ControlCount, Resize/Moved, ProgrammaticChange — implemented in CommandGroup.prg
		// SetFocus() provided by ControlProperties.xh in CommandGroup.prg


	END CLASS
	END NAMESPACE      
