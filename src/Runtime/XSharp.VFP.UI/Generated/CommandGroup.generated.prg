

BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS CommandGroup IMPLEMENTS IVFPControl, IVFPOwner, IVFPGroup
#define VFP_CONTROLCOUNT_OVERRIDE
#include "VFPControl.xh"
#include "VFPContainer.xh"
#include "VFPPropertiesDynamic.xh"
#include "VFPGroup.xh"
#include "FontProperties.xh"

		// BorderColor, BackStyle, ControlCount, Resize/Moved, ProgrammaticChange — implemented in CommandGroup.prg
		// SetFocus() provided by ControlProperties.xh in CommandGroup.prg

		PROPERTY FontOutline AS LOGIC AUTO
		PROPERTY FontShadow  AS LOGIC AUTO

	END CLASS
	END NAMESPACE
