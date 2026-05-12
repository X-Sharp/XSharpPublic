
// Class Page  BaseClass   Page  Class  Page
BEGIN NAMESPACE XSharp.VFP.UI
	PARTIAL CLASS Page  IMPLEMENTS IVFPControl, IVFPOwner, IVFPGraphics
#include "VFPControl.xh"
		#include "VFPContainer.xh"
		#include "VFPPropertiesDynamic.xh"

		// PageOrder, Picture, BackStyle, Activate/Deactivate — implemented in Page.prg
		PROPERTY FontOutline AS LOGIC AUTO
		PROPERTY FontShadow  AS LOGIC AUTO

	END CLASS
	END NAMESPACE
