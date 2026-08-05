USING System
USING XSharp.VFP.UI

/// <summary>
/// The <@MenuName@> class.
/// </summary>
CLASS <@MenuName@> INHERIT XSharp.VFP.UI.Menu

	OVERRIDE METHOD Init() AS USUAL CLIPPER
		LOCAL oPad   AS XSharp.VFP.UI.Pad
		LOCAL oPopup AS XSharp.VFP.UI.Popup

		<@MenuInit@>
		//SELF:Activate( _Screen )
		RETURN SUPER:Init()

	<@MenuCode@>

END CLASS


/// <summary>
/// The <@MenuName@>.MPR class.
/// This class INHERIT from the <@MenuName@>
/// </summary>
CLASS <@MenuName@>.MPR INHERIT <@MenuName@>


END CLASS
