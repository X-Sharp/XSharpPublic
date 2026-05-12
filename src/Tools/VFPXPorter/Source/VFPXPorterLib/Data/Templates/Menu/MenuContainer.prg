USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Data
USING System.Drawing
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING System.Windows.Forms

USING XSharp.VFP.UI

/// <summary>
/// The <@MenuName@> class.
/// </summary>
PUBLIC CLASS <@MenuName@>
	
	<@MenuDeclaration@>
	
	PROTECT MenuStrips AS List<System.Windows.Forms.MenuStrip>
	
	PUBLIC CONSTRUCTOR() STRICT
		<@MenuInit@>
		
	RETURN
	
	PROPERTY ThisForm AS Form GET SELF:Owner 
	PROTECTED Owner AS Form
	
	PUBLIC METHOD DoWith( args PARAMS USUAL[] ) AS VOID
		// The first parameter (if any), must be the Owner (the parent window)
		IF args:Length > 0
			IF args[1] IS System.Windows.Forms.Form
				SELF:Owner := (System.Windows.Forms.Form)args[1]
			ENDIF
		ENDIF
	RETURN
	
	PUBLIC METHOD MrkBar( cMenuName AS STRING, menuItemID := NULL AS OBJECT ) AS LOGIC
	RETURN FALSE
	
	PUBLIC METHOD PrmBar( cMenuName AS STRING, menuItemID := NULL AS OBJECT ) AS STRING
		RETURN ""
		
	<@MenuCode@>
		
END CLASS
	
	
/// <summary>
/// The <@MenuName@>.MPR class.
/// This class INHERIT from the <@MenuName@>
/// </summary>
PUBLIC CLASS <@MenuName@>.MPR INHERIT <@MenuName@>


END CLASS
