USING System.Collections
USING System.Collections.Generic
USING System.Windows.Forms
USING System.IO
USING Xide
USING XSharpModel
using XSharp.VODesigners

BEGIN NAMESPACE XSharp.VOEditors

CLASS XSharpDesignWindowItem inherit DesignWindowItem

	CONSTRUCTOR( oDesigner as VOWindowEditor, oTemplate as VOControlTemplate)
		SUPER(oDesigner , oTemplate)
		

	METHOD GetVOStylesString(eVOStyle as VOStyle ) as STRING
		LOCAL cResult := SUPER:GetVOStylesString(eVOStyle) AS STRING
		LOCAL oXSharpDesigner as XSharp_VOWindowEditor
		oXSharpDesigner := SELF:oDesigner astype XSharp_VOWindowEditor
		IF !String.IsNullOrEmpty(cResult) .and. oXSharpDesigner != NULL_OBJECT
			LOCAL aStyles := cResult:Split("|":ToCharArray()) AS STRING[]
			oXSharpDesigner:AddStyles(aStyles)
			oXSharpDesigner:AddStyles(<STRING>{"WS_VISIBLE"})
		ENDIF
		return cResult

END CLASS
END NAMESPACE