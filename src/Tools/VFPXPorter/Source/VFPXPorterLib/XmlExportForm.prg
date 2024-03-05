// XmlExport.prg
// Created by    : fabri
// Creation Date : 11/8/2019 8:12:19 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml.Serialization

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
		/// The XmlExport class.
	/// </summary>
	[XmlRoot("ScxForm")];
	CLASS XmlExportForm
	
		[XmlArray("Items")];
		[XmlArrayItem("Item")];
		PROPERTY Items AS List<BaseItem> AUTO
		
		CONSTRUCTOR( itemList AS List<BaseItem> )
			SELF:Items := itemList
			RETURN
			
		CONSTRUCTOR( )
			RETURN
			
	END CLASS
END NAMESPACE // FabVFPXPorter