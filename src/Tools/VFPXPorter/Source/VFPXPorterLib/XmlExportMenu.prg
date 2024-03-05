// XmlExport.prg
// Created by    : fabri
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
	[XmlRoot("MnxMenu")];
	CLASS XmlExportMenu
	
		[XmlArray("Items")];
		[XmlArrayItem("Item")];
		PROPERTY Items AS List<MNXItem> AUTO
		
		CONSTRUCTOR( itemList AS List<MNXItem> )
			SELF:Items := itemList
			RETURN
			
		CONSTRUCTOR( )
			RETURN
			
	END CLASS
END NAMESPACE // FabVFPXPorter