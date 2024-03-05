// XmlExportProject.prg
// Created by    : fabri
// Creation Date : 11/14/2019 8:43:35 AM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml.Serialization

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
		/// The XmlExportProject class.
	/// </summary>
	[XmlRoot("PjxProject")];
	CLASS XmlExportProject
	
		[XmlArray("Items")];
		[XmlArrayItem("Item")];
		PROPERTY Items AS List<PJXItem> AUTO
		
		CONSTRUCTOR( itemList AS List<PJXItem> )
			SELF:Items := itemList
			RETURN
			
		CONSTRUCTOR()
			RETURN
			
	END CLASS
END NAMESPACE // FabVFPXPorter