// XmlExportReport.prg
// Created by    : fabri
// Creation Date : 2/11/2021 10:38:36 PM
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
	[XmlRoot("FrxReport")];
	CLASS XmlExportReport
	
		[XmlArray("Items")];
		[XmlArrayItem("Item")];
		PROPERTY Items AS List<FRXItem> AUTO
		
		CONSTRUCTOR( itemList AS List<FRXItem> )
			SELF:Items := itemList
			RETURN
			
		CONSTRUCTOR( )
			RETURN
			
	END CLASS
END NAMESPACE // FabVFPXPorter