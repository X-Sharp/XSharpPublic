// ProjectItem.prg
// Created by    : fabri
// Creation Date : 11/13/2019 3:36:08 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
		/// The ProjectItem class.
	/// </summary>
	CLASS ProjectItem
	
		PROPERTY Owner AS VFPProject AUTO
		
		PROPERTY Name AS STRING AUTO
		
		
		CONSTRUCTOR( own AS VFPProject, nme AS STRING )
			SELF:Owner := own
			SELF:Name := nme
			RETURN
			
	END CLASS
END NAMESPACE // global::FabVFPXPorter.VFP