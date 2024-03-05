// VFPProject.prg
// Created by    : fabri
// Creation Date : 11/13/2019 3:35:56 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE VFPXPorterLib

	/// <summary>
	/// The VFPProject class.
	/// </summary>
	CLASS VFPProject
		
		PROPERTY Name AS STRING  AUTO
		
		PROPERTY HomeDir AS STRING AUTO
		
		PROPERTY Forms AS List<ProjectItem> AUTO
		PROPERTY Libraries AS List<ProjectItem> AUTO
		PROPERTY Menus AS List<ProjectItem> AUTO
		PROPERTY Programs AS List<ProjectItem> AUTO
		PROPERTY Reports AS List<ProjectItem> AUTO
		PROPERTY Databases AS List<ProjectItem> AUTO
		PROPERTY FreeTables AS List<ProjectItem> AUTO
		PROPERTY Others AS List<ProjectItem> AUTO
		
		PROPERTY Main AS ProjectItem AUTO
		
		CONSTRUCTOR()
			SELF:Forms := List<ProjectItem>{}
			SELF:Libraries := List<ProjectItem>{}
			SELF:Menus := List<ProjectItem>{}
			SELF:Programs := List<ProjectItem>{}
			SELF:Reports := List<ProjectItem>{}
			SELF:Databases := List<ProjectItem>{}
			SELF:FreeTables := List<ProjectItem>{}
			SELF:Others := List<ProjectItem>{}
			RETURN
			
			END CLASS
END NAMESPACE // global::FabVFPXPorter.VFP