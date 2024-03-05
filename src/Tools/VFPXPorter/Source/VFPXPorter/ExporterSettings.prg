// ExporterSettings.prg
// Created by    : fabri
// Creation Date : 7/18/2022 4:05:50 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Windows.Forms
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorter

	/// <summary>
	/// The ExporterSettings class.
	/// </summary>
	CLASS ExporterSettings
		PRIVATE fileName AS STRING
		PRIVATE iniSettings AS FabIniFileAsJSON

		CONSTRUCTOR( fullPath AS STRING )
			SELF:fileName := fullPath
			SELF:iniSettings := FabIniFileAsJSON{ SELF:fileName }
			RETURN

		PROPERTY FullPath AS STRING GET SELF:fileName

		METHOD Reset() AS VOID
			SELF:iniSettings:Reset()
		END METHOD

		PROPERTY Warning AS LOGIC GET SELF:iniSettings:ReadValue( "Startup", "Warning", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Startup", "Warning", VALUE )

		PROPERTY Output AS STRING GET SELF:iniSettings:ReadValue( "Folders", "Output" ) ;
			SET SELF:iniSettings:WriteValue( "Folders", "Output", VALUE )

		PROPERTY Items AS STRING GET SELF:iniSettings:ReadValue( "Folders", "Items" ) ;
			SET SELF:iniSettings:WriteValue( "Folders", "Items", VALUE )

		PROPERTY ConvertHandlers AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "ConvertHandlers", FALSE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "ConvertHandlers", VALUE )
		PROPERTY ConvertUserDef AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "ConvertUserDef", FALSE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "ConvertUserDef", VALUE )
		PROPERTY ConvertThisObject AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "ConvertThisObject", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "ConvertThisObject", VALUE )
		PROPERTY ConvertStatement AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "ConvertStatement", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "ConvertStatement", VALUE )
		PROPERTY ConvertStatementOnlyIfLast AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "ConvertStatementOnlyIfLast", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "ConvertStatementOnlyIfLast", VALUE )
		PROPERTY KeepOriginal AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "KeepOriginal", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "KeepOriginal", VALUE )
		PROPERTY NameUDF AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "NameUDF", FALSE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "NameUDF", VALUE )
		PROPERTY RemoveSet AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "RemoveSet", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "RemoveSet", VALUE )
		PROPERTY PrefixEvent AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "PrefixEvent", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "PrefixEvent", VALUE )
		PROPERTY KeepFoxProEventName AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "KeepFoxProEventName", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "KeepFoxProEventName", VALUE )
		PROPERTY GenerateOnlyHandledEvent AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "GenerateOnlyHandledEvent", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Items", "GenerateOnlyHandledEvent", VALUE )

		PROPERTY PrefixClassFile AS LOGIC GET SELF:iniSettings:ReadValue( "Items", "PrefixClassFile", false ) ;
			SET SELF:iniSettings:WriteValue( "Items", "PrefixClassFile", VALUE )

		PROPERTY Modifier AS STRING GET SELF:iniSettings:ReadValue( "Items", "Modifier", "PUBLIC" ) ;
			SET SELF:iniSettings:WriteValue( "Items", "Modifier", VALUE )

		PROPERTY ItemsType AS STRING GET SELF:iniSettings:ReadValue( "Folders", "ItemsType", self:DefaultFolders ) ;
			SET SELF:iniSettings:WriteValue( "Folders", "ItemsType", VALUE )

		PROPERTY DefaultFolders AS STRING GET XPorterSettings.DefaultFolders

		PROPERTY LibInSubFolder AS LOGIC GET SELF:iniSettings:ReadValue( "Project", "LibInSubFolder", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Project", "LibInSubFolder", VALUE )
		PROPERTY IgnoreErrors AS LOGIC GET SELF:iniSettings:ReadValue( "Project", "IgnoreErrors", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Project", "IgnoreErrors", VALUE )
		PROPERTY StoreInFolders AS LOGIC GET SELF:iniSettings:ReadValue( "Project", "StoreInFolders", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Project", "StoreInFolders", VALUE )
		PROPERTY EmptyFolder AS LOGIC GET SELF:iniSettings:ReadValue( "Project", "EmptyFolder", TRUE ) ;
			SET SELF:iniSettings:WriteValue( "Project", "EmptyFolder", VALUE )

		PROPERTY RessourcesFolder AS STRING GET SELF:iniSettings:ReadValue( "Folders", "Ressources", Path.Combine(Path.GetDirectoryName( Application.ExecutablePath ),XPorterSettings.DataFolder) ) ;
			SET SELF:iniSettings:WriteValue( "Folders", "Ressources", VALUE )

		METHOD ToXPorterSettings() AS XPorterSettings
			LOCAL settings AS XPorterSettings
			//
			settings := XPorterSettings{}
			settings:ConvertHandlers := SELF:ConvertHandlers
			settings:ConvertStatement := SELF:ConvertStatement
			settings:ConvertStatementOnlyIfLast := SELF:ConvertStatementOnlyIfLast
			settings:ConvertThisObject := SELF:ConvertThisObject
			settings:ConvertUserDef	:= SELF:ConvertUserDef
			settings:EmptyFolder	:= SELF:EmptyFolder
			settings:IgnoreErrors	:= SELF:IgnoreErrors
			settings:ItemsPath		:= SELF:Items
			settings:ItemsType		:= SELF:ItemsType
			settings:KeepOriginal	:= SELF:KeepOriginal
			settings:LibInSubFolder	:= SELF:LibInSubFolder
			settings:Modifier		:= SELF:Modifier
			settings:NameUDF		:= SELF:NameUDF
			settings:OutputPath		:= SELF:Output
			settings:PrefixClassFile:= SELF:PrefixClassFile
			settings:RemoveSet		:= SELF:RemoveSet
			settings:StoreInFolders	:= SELF:StoreInFolders
			settings:PrefixEvent	:= SELF:PrefixEvent
			settings:KeepFoxProEventName := SELF:KeepFoxProEventName
			settings:GenerateOnlyHandledEvent := SELF:GenerateOnlyHandledEvent
			//
			XPorterSettings.DataFolder := SELF:RessourcesFolder
			//
			RETURN settings

	END CLASS
END NAMESPACE // VFPXPorter