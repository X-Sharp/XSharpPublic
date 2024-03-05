// XPorterSettings.prg
// Created by    : fabri
// Creation Date : 9/14/2022 9:32:47 AM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE VFPXPorterLib

/// <summary>
/// The XPorterSettings class.
/// </summary>
CLASS XPorterSettings

	/// <summary>
	/// Default Folders names (See StoreInFolders).
	/// "Forms=Forms;Libs=Libs;..."
	/// </summary>
	/// <value></value>
	PUBLIC STATIC PROPERTY DefaultFolders AS STRING
		GET
			RETURN "Forms=Forms;Libs=Libs;Menus=Menus;Code=Code;Databases=Databases;FreeTables=FreeTables;Others=Others"
		END GET
	END PROPERTY

#region Various Folders definitions
    PUBLIC STATIC DataFolder := ".\Data"	AS STRING

    /// <summary>
    /// Namespace of the library that contains VFP-support controls/classes/tools
    /// </summary>
    PUBLIC STATIC SuppportLib := "XSharp.VFP.UI" AS STRING

	PUBLIC STATIC PROPERTY TemplateFolder		AS STRING GET XPorterSettings.DataFolder + "\\Templates"
	PUBLIC STATIC PROPERTY ToolsFolder			AS STRING GET XPorterSettings.TemplateFolder + "\\Tools"
	PUBLIC STATIC PROPERTY OthersFolder			AS STRING GET XPorterSettings.TemplateFolder + "\\Others"
	PUBLIC STATIC PROPERTY MenuFolder			AS STRING GET XPorterSettings.TemplateFolder + "\\Menu"
	PUBLIC STATIC PROPERTY DesignerFolder		AS STRING GET XPorterSettings.TemplateFolder + "\\Designer"
	PUBLIC STATIC PROPERTY FormFolder			AS STRING GET XPorterSettings.TemplateFolder + "\\Form"
	PUBLIC STATIC PROPERTY SingleFolder			AS STRING GET XPorterSettings.TemplateFolder + "\\SingleFile"

	PUBLIC STATIC PROPERTY PropRulesFile		AS STRING GET XPorterSettings.DataFolder + "\\PropRules.json"
	PUBLIC STATIC PROPERTY EventRulesFile		AS STRING GET XPorterSettings.DataFolder + "\\EventRules.json"
	PUBLIC STATIC PROPERTY ConvertTableFile		AS STRING GET XPorterSettings.DataFolder + "\\TypeConvert.json"
	PUBLIC STATIC PROPERTY StatementsFile		AS STRING GET XPorterSettings.DataFolder + "\\Statements.json"
	PUBLIC STATIC PROPERTY VFPElementsFile		AS STRING GET XPorterSettings.DataFolder + "\\VFP2WinForms.json"

	PUBLIC STATIC PROPERTY StartFile				AS STRING GET XPorterSettings.OthersFolder + "\\VFPStart.prg"

	PUBLIC STATIC PROPERTY MenuContainerFile		AS STRING GET XPorterSettings.MenuFolder + "\\MenuContainer.prg"

	PUBLIC STATIC PROPERTY DesignerPrefixFile		AS STRING GET XPorterSettings.DesignerFolder + "\\prefix.prg"
	PUBLIC STATIC PROPERTY DesignerStartTypeFile	AS STRING GET XPorterSettings.DesignerFolder + "\\starttype.prg"
	PUBLIC STATIC PROPERTY DesignerEndTypeFile		AS STRING GET XPorterSettings.DesignerFolder + "\\endtype.prg"
	PUBLIC STATIC PROPERTY DesignerInitTypeFile		AS STRING GET XPorterSettings.DesignerFolder + "\\inittype.prg"
	PUBLIC STATIC PROPERTY FormPrefixFile			AS STRING GET XPorterSettings.FormFolder + "\\prefix.prg"
	PUBLIC STATIC PROPERTY FormStartTypeFile		AS STRING GET XPorterSettings.FormFolder + "\\starttype.prg"
	PUBLIC STATIC PROPERTY FormEndTypeFile			AS STRING GET XPorterSettings.FormFolder + "\\endtype.prg"
	PUBLIC STATIC PROPERTY FormInitTypeFile			AS STRING GET XPorterSettings.FormFolder + "\\inittype.prg"

	PUBLIC STATIC PROPERTY SingleFormPrefixFile		AS STRING GET XPorterSettings.SingleFolder + "\\prefix.prg"
	PUBLIC STATIC PROPERTY SingleFormStartTypeFile	AS STRING GET XPorterSettings.SingleFolder + "\\starttype.prg"
	PUBLIC STATIC PROPERTY SingleFormEndTypeFile	AS STRING GET XPorterSettings.SingleFolder + "\\endtype.prg"
	PUBLIC STATIC PROPERTY SingleFormInitTypeFile	AS STRING GET XPorterSettings.SingleFolder + "\\inittype.prg"
	PUBLIC STATIC PROPERTY BindingCodeFile			AS STRING GET XPorterSettings.SingleFolder + "\\inittypeBinding.prg"

	PUBLIC STATIC PROPERTY SingleNoContainerFormPrefixFile			AS STRING GET XPorterSettings.SingleFolder + "\\prefixNotContainer.prg"
	PUBLIC STATIC PROPERTY SingleNoContainerFormStartTypeFile		AS STRING GET XPorterSettings.SingleFolder + "\\starttypeNotContainer.prg"
	PUBLIC STATIC PROPERTY SingleNoContainerFormEndTypeFile			AS STRING GET XPorterSettings.SingleFolder + "\\endtypeNotContainer.prg"
	PUBLIC STATIC PROPERTY SingleNoContainerFormInitTypeFile		AS STRING GET XPorterSettings.SingleFolder + "\\inittypeNotContainer.prg"

#endregion

	CONSTRUCTOR()

		SELF:OutputPath := ""
		SELF:ItemsPath := ""
		SELF:ConvertHandlers := FALSE
		SELF:ConvertUserDef := FALSE
		SELF:ConvertThisObject := TRUE
		SELF:ConvertStatement := TRUE
		SELF:ConvertStatementOnlyIfLast := FALSE
		SELF:KeepOriginal := TRUE
		SELF:NameUDF := FALSE
		SELF:RemoveSet := TRUE
		SELF:PrefixClassFile := TRUE
		SELF:Modifier := "PUBLIC"
		SELF:ItemsType := XPorterSettings.DefaultFolders
		SELF:LibInSubFolder := TRUE
		SELF:IgnoreErrors := TRUE
		SELF:StoreInFolders := FALSE
		SELF:EmptyFolder := TRUE
		SELF:PrefixEvent := FALSE
		SELF:KeepFoxProEventName := TRUE
		RETURN

	/// <summary>
	/// Output Folder
	/// </summary>
	/// <value></value>
	PROPERTY OutputPath AS STRING AUTO

	/// <summary>
	/// Folder where are stored the VFP Items.
	/// Used as FileName when exporting
	/// </summary>
	/// <value></value>
	PROPERTY ItemsPath AS STRING AUTO

	/// <summary>
	/// Put the original code in comments when exporting
	/// </summary>
	/// <value></value>
	PROPERTY KeepOriginal AS LOGIC AUTO

	/// <summary>
	/// With User-Defined Method... Keep the Original name, or the one the XPorter has built ?
	/// When False, keep the original
	/// </summary>
	/// <value></value>
	PROPERTY NameUDF AS LOGIC AUTO

	/// <summary>
	/// Remove the "Set_" prefix, if any, in EventHandlers
	/// </summary>
	/// <value></value>
	PROPERTY RemoveSet AS LOGIC AUTO

	/// <summary>
	/// Prefix FileName with the name of the currently processed Form/Library/...
	/// </summary>
	/// <value></value>
	PROPERTY PrefixClassFile AS LOGIC AUTO

	/// <summary>
	/// Default modifier for fields : Public, Protected, Private
	/// </summary>
	/// <value></value>
	PROPERTY Modifier AS STRING AUTO

	/// <summary>
	/// Folders names (See StoreInFolders) setting.
	/// </summary>
	/// <value></value>
	PROPERTY FolderNames AS Dictionary<STRING,STRING>
		GET
			VAR FolderNames := SELF:ItemsType
			VAR names := FolderNames:Split( <CHAR>{ ';' } )
			IF names:Length != 7
				FolderNames := XPorterSettings.DefaultFolders
				names := FolderNames:Split( <CHAR>{ ';' } )
			ENDIF
			VAR Folders := Dictionary<STRING,STRING>{}
			FOREACH VAR name IN names
				VAR data := name:Split( <CHAR>{ '=' } )
				Folders:Add( data[1], data[2])
			NEXT
			RETURN Folders
		END GET
	END PROPERTY

	/// <summary>
	/// Folders names (See StoreInFolders) setting.
	/// "Forms=Forms;Libs=Libs;..."
	/// </summary>
	/// <value></value>
	PROPERTY ItemsType AS STRING AUTO



	/// <summary>
	/// Each Item type is stored in is own Folder ( Form, Menu, ... )
	/// </summary>
	/// <value></value>
	PROPERTY StoreInFolders AS LOGIC AUTO

	/// <summary>
	/// Empty the destinaiton Folder before exporting
	/// </summary>
	/// <value></value>
	PROPERTY EmptyFolder AS LOGIC AUTO

	/// <summary>
	/// Indicate if Lib Files are exported in the same folder as Project files (and Form), or in a SubFolder
	/// </summary>
	PROPERTY LibInSubFolder AS LOGIC AUTO

	/// <summary>
	/// Keep trying to export
	/// </summary>
	/// <value></value>
	PROPERTY IgnoreErrors AS LOGIC AUTO

	/// <summary>
	/// Apply CodeConverter to Event Handlers
	/// </summary>
	PROPERTY ConvertHandlers AS LOGIC AUTO

	/// <summary>
	/// Convert Statement to Method Call
	/// </summary>
	/// <value></value>
	PROPERTY ConvertStatement AS LOGIC AUTO

	/// <summary>
	/// Convert Statement only if it is the last on line
	/// </summary>
	/// <value></value>
	PROPERTY ConvertStatementOnlyIfLast AS LOGIC AUTO

	/// <summary>
	/// Apply CodeConverter to User-Def Methods
	/// </summary>
	PROPERTY ConvertUserDef AS LOGIC AUTO

	/// <summary>
	/// Apply ThisObject CodeConverter to all Methods
	/// </summary>
	PROPERTY ConvertThisObject AS LOGIC AUTO

	/// <summary>
	/// Prefix Event methods with the name of the FORM
	/// </summary>
	PROPERTY PrefixEvent AS LOGIC AUTO

	/// <summary>
	/// Keep the FoxPro Event name when generating EventHandlers
	/// </summary>
	/// <value></value>
	PROPERTY KeepFoxProEventName AS LOGIC AUTO


	/// <summary>
	/// Generate EventHandlers only for Event that appears in EventRules.json
	/// </summary>
	/// <value></value>
	PROPERTY GenerateOnlyHandledEvent AS LOGIC AUTO


END CLASS
END NAMESPACE // VFPXPorterLib