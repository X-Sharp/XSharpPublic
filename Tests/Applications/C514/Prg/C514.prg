// 514. error XS0579: Duplicate 'global  :: System . Runtime . CompilerServices . CompilerGenerated' attribute

[GLOBAL::System.Runtime.CompilerServices.CompilerGeneratedAttribute()] ;
PARTIAL CLASS TestClass
END CLASS


// original code:
BEGIN NAMESPACE VnMacro.Properties
    
    [GLOBAL::System.Runtime.CompilerServices.CompilerGeneratedAttribute()] ;
    [GLOBAL::System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "10.0.0.0")] ;
    INTERNAL SEALED PARTIAL CLASS Settings INHERIT GLOBAL::System.Configuration.ApplicationSettingsBase
        
        STATIC PRIVATE defaultInstance := ((Settings)(GLOBAL::System.Configuration.ApplicationSettingsBase.Synchronized(Settings{}))) AS Settings
        STATIC ACCESS Default() AS Settings
            RETURN defaultInstance
    
    END CLASS
END NAMESPACE

FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:ToString()
RETURN

