using Microsoft.Win32;
using System.ComponentModel;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.Project.Options
{
    public class OtherEditorOptions : BaseOptionModel<OtherEditorOptions>
    {
[Category("Menu Editor")]
        [DisplayName("Parent Class")]
        [Description("The parent class name used when no parent class is specified.")]
        [DefaultValue("Menu")]
        public string MenuParentClass { get; set; } 
        [Category("Menu Editor")]
        [DisplayName("Toolbar Parent Class")]
        [Description("The toolbar parent class name used when no parent class is specified.")]
        [DefaultValue("Toolbar")]
        public string ToolbarParentClass { get; set; } 
        [Category("DbServer Editor")]
        [DisplayName("Parent Class")]
        [Description("The parent class name used when no parent class is specified.")]
        [DefaultValue("DbServer")]
        public string DbServerParentClass { get; set; } 

        [Category("DbServer Editor")]
        [DisplayName("Default RDD")]
        [Description("The default RDD used for the DbServer editor.")]
        [DefaultValue("DBFNTX")]
        public string DbServerDefaultRDD { get; set; } 

        [Category("FieldSpec Editor")]
        [DisplayName("Parent Class")]
        [Description("The parent class name used when no parent class is specified.")]
        [DefaultValue("FieldSpec")]
        public string FieldSpecParentClass { get; set; } 
        [Category("Disassembler")]
        [DisplayName("Program")]
        [Description("The full path to the disassembler path to use, such as ILSpy or Reflector")]
        [DefaultValue("")]
        public string Disassembler { get; set; } 

        [Category("Solution  Explorer")]
        [DisplayName("Hide Includes")]
        [Description("Hide the include files node in the Solution Explorer.")]
        [DefaultValue(false)]
        public bool HideIncludes{ get; set; }

        [Category("Windows Forms Editor")]
        [DisplayName("Backup Source Files")]
        [Description("Backup the Form.Prg and Form.Designer.Prg when saving in the Windows Forms Editor.")]
        public bool BackupFormFiles { get; set; } = false;

        [Category("Windows Forms Editor")]
        [DisplayName("Auto-build for Shadow Designer (SDK-style projects)")]
        [Description("When a SDK-style project's WinForms Designer needs a build to resolve " +
            "assembly references (e.g. the first time it's opened in a session), build " +
            "automatically instead of asking. When off (default), you're prompted to confirm " +
            "the build first.")]
        [DefaultValue(false)]
        public bool AutoBuildForShadowDesigner { get; set; } = false;

        public override void WriteToSettings()
        {
            XCustomEditorSettings.DbServerDefaultRDD = this.DbServerDefaultRDD;
            XCustomEditorSettings.DbServerParentClass = this.DbServerParentClass;
            XCustomEditorSettings.MenuParentClass = this.MenuParentClass;
            XCustomEditorSettings.FieldSpecParentClass = this.FieldSpecParentClass;
            XCustomEditorSettings.ToolbarParentClass = this.ToolbarParentClass;
            XCustomEditorSettings.BackupFormFiles = this.BackupFormFiles;
            XCustomEditorSettings.AutoBuildForShadowDesigner = this.AutoBuildForShadowDesigner;
            XSettings.Disassembler = this.Disassembler;
            XSettings.HideIncludes = this.HideIncludes;
        }

    }
}

