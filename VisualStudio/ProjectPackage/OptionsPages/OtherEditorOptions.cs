using Microsoft.Win32;
using System.ComponentModel;

namespace XSharp.Project.Options
{
    internal class OtherEditorOptions : BaseOptionModel<OtherEditorOptions>
    {
        [Category("Menu Editor")]
        [DisplayName("Parent Class")]
        [Description("The parent class name used when no parent class is specified.")]
        [DefaultValue("Menu")]
        public string MenuParentClass { get; set; } = "Menu";
        [Category("Menu Editor")]
        [DisplayName("Toolbar Parent Class")]
        [Description("The toolbar parent class name used when no parent class is specified.")]
        [DefaultValue("Toolbar")]
        public string ToolbarParentClass { get; set; } = "Toolbar";
        [Category("DbServer Editor")]
        [DisplayName("Parent Class")]
        [Description("The parent class name used when no parent class is specified.")]
        [DefaultValue("DbServer")]
        public string DbServerParentClass { get; set; } = "DbServer";

        [Category("DbServer Editor")]
        [DisplayName("Default RDD")]
        [Description("The default RDD used for the DbServer editor.")]
        [DefaultValue("DBFNTX")]
        public string DbServerDefaultRDD { get; set; } = "DBFNTX";

        [Category("FieldSpec Editor")]
        [DisplayName("Parent Class")]
        [Description("The parent class name used when no parent class is specified.")]
        [DefaultValue("FieldSpec")]
        public string FieldSpecParentClass { get; set; } = "FieldSpec";
        [Category("Disassembler")]
        [DisplayName("Program")]
        [Description("The full path to the disassembler path to use.")]
        public string Disassembler { get; set; } = "";

        [Category("Solution  Explorer")]
        [DisplayName("Hide Includes")]
        [Description("Hide the include files node in the Solution Explorer.")]
        [DefaultValue(false)]
         public bool HideIncludes{ get; set; } = false;

    }
}

