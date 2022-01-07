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

        private bool _debugging = false;
        [Category("Debugging")]
        [DisplayName("Enable XSharp Project System debugging")]
        [Description("Enable or Disable debugging to a LOG file or to the Debug window")]
        public bool Debugging
        {   get => _debugging;
            set
            {
                bool changed = _debugging != value;
                if (changed)
                {
                    _debugging = value;
                    LogtoDisk = value;
                    LogToDebug = value;

                }
            }
        }
        private bool _logToDisk = false;
        [Category("Debugging")]
        [DisplayName("Log to file")]
        [Description("Log to a disk file (in %temp%\\XSharp.Intellisense folder)")]
        public bool LogtoDisk
        {
            get => _logToDisk;
            set
            {
                _logToDisk = value;
                if (_logToDisk && !Debugging)
                {
                    Debugging = true;
                }
            }
        }
        private bool _logToDebug = false;
        [Category("Debugging")]
        [DisplayName("Log to Debug window")]
        [Description("Log to Debugger window")]
        public bool LogToDebug
        {
            get => _logToDebug;
            set
            {
                _logToDebug = value;
                if (_logToDebug && ! Debugging)
                {
                    Debugging = true;
                }
            }
        }




    }
}

