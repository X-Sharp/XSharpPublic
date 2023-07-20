using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharp.LanguageService;
using XSharp.Project.Options;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.Project
{
    public class ProjectSystemOptions : OptionsBase
    {
        public ProjectSystemOptions()
        {
            DebuggerOptions = new DebuggerOptions();
            WindowEditorOptions = new WindowEditorOptions();
            OtherEditorOptions = new OtherEditorOptions();
        } 
        public DebuggerOptions DebuggerOptions { get; set; }
        public WindowEditorOptions WindowEditorOptions { get; set; }
        public OtherEditorOptions OtherEditorOptions { get; set; }

        public override void WriteToSettings()
        {
            DebuggerOptions.WriteToSettings();
            WindowEditorOptions.WriteToSettings();
            OtherEditorOptions.WriteToSettings();
        }
        public bool Save()
        {
            try
            {
                var str = JsonConvert.SerializeObject(this, Formatting.Indented);
                CreatePath();
                var sFile = System.IO.Path.Combine(GetPath(), SETTINGS);
                System.IO.File.WriteAllText(sFile, str);
                return true;
            }
            catch
            {
                return false;
            }
        }
        private const string SETTINGS = "ProjectSystemSettings.json";

        public static ProjectSystemOptions Load()
        {
            try
            {
                var sFile = System.IO.Path.Combine(GetPath(), SETTINGS);
                if (System.IO.File.Exists(sFile))
                {
                    var str = System.IO.File.ReadAllText(sFile);
                    return JsonConvert.DeserializeObject<ProjectSystemOptions>(str);
                }
            }
            catch
            {
            }
            return null;
        }

    }
}
