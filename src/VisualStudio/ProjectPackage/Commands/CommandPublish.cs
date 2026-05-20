using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;

using Newtonsoft.Json;

using System;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;

using Task = System.Threading.Tasks.Task;

namespace XSharp.Project
{



        [Command(PackageIds.idPublishProject)]
    internal sealed class CommandPublish : CommandBuild<CommandPublish>
    {
        protected override string CommandName => "Build.PublishSelection";
        protected override string CommandDescription => "Publish";

        public const int CommandID = (int)2005; // ECMD_PUBLISHSELECTION
        public static readonly Guid CommandGroup = VsMenus.guidStandardCommandSet2K;

        protected override async Task DoCmdAsync()
        {
            if (!await VerifySdkProjectAsync())
            {
                return;
            }

            // Show publish dialog to get options
            var dialog = new PublishDialog(projectPath);
            var result = dialog.ShowDialog();

            if (result == true)
            {
                await PublishProjectAsync(projectPath, dialog.PublishOptions);
            }
        }

        private async Task PublishProjectAsync(string projectPath, PublishOptions options)
        {
            try
            {
                await VS.StatusBar.ShowMessageAsync("Publishing project...");

                var arguments = BuildPublishArguments(projectPath, options);
                options.Save();
                var result = await CreateProcessAsync(arguments,
                    $"Publishing project: {Path.GetFileName(projectPath)}");

                if (result == 0)
                {
                   await VS.MessageBox.ShowAsync(CommandDescription,
                        $"{CommandDescription} published successfully to:\n{options.OutputPath}",
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGICON.OLEMSGICON_INFO,
                        Microsoft.VisualStudio.Shell.Interop.OLEMSGBUTTON.OLEMSGBUTTON_OK);
                }
            }
            catch (Exception ex)
            {
                await VS.MessageBox.ShowErrorAsync(CommandDescription+" Error", $"Failed to publish project:\n{ex.Message}");
            }
        }

        private string BuildPublishArguments(string projectPath, PublishOptions options)
        {
            var args = $"publish \"{projectPath}\"";

            if (!string.IsNullOrEmpty(options.Configuration))
            {
                args += $" -c {options.Configuration}";
            }

            if (!string.IsNullOrEmpty(options.TargetFramework))
            {
                args += $" -f {options.TargetFramework}";
            }

            if (!string.IsNullOrEmpty(options.Runtime))
            {
                args += $" -r {options.Runtime}";
            }

            if (!string.IsNullOrEmpty(options.OutputPath))
            {
                args += $" -o \"{options.OutputPath}\"";
            }

            if (options.SelfContained.HasValue)
            {
                args += options.SelfContained.Value ? " --self-contained" : " --no-self-contained";
            }

            if (options.SingleFile)
            {
                args += " -p:PublishSingleFile=true";
            }

            if (options.ReadyToRun)
            {
                args += " -p:PublishReadyToRun=true";
            }

            if (options.Trimmed)
            {
                args += " -p:PublishTrimmed=true";
            }

            return args;
        }
    }

    public class PublishOptions : INotifyPropertyChanged
    {
        private string _projectDir;
        private string _projectName;
        private string _jsonFile;
        private string _configuration = "Release";
        private string _targetFramework;
        private string _runtime;
        private string _outputPath;
        private bool? _selfContained;
        private bool _singleFile;
        private bool _readyToRun;
        private bool _trimmed;

        public event PropertyChangedEventHandler PropertyChanged;

        public PublishOptions()
        {

        }
        public PublishOptions(string projectPath)
        {
            // Set default output path based on project location
            _projectDir = Path.GetDirectoryName(projectPath);
            _projectName = Path.GetFileNameWithoutExtension(projectPath);
            OutputPath = Path.Combine(_projectDir, "bin", "publish", _projectName);
            _jsonFile = Path.Combine(_projectDir, $"{_projectName}.publishsettings.json");
            var project = XSharpProjectNode.FindProject( projectPath ) as XSharpSdkProjectNode;
            if (project != null)
            {
                TargetFramework = project.TargetFrameworks.FirstOrDefault();
                Runtime = "win-x86"; // Default runtime, could be improved to detect from project
            }
        }

        public string Configuration
        {
            get => _configuration;
            set => SetProperty(ref _configuration, value);
        }

        public string TargetFramework
        {
            get => _targetFramework;
            set => SetProperty(ref _targetFramework, value);
        }

        public string Runtime
        {
            get => _runtime;
            set => SetProperty(ref _runtime, value);
        }

        public string OutputPath
        {
            get => _outputPath;
            set => SetProperty(ref _outputPath, value);
        }

        public bool? SelfContained
        {
            get => _selfContained;
            set => SetProperty(ref _selfContained, value);
        }

        public bool SingleFile
        {
            get => _singleFile;
            set => SetProperty(ref _singleFile, value);
        }

        public bool ReadyToRun
        {
            get => _readyToRun;
            set => SetProperty(ref _readyToRun, value);
        }

        public bool Trimmed
        {
            get => _trimmed;
            set => SetProperty(ref _trimmed, value);
        }

        protected void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        protected bool SetProperty<T>(ref T field, T value, [CallerMemberName] string propertyName = null)
        {
            if (Equals(field, value)) return false;
            field = value;
            OnPropertyChanged(propertyName);
            return true;
        }

        public void Save()
        {
            var str = JsonConvert.SerializeObject(this, Formatting.Indented);
            System.IO.File.WriteAllText(_jsonFile, str);


        }
        public void Restore()
        {
            if (System.IO.File.Exists(_jsonFile))
            {
                var str = System.IO.File.ReadAllText(_jsonFile);
                var options = JsonConvert.DeserializeObject<PublishOptions>(str);
                if (options != null)
                {
                    Configuration = options.Configuration;
                    TargetFramework = options.TargetFramework;
                    Runtime = options.Runtime;
                    OutputPath = options.OutputPath;
                    SelfContained = options.SelfContained;
                    SingleFile = options.SingleFile;
                    ReadyToRun = options.ReadyToRun;
                    Trimmed = options.Trimmed;
                }
            }

        }
    }
}
