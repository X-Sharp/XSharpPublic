using Microsoft.VisualStudio.PlatformUI;
using System;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Controls;

namespace XSharp.Project
{
    public partial class PublishDialog : Window
    {
        private readonly string _projectPath;

        public PublishOptions PublishOptions { get; private set; }

        public PublishDialog(string projectPath)
        {
            InitializeComponent();
            _projectPath = projectPath;
            PublishOptions = new PublishOptions();

            LoadProjectSettings();
        }

        private void LoadProjectSettings()
        {
            try
            {
                var prj = (XSharpSdkProjectNode) XSharpProjectNode.FindProject(_projectPath);
                if (prj != null)
                {
                    // Get target frameworks
                    var targetFrameworks = prj.TargetFrameworks;
                    if (targetFrameworks != null && targetFrameworks.Count > 0)
                    {
                        foreach (var framework in targetFrameworks)
                        {
                            TargetFrameworkComboBox.Items.Add(new ComboBoxItem { Content = framework });
                        }
                        if (TargetFrameworkComboBox.Items.Count > 0)
                        {
                            ((ComboBoxItem)TargetFrameworkComboBox.Items[0]).IsSelected = true;
                        }
                    }

                    // Set default output path
                    var projectDir = Path.GetDirectoryName(_projectPath);
                    var projectName = Path.GetFileNameWithoutExtension(_projectPath);
                    OutputPathTextBox.Text = Path.Combine(projectDir, "bin", "publish", projectName);
                }
            }
            catch
            {
                // If we can't load project settings, use defaults
                OutputPathTextBox.Text = Path.Combine(
                    Path.GetDirectoryName(_projectPath),
                    "bin",
                    "publish",
                    Path.GetFileNameWithoutExtension(_projectPath));
            }
        }

        private void BrowseOutputPath_Click(object sender, RoutedEventArgs e)
        {
            var dialog = new System.Windows.Forms.FolderBrowserDialog
            {
                Description = "Select publish output folder",
                SelectedPath = OutputPathTextBox.Text
            };

            if (dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                OutputPathTextBox.Text = dialog.SelectedPath;
            }
        }

        private void Publish_Click(object sender, RoutedEventArgs e)
        {
            // Validate inputs
            if (string.IsNullOrWhiteSpace(OutputPathTextBox.Text))
            {
                MessageBox.Show("Please specify an output path.", "Validation Error",
                    MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            if (SelfContainedCheckBox.IsChecked == true && string.IsNullOrWhiteSpace(GetRuntimeValue()))
            {
                MessageBox.Show("Self-Contained deployment requires a Target Runtime (RID) to be specified.",
                    "Validation Error", MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            // Build publish options
            PublishOptions = new PublishOptions
            {
                Configuration = ((ComboBoxItem)ConfigurationComboBox.SelectedItem)?.Content?.ToString() ?? "Release",
                TargetFramework = GetTargetFrameworkValue(),
                Runtime = GetRuntimeValue(),
                OutputPath = OutputPathTextBox.Text,
                SelfContained = SelfContainedCheckBox.IsChecked == true ? (bool?)true :
                               !string.IsNullOrWhiteSpace(GetRuntimeValue()) ? (bool?)false : null,
                SingleFile = SingleFileCheckBox.IsChecked == true,
                ReadyToRun = ReadyToRunCheckBox.IsChecked == true,
                Trimmed = TrimmedCheckBox.IsChecked == true
            };

            DialogResult = true;
            Close();
        }

        private void Cancel_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = false;
            Close();
        }

        private string GetTargetFrameworkValue()
        {
            if (TargetFrameworkComboBox.SelectedItem is ComboBoxItem item)
            {
                return item.Content?.ToString();
            }
            return TargetFrameworkComboBox.Text;
        }

        private string GetRuntimeValue()
        {
            if (RuntimeComboBox.SelectedItem is ComboBoxItem item)
            {
                return item.Content?.ToString();
            }
            return RuntimeComboBox.Text;
        }
    }
}
