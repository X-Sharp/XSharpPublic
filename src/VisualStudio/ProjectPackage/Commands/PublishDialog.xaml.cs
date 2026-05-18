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
            PublishOptions = new PublishOptions(projectPath);
            PublishOptions.Restore();

            // Set DataContext for binding
            this.DataContext = PublishOptions;

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
                            TargetFrameworkComboBox.Items.Add(framework);
                        }
                        if (TargetFrameworkComboBox.Items.Count > 0 && string.IsNullOrEmpty(PublishOptions.TargetFramework))
                        {
                            TargetFrameworkComboBox.SelectedIndex = 0;
                        }
                    }
                }
            }
            catch
            {
                ;
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
            if (string.IsNullOrWhiteSpace(PublishOptions.OutputPath))
            {
                MessageBox.Show("Please specify an output path.", "Validation Error",
                    MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            if (PublishOptions.SelfContained == true && string.IsNullOrWhiteSpace(PublishOptions.Runtime))
            {
                MessageBox.Show("Self-Contained deployment requires a Target Runtime (RID) to be specified.",
                    "Validation Error", MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            // If runtime is specified but SelfContained is not set, default to false
            if (!PublishOptions.SelfContained.HasValue && !string.IsNullOrWhiteSpace(PublishOptions.Runtime))
            {
                PublishOptions.SelfContained = false;
            }

            DialogResult = true;
            Close();
        }

        private void Cancel_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = false;
            Close();
        }
    }
}
