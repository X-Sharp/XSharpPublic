using Microsoft.Build.Framework;
using Microsoft.Build.Shared;
using Microsoft.Build.Tasks;
using Microsoft.Build.Utilities;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Security;
using System.Text;

namespace XSharp.Build
{


    public abstract class CreateManifestResourceName : TaskExtension
    {
        private ITaskItem[] _resourceFiles;

        private string _rootNamespace;

        private ITaskItem[] _manifestResourceNames;

        private ITaskItem[] _resourceFilesWithManifestResourceNames;

        private bool _prependCultureAsDirectory = true;

        protected Dictionary<string, ITaskItem> itemSpecToTaskitem = new Dictionary<string, ITaskItem>(StringComparer.OrdinalIgnoreCase);

        public bool PrependCultureAsDirectory
        {
            get
            {
                return _prependCultureAsDirectory;
            }
            set
            {
                _prependCultureAsDirectory = value;
            }
        }

        [Required]
        public ITaskItem[] ResourceFiles
        {
            get
            {
                Microsoft.Build.Shared.ErrorUtilities.VerifyThrowArgumentNull(_resourceFiles, "resourceFiles");
                return _resourceFiles;
            }
            set
            {
                _resourceFiles = value;
            }
        }

        public string RootNamespace
        {
            get
            {
                return _rootNamespace;
            }
            set
            {
                _rootNamespace = value;
            }
        }

        [Output]
        public ITaskItem[] ManifestResourceNames => _manifestResourceNames;

        [Output]
        public ITaskItem[] ResourceFilesWithManifestResourceNames
        {
            get
            {
                return _resourceFilesWithManifestResourceNames;
            }
            set
            {
                _resourceFilesWithManifestResourceNames = value;
            }
        }

        protected abstract string CreateManifestName(string fileName, string linkFileName, string rootNamespaceName, string dependentUponFileName, Stream binaryStream);

        protected abstract bool IsSourceFile(string fileName);

        private Stream CreateFileStreamOverNewFileStream(string path, FileMode mode, FileAccess access)
        {
            return new FileStream(path, mode, access);
        }

        internal bool Execute(CreateFileStream createFileStream)
        {
            _manifestResourceNames = new TaskItem[ResourceFiles.Length];
            _resourceFilesWithManifestResourceNames = new TaskItem[ResourceFiles.Length];
            bool result = true;
            int num = 0;
            if (RootNamespace != null)
            {
                base.Log.LogMessage(MessageImportance.Low, "Root namespace \"{0}\"", _rootNamespace);
            }
            else
            {
                base.Log.LogMessage(MessageImportance.Low, "Root namespace is empty");
            }
            ITaskItem[] resourceFiles = ResourceFiles;
            foreach (ITaskItem taskItem in resourceFiles)
            {
                try
                {
                    string itemSpec = taskItem.ItemSpec;
                    string metadata = taskItem.GetMetadata("DependentUpon");
                    bool flag = metadata != null && metadata.Length > 0 && IsSourceFile(metadata);
                    if (flag)
                    {
                        base.Log.LogMessage(MessageImportance.Low, "Resource file \"{0}\" depends on \"{1}\"", itemSpec, metadata);
                    }
                    else
                    {
                        base.Log.LogMessage(MessageImportance.Low, "Resource file \"{0}\" doesn't depend on any other file", itemSpec);
                    }
                    Stream stream = null;
                    if (flag)
                    {
                        string path = Path.Combine(Path.GetDirectoryName(itemSpec), metadata);
                        stream = createFileStream(path, FileMode.Open, FileAccess.Read);
                    }
                    itemSpecToTaskitem[taskItem.ItemSpec] = taskItem;
                    string text;
                    using (stream)
                    {
                        text = CreateManifestName(itemSpec, taskItem.GetMetadata("TargetPath"), RootNamespace, flag ? metadata : null, stream);
                    }
                    _manifestResourceNames[num] = new TaskItem(taskItem);
                    _manifestResourceNames[num].ItemSpec = text;
                    _resourceFilesWithManifestResourceNames[num] = new TaskItem(taskItem);
                    _resourceFilesWithManifestResourceNames[num].SetMetadata("ManifestResourceName", text);
                    if (string.IsNullOrEmpty(_resourceFilesWithManifestResourceNames[num].GetMetadata("LogicalName")) && string.Equals(_resourceFilesWithManifestResourceNames[num].GetMetadata("Type"), "Non-Resx", StringComparison.OrdinalIgnoreCase))
                    {
                        _resourceFilesWithManifestResourceNames[num].SetMetadata("LogicalName", text);
                    }
                    base.Log.LogMessage(MessageImportance.Low, "MSB3041: Unable to create a manifest resource name for \"{0}\", {1}", itemSpec, text);
                }
                catch (Exception ex)
                {
                    if (NotExpectedException(ex))
                    {
                        throw;
                    }
                    base.Log.LogError("MSB3041: Unable to create a manifest resource name for \"{0}\", {1}", taskItem.ItemSpec, ex.Message);
                    result = false;
                }
                num++;
            }
            return result;
        }
        internal static bool NotExpectedException(Exception e)
        {
            if (e is UnauthorizedAccessException || e is NotSupportedException || (e is ArgumentException && !(e is ArgumentNullException)) || e is SecurityException || e is IOException)
            {
                return false;
            }
            return true;
        }
        public override bool Execute()
        {
            return Execute(CreateFileStreamOverNewFileStream);
        }

        private static bool IsValidEverettIdFirstChar(char c)
        {
            if (!char.IsLetter(c))
            {
                return char.GetUnicodeCategory(c) == UnicodeCategory.ConnectorPunctuation;
            }
            return true;
        }

        private static bool IsValidEverettIdChar(char c)
        {
            UnicodeCategory unicodeCategory = char.GetUnicodeCategory(c);
            if (!char.IsLetterOrDigit(c) && unicodeCategory != UnicodeCategory.ConnectorPunctuation && unicodeCategory != UnicodeCategory.NonSpacingMark && unicodeCategory != UnicodeCategory.SpacingCombiningMark)
            {
                return unicodeCategory == UnicodeCategory.EnclosingMark;
            }
            return true;
        }

        private static string MakeValidEverettSubFolderIdentifier(string subName)
        {
            Microsoft.Build.Shared.ErrorUtilities.VerifyThrowArgumentNull(subName, "subName");
            if (subName.Length == 0)
            {
                return subName;
            }
            StringBuilder stringBuilder = new StringBuilder(subName.Length + 1);
            if (!IsValidEverettIdFirstChar(subName[0]))
            {
                if (!IsValidEverettIdChar(subName[0]))
                {
                    stringBuilder.Append('_');
                }
                else
                {
                    stringBuilder.Append('_');
                    stringBuilder.Append(subName[0]);
                }
            }
            else
            {
                stringBuilder.Append(subName[0]);
            }
            for (int i = 1; i < subName.Length; i++)
            {
                if (!IsValidEverettIdChar(subName[i]))
                {
                    stringBuilder.Append('_');
                }
                else
                {
                    stringBuilder.Append(subName[i]);
                }
            }
            return stringBuilder.ToString();
        }

        internal static string MakeValidEverettFolderIdentifier(string name)
        {
            Microsoft.Build.Shared.ErrorUtilities.VerifyThrowArgumentNull(name, "name");
            StringBuilder stringBuilder = new StringBuilder(name.Length + 1);
            string[] array = name.Split('.');
            stringBuilder.Append(MakeValidEverettSubFolderIdentifier(array[0]));
            for (int i = 1; i < array.Length; i++)
            {
                stringBuilder.Append('.');
                stringBuilder.Append(MakeValidEverettSubFolderIdentifier(array[i]));
            }
            if (stringBuilder.ToString() == "_")
            {
                stringBuilder.Append('_');
            }
            return stringBuilder.ToString();
        }

        public static string MakeValidEverettIdentifier(string name)
        {
            Microsoft.Build.Shared.ErrorUtilities.VerifyThrowArgumentNull(name, "name");
            StringBuilder stringBuilder = new StringBuilder(name.Length);
            string[] array = name.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
            stringBuilder.Append(MakeValidEverettFolderIdentifier(array[0]));
            for (int i = 1; i < array.Length; i++)
            {
                stringBuilder.Append('.');
                stringBuilder.Append(MakeValidEverettFolderIdentifier(array[i]));
            }
            return stringBuilder.ToString();
        }
    }
    internal delegate Stream CreateFileStream(string path, FileMode mode, FileAccess access);
}
