//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Security;
using System.Text;

#pragma warning disable RS0016
namespace XSharp.Build
{

    public abstract class CreateManifestResourceName : Task
    {

        protected Dictionary<string, ITaskItem> itemSpecToTaskitem = new Dictionary<string, ITaskItem>(StringComparer.OrdinalIgnoreCase);

        public bool PrependCultureAsDirectory { get; set; }

        public bool UseDependentUponConvention { get; set; }

        [Required]
        public ITaskItem[] ResourceFiles { get; set; }

        public string RootNamespace { get; set; }

        [Output]
        public ITaskItem[] ManifestResourceNames { get; private set; }

        [Output]
        public ITaskItem[] ResourceFilesWithManifestResourceNames { get; set; }

        protected abstract string CreateManifestName(string fileName, string linkFileName, string rootNamespaceName, string dependentUponFileName, Stream binaryStream);

        protected abstract bool IsSourceFile(string fileName);

        private Stream CreateFileStreamOverNewFileStream(string path, FileMode mode, FileAccess access)
        {
            return new FileStream(path, mode, access);
        }

        internal bool Execute(CreateFileStream createFileStream)
        {
            ManifestResourceNames = new TaskItem[ResourceFiles.Length];
            ResourceFilesWithManifestResourceNames = new TaskItem[ResourceFiles.Length];
            bool result = true;
            int num = 0;
            if (RootNamespace != null)
            {
                base.Log.LogMessage(MessageImportance.Low, "CreateManifestResourceName.RootNamespace \"{0}\"", RootNamespace);
            }
            else
            {
                base.Log.LogMessage(MessageImportance.Low, "CreateManifestResourceName.RootNamespace is empty");
            }
            var resourceFiles = ResourceFiles;
            foreach (ITaskItem taskItem in resourceFiles)
            {
                try
                {
                    string itemSpec = taskItem.ItemSpec;
                    string fileNameWithoutExtension = Path.GetFileNameWithoutExtension(itemSpec);
                    string dependentUpOn = taskItem.GetMetadata("DependentUpon");
                    string type = taskItem.GetMetadata("Type");
                    bool isResx = !string.IsNullOrEmpty(type) && type.ToLower() == "resx";
                    if (string.IsNullOrEmpty(dependentUpOn))
                    {
                        isResx = Path.GetExtension(itemSpec).ToLower() == ".resx";
                    }
                    if (isResx && UseDependentUponConvention && string.IsNullOrEmpty(dependentUpOn))
                    {
                        var temp = Path.ChangeExtension(Path.GetFileName(itemSpec), Constants.SourceFileExtension);
                        if (taskItem.GetMetadata("WithCulture").ToLower() == "true")
                        {
                            string culture = taskItem.GetMetadata("Culture");
                            if (!string.IsNullOrEmpty(culture))
                            {
                                int length = fileNameWithoutExtension.Length - culture.Length - 1;
                                temp = fileNameWithoutExtension.Substring(0, length) + Constants.SourceFileExtension;
                            }
                        }
                        if (File.Exists(Path.Combine(Path.GetDirectoryName(itemSpec), temp)))
                        {
                            dependentUpOn = temp;
                        }
                    }
                    var exists = !string.IsNullOrEmpty(dependentUpOn) && IsSourceFile(dependentUpOn);
                    if (exists)
                    {
                        base.Log.LogMessage(MessageImportance.Low, "Resource file \"{0}\" depends on \"{1}\"", itemSpec, dependentUpOn);
                    }
                    else
                    {
                        base.Log.LogMessage(MessageImportance.Low, "Resource file \"{0}\" doesn't depend on any other file", itemSpec);
                    }
                    Stream stream = null;
                    if (exists)
                    {
                        string path = Path.Combine(Path.GetDirectoryName(itemSpec), dependentUpOn);
                        stream = createFileStream(path, FileMode.Open, FileAccess.Read);
                    }
                    itemSpecToTaskitem[taskItem.ItemSpec] = taskItem;
                    string text;
                    using (stream)
                    {
                        text = CreateManifestName(itemSpec, taskItem.GetMetadata("TargetPath"), RootNamespace, exists ? dependentUpOn : null, stream);
                    }
                    ManifestResourceNames[num] = new TaskItem(taskItem);
                    ManifestResourceNames[num].ItemSpec = text;
                    ResourceFilesWithManifestResourceNames[num] = new TaskItem(taskItem);
                    var resFileItem = ResourceFilesWithManifestResourceNames[num];
                    resFileItem.SetMetadata("ManifestResourceName", text);
                    if (string.IsNullOrEmpty(resFileItem.GetMetadata("LogicalName")) && string.Equals(resFileItem.GetMetadata("Type"), "Non-Resx", StringComparison.OrdinalIgnoreCase))
                    {
                        resFileItem.SetMetadata("LogicalName", text);
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
