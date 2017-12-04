//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using System.Collections.Generic;


namespace XSharp.Project
{
    internal class ErrorSnapshot : WpfTableEntriesSnapshotBase
    {
        private const int UndefinedLineOrColumn = -1;
        private readonly int _versionNumber;
        public readonly List<IErrorListItem> Errors;
        private Guid _projectGuid;
        protected const string ProjectNames = StandardTableKeyNames.ProjectName + "s";
        protected const string ProjectGuids = StandardTableKeyNames.ProjectGuid + "s";
        internal ErrorSnapshot(int versionNumber, IList<IErrorListItem> errors, Guid projectGuid)
        {
            _versionNumber = versionNumber;
            _projectGuid = projectGuid;
            Errors = new List<IErrorListItem>(errors);
        }

        public override int Count
        {
            get
            {
                return Errors.Count;
            }
        }

        public override int VersionNumber
        {
            get
            {
                return _versionNumber;
            }
        }

        /// <summary>
        /// Attempts to get a column value for the item at index in the snapshot. Note that this method
        /// gets called for any columns that the control supports. If we do not have a value
        /// to supply for the column or if somehow we got an invalid index the content needs to be
        /// set to null and return false which lets the table control set the value.
        /// </summary>
        public override bool TryGetValue(int index, string columnName, out object content)
        {
            if (index >= 0 && index < Errors.Count)
            {
                var error = Errors[index];
                switch (columnName)
                {
                    case StandardTableKeyNames.ErrorSource:
                        content = error.ErrorSource;
                        return true;
                    case StandardTableKeyNames.ErrorCode:
                        content = error.ErrorCode;
                        return true;
                    case StandardTableKeyNames.Text:
                        content = error.Message;
                        return true;
                    case StandardTableKeyNames.ProjectName:
                        content = error.ProjectName;
                        return true;
                    case StandardTableKeyNames.DocumentName:
                        content = error.Filename;
                        return true;
                    case StandardTableKeyNames.Line:
                        content = GetErrorListAdjustedLineOrColumn(error.Line);
                        return true;
                    case StandardTableKeyNames.Column:
                        content = GetErrorListAdjustedLineOrColumn(error.Column);
                        return true;
                    case StandardTableKeyNames.ErrorSeverity:
                        content = error.Severity.ToVSERRORCATEGORY();
                        return true;
                    case StandardTableKeyNames.ErrorCategory:
                        content = error.ErrorCategory;
                        return true;
                    case StandardTableKeyNames.TaskCategory:
                        if (error.ErrorSource == ErrorSource.Build)
                            content = VSTASKCATEGORY.CAT_BUILDCOMPILE;
                        else
                            content = VSTASKCATEGORY.CAT_CODESENSE; ;
                        return true;
                    case StandardTableKeyNames.BuildTool:
                        content = error.BuildTool;
                        return true;
                    case StandardTableKeyNames.ErrorRank:
                        content = ErrorRank.Lexical;
                        return true;
                    case StandardTableKeyNames.ProjectGuid:
                        content = _projectGuid;
                        return false;
                    case StandardTableKeyNames.HelpKeyword:
                        content = "";
                        return true;
                    case StandardTableKeyNames.HelpLink:
                        string errorcode = error?.ErrorCode.ToLower();
                        if (errorcode != null && errorcode.StartsWith("xs"))
                        {
                            content = "https://www.xsharp.info/help/" + errorcode + ".html";
                            return true;
                        }
                        break;
                    case StandardTableKeyNames.ErrorCodeToolTip:
                        content = "Get help for '"+error.ErrorCode+"' from the XSharp website";
                        return true;

                    case ProjectNames:
                        content = new string[] {  };
                        return true;
                    case ProjectGuids:
                        content = new Guid[] {  };
                        return true;

                }
            }

            // This method gets called for anything that the control supports. If we do not have a value
            // to supply for the column or if somehow we got an invalid index the content needs to be
            // set to null and return false which lets the table control set the value.
            content = null;
            return false;
        }


        /// <summary>
        /// Helper to account for the fact the error list is 0 based.
        /// </summary>
        private static int GetErrorListAdjustedLineOrColumn(int lineOrColumn)
        {
            if (lineOrColumn != UndefinedLineOrColumn)
            {
                return lineOrColumn > 0 ? lineOrColumn - 1 : 0;
            }

            return lineOrColumn;
        }
    }
}