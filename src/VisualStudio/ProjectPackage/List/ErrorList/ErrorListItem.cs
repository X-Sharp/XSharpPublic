//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using XSharp.Project;

namespace XSharp.Project
{
    internal interface IErrorListItem: XSharpModel.IXErrorPosition, IListItem
    {
        /// <summary>
        /// Error message
        /// </summary>
        string Message { get; set; }

        /// <summary>
        /// File name of the error item
        /// </summary>
        string Filename { get; set; }

        /// <summary>
        /// Project name of the error item
        /// </summary>
        string ProjectName { get; set; }

        /// <summary>
        /// Severity of the error item
        /// </summary>
        MessageSeverity Severity { get; set; }

        /// <summary>
        /// Error source
        /// </summary>
        ErrorSource ErrorSource { get; set; }

        /// <summary>
        /// Error code for the error item
        /// </summary>
        string ErrorCode { get; set; }

        /// <summary>
        /// Category of the error
        /// </summary>
        string ErrorCategory { get; set; }

        /// <summary>
        /// BuildTool
        /// </summary>
        string BuildTool { get; set; }

        /// <summary>
        /// ProjectGuid
        /// </summary>
        Guid ProjectGuid { get; set; }

        string Key { get; }
    }
    internal class ErrorListItem : IErrorListItem, XSharpModel.IXErrorPosition
    {
        protected const string ProjectNames = StandardTableKeyNames.ProjectName + "s";
        protected const string ProjectGuids = StandardTableKeyNames.ProjectGuid + "s";

        internal ErrorListItem()
        {
            ErrorCategory = "Compiler";

        }
        public int Column { get; set; }
        public int Length { get; set; }

        public string ErrorCode { get; set; }


        public ErrorSource ErrorSource { get; set; }

        public string Filename { get; set; }
        public int Line { get; set; }

        public string Message { get; set; }

        public string ProjectName { get; set; }

        public MessageSeverity Severity { get; set; }
        public string ErrorCategory { get; set; }
        public string BuildTool { get; set; }

        public Guid ProjectGuid { get; set; }

        public string Key
        {
            get
            {
                return Filename + Line.ToString() + Column.ToString() + ErrorCode + Message;
            }
        }
        public bool GetValue(string columnName, out object content)
        {
            switch (columnName)
            {
                case StandardTableKeyNames.ErrorSource:
                    content = this.ErrorSource;
                    return true;
                case StandardTableKeyNames.ErrorCode:
                    content = this.ErrorCode;
                    return true;
                case StandardTableKeyNames.Text:
                    content = this.Message;
                    return true;
                case StandardTableKeyNames.ProjectName:
                    content = this.ProjectName;
                    return true;
                case StandardTableKeyNames.DocumentName:
                    content = this.Filename;
                    return true;
                case StandardTableKeyNames.Line:
                    content = ListHelpers.GetAdjustedLineOrColumn(this.Line);
                    return true;
                case StandardTableKeyNames.Column:
                    content = ListHelpers.GetAdjustedLineOrColumn(this.Column);
                    return true;
                case StandardTableKeyNames.ErrorSeverity:
                    content = (__VSERRORCATEGORY)this.Severity;
                    return true;
                case StandardTableKeyNames.ErrorCategory:
                    content = this.ErrorCategory;
                    return true;
                case StandardTableKeyNames.TaskCategory:
                    if (this.ErrorSource == ErrorSource.Build)
                        content = VSTASKCATEGORY.CAT_BUILDCOMPILE;
                    else
                        content = VSTASKCATEGORY.CAT_CODESENSE;
                    return true;
                case StandardTableKeyNames.BuildTool:
                    content = this.BuildTool;
                    return true;
                case StandardTableKeyNames.ErrorRank:
                    content = ErrorRank.Lexical;
                    return true;
                case StandardTableKeyNames.ProjectGuid:
                    content = this.ProjectGuid; // _projectGuid;
                    return true;
                case StandardTableKeyNames.HelpKeyword:
                    content = "";
                    return true;
                case StandardTableKeyNames.HelpLink:
                case StandardTableKeyNames.ErrorCodeToolTip:
                    string errorcode = this?.ErrorCode?.ToLower();
                    if (errorcode != null && errorcode.StartsWith("xs"))
                    {
                        if (columnName == StandardTableKeyNames.HelpLink)
                            content = "https://www.xsharp.eu/help/" + errorcode + ".html";
                        else
                            content = "Get help for '" + this.ErrorCode + "' from the XSharp website";
                        return true;
                    }
                    break;
                case ProjectNames:
                    content = new string[] { };
                    return true;
                case ProjectGuids:
                    content = new Guid[] { this.ProjectGuid };
                    return true;

            }
            content = null;
            return false;
        }
    }
}
