using Microsoft.VisualStudio.Shell.TableManager;
using System;
using XSharp.Project;

namespace XSharp.Project
{
    internal interface IErrorListItem
    {
        /// <summary>
        /// Line of code on the error item
        /// </summary>
        int Line { get; set; }

        /// <summary>
        /// Column of code on the error item
        /// </summary>
        int Column { get; set; }

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

        string Key { get; }
    }
    internal class ErrorListItem : IErrorListItem
    {

        internal ErrorListItem()
        {
            ErrorCategory = "Compiler";

        }
        public int Column { get; set; }

        public string ErrorCode { get; set; }


        public ErrorSource ErrorSource { get; set; }

        public string Filename { get; set; }
        public int Line { get; set; }

        public string Message { get; set; }

        public string ProjectName { get; set; }

        public MessageSeverity Severity { get; set; }
        public string ErrorCategory { get; set; }
        public string BuildTool { get; set; }

        public string Key
        {
            get
            {
                return Filename + Line.ToString() + Column.ToString()+ErrorCode;
            }
        }
    }

}