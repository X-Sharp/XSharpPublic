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
        object ErrorSource { get; set; }

        /// <summary>
        /// Error code for the error item
        /// </summary>
        object ErrorCode { get; set; }

        /// <summary>
        /// Category of the error
        /// </summary>
        string ErrorCategory { get; set; }
    }
}