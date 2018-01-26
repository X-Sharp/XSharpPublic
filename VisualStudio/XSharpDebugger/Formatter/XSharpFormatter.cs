//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;
using System;
using System.Collections.ObjectModel;
using System.Linq;
using Type = Microsoft.VisualStudio.Debugger.Metadata.Type;

namespace XSharpDebugger.Formatter
{
    /// <summary>
    /// This class is the main entry point into the Formatter.  The Formatter is used by the debug
    /// engine to format the result of inspection queries into strings that can be shown to the
    /// user.  See the method comments below for more details about each method.
    /// </summary>
    public sealed class XSharpFormatter : IDkmClrFormatter
    {
        /// <summary>
        /// This method is called by the debug engine to populate the text representing the type of
        /// a result.
        /// </summary>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <param name="clrType">This is the raw type we want to format</param>
        /// <param name="customTypeInfo">If Expression Compiler passed any additional information
        /// about the type that doesn't exist in metadata, this parameter contais that information.</param>
        /// <param name="formatSpecifiers">A list of custom format specifiers that the debugger did
        /// not understand.  If you want special format specifiers for your language, handle them
        /// here.  The formatter should ignore any format specifiers it does not understand.</param>
        /// <returns>The text of the type name to display</returns>
        string IDkmClrFormatter.GetTypeName(
            DkmInspectionContext inspectionContext,
            DkmClrType clrType,
            DkmClrCustomTypeInfo customTypeInfo,
            ReadOnlyCollection<string> formatSpecifiers)
        {
            // Get the LMR type for the DkmClrType.  LMR Types (Microsoft.VisualStudio.Debugger.Metadata.Type)
            // are similar to System.Type, but represent types that live in the process being debugged.
            Type lmrType = clrType.GetLmrType();

            XSharpType xType = Utility.GetXSharpTypeForLmrType(lmrType);
            if (xType == XSharpType.Invalid)
            {
                // We don't know about this type.  Delegate to the C# Formatter to format the
                // type name.
                return inspectionContext.GetTypeName(clrType, customTypeInfo, formatSpecifiers);
            }
            return xType.ToString();
        }

        /// <summary>
        /// This method is called by the debug engine to populate the text representing the value
        /// of an expression.
        /// </summary>
        /// <param name="clrValue">The raw value to get the text for</param>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <param name="formatSpecifiers"></param>
        /// <returns>The text representing the given value</returns>
        string IDkmClrFormatter.GetValueString(DkmClrValue clrValue, DkmInspectionContext inspectionContext, ReadOnlyCollection<string> formatSpecifiers)
        {
            DkmClrType clrType = clrValue.Type;
            if (clrType == null)
            {
                // This can be null in some error cases
                return string.Empty;
            }

            // Try to format the value.  If we can't format the value, delegate to the C# Formatter.
            string value = TryFormatValue(clrValue, inspectionContext);
            return value ?? clrValue.GetValueString(inspectionContext, formatSpecifiers);
        }

        /// <summary>
        /// This method is called by the debug engine to get the raw string to show in the
        /// string/xml/html visualizer.
        /// </summary>
        /// <param name="clrValue">The raw value to get the text for</param>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <returns>Raw underlying string</returns>
        string IDkmClrFormatter.GetUnderlyingString(DkmClrValue clrValue, DkmInspectionContext inspectionContext)
        {
            // Get the raw string to show in the string/xml/html visualizer.
            // The C# behavior is good enough for our purposes.
            return clrValue.GetUnderlyingString(inspectionContext);
        }

        /// <summary>
        /// This method is called by the debug engine to determine if a value has an underlying
        /// string.  If so, the debugger will show a magnifying glass icon next to the value.  The
        /// user can then use it to select a text visualizer.
        /// </summary>
        /// <param name="clrValue">The raw value to get the text for</param>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <returns></returns>
        bool IDkmClrFormatter.HasUnderlyingString(DkmClrValue clrValue, DkmInspectionContext inspectionContext)
        {
            // The C# behavior is good enough for our purposes.
            return clrValue.HasUnderlyingString(inspectionContext);
        }

        private string TryFormatValue(DkmClrValue value, DkmInspectionContext inspectionContext)
        {
            if (value.ValueFlags.HasFlag(DkmClrValueFlags.Error))
            {
                // Error message.  Just show the error.
                return value.HostObjectValue as string;
            }

            Type lmrType = value.Type.GetLmrType();
            if (lmrType.IsEnum)
            {
                return value.GetValueString(inspectionContext, null);
            }
            XSharpType xType = Utility.GetXSharpTypeForLmrType(lmrType);
            if (xType == XSharpType.Invalid)
            {
                // We don't know how to format this value
                return null;
            }
            uint radix = inspectionContext.Radix;
            object hostObjectValue = value.HostObjectValue;
            if (hostObjectValue != null)
            {
                // If the value can be marshalled into the debugger process, HostObjectValue is the
                // equivalent value in the debugger process.
                switch (System.Type.GetTypeCode(hostObjectValue.GetType()))
                {
                    case TypeCode.Int32:
                        return FormatInteger((int)hostObjectValue, radix);
                    case TypeCode.Boolean:
                        return (bool)hostObjectValue ? "TRUE" : "FALSE";
                    case TypeCode.String:
                        return FormatString(hostObjectValue.ToString(), inspectionContext.EvaluationFlags);
                }
            }

            return null;
        }

        private string FormatInteger(int value, uint radix)
        {
            return radix == 16 ? "0x" + value.ToString("X8") : value.ToString();
        }

        private string FormatString(string s, DkmEvaluationFlags flags)
        {
            if (flags.HasFlag(DkmEvaluationFlags.NoQuotes))
            {
                // No quotes - return the raw string.
                // If X# handled escaping aside from quotes, we would still want to do escaping.
                return s;
            }
            else
            {
                // Escape special characters in the string and wrap in single quotes.
                s = s.Replace("\"", "\"\"");
                return "\"" + s + "\"";
            }
        }
    }
}
