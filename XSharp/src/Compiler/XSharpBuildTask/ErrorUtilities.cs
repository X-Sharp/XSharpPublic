//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using Microsoft.Build.Framework;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Threading;

namespace Microsoft.Build.Shared
{

    internal static class ErrorUtilities
    {
        private static readonly bool throwExceptions = string.IsNullOrEmpty(Environment.GetEnvironmentVariable("MSBUILDDONOTTHROWINTERNAL"));

#if NOTUSED
        private static readonly bool enableMSBuildDebugTracing = !string.IsNullOrEmpty(Environment.GetEnvironmentVariable("MSBUILDENABLEDEBUGTRACING"));
        public static void DebugTraceMessage(string category, string formatstring, params object[] parameters)
        {
            if (enableMSBuildDebugTracing)
            {
                if (parameters != null)
                {
                    Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, formatstring, parameters), category);
                }
                else
                {
                    Trace.WriteLine(formatstring, category);
                }
            }
        }
#endif
        internal static void ThrowInternalError(string message, params object[] args)
        {
            if (throwExceptions)
            {
                throw new Microsoft.Build.Shared.InternalErrorException(Microsoft.Build.Shared.ResourceUtilities.FormatString(message, args));
            }
        }
#if NOTUSED
        internal static void ThrowInternalError(string message, Exception innerException, params object[] args)
        {
            if (throwExceptions)
            {
                throw new Microsoft.Build.Shared.InternalErrorException(Microsoft.Build.Shared.ResourceUtilities.FormatString(message, args), innerException);
            }
        }


        internal static void ThrowInternalErrorUnreachable()
        {
            if (throwExceptions)
            {
                throw new Microsoft.Build.Shared.InternalErrorException("Unreachable?");
            }
        }

        internal static void ThrowIfTypeDoesNotImplementToString(object param)
        {
        }

        internal static void VerifyThrowInternalNull(object parameter, string parameterName)
        {
            if (parameter == null)
            {
                ThrowInternalError("{0} unexpectedly null", parameterName);
            }
        }


        internal static void VerifyThrowInternalLockHeld(object locker)
        {
            if (!Monitor.IsEntered(locker))
            {
                ThrowInternalError("Lock should already have been taken");
            }
        }

        internal static void VerifyThrowInternalLength(string parameterValue, string parameterName)
        {
            VerifyThrowInternalNull(parameterValue, parameterName);
            if (parameterValue.Length == 0)
            {
                ThrowInternalError("{0} unexpectedly empty", parameterName);
            }
        }

        internal static void VerifyThrowInternalRooted(string value)
        {
            if (!Path.IsPathRooted(value))
            {
                ThrowInternalError("{0} unexpectedly not a rooted path", value);
            }
        }

        internal static void VerifyThrow(bool condition, string unformattedMessage)
        {
            if (!condition)
            {
                ThrowInternalError(unformattedMessage, null, null);
            }
        }
#endif
        internal static void VerifyThrow(bool condition, string unformattedMessage, object arg0)
        {
            if (!condition)
            {
                ThrowInternalError(unformattedMessage, arg0);
            }
        }
#if NOTUSED

        internal static void VerifyThrow(bool condition, string unformattedMessage, object arg0, object arg1)
        {
            if (!condition)
            {
                ThrowInternalError(unformattedMessage, arg0, arg1);
            }
        }

        internal static void VerifyThrow(bool condition, string unformattedMessage, object arg0, object arg1, object arg2)
        {
            if (!condition)
            {
                ThrowInternalError(unformattedMessage, arg0, arg1, arg2);
            }
        }

        internal static void VerifyThrow(bool condition, string unformattedMessage, object arg0, object arg1, object arg2, object arg3)
        {
            if (!condition)
            {
                ThrowInternalError(unformattedMessage, arg0, arg1, arg2, arg3);
            }
        }

        internal static void ThrowInvalidOperation(string resourceName, params object[] args)
        {
            if (throwExceptions)
            {
                throw new InvalidOperationException(Microsoft.Build.Shared.ResourceUtilities.FormatResourceString(resourceName, args));
            }
        }

        internal static void VerifyThrowInvalidOperation(bool condition, string resourceName)
        {
            if (!condition)
            {
                ThrowInvalidOperation(resourceName, null);
            }
        }

        internal static void VerifyThrowInvalidOperation(bool condition, string resourceName, object arg0)
        {
            if (!condition)
            {
                ThrowInvalidOperation(resourceName, arg0);
            }
        }

        internal static void VerifyThrowInvalidOperation(bool condition, string resourceName, object arg0, object arg1)
        {
            if (!condition)
            {
                ThrowInvalidOperation(resourceName, arg0, arg1);
            }
        }

        internal static void VerifyThrowInvalidOperation(bool condition, string resourceName, object arg0, object arg1, object arg2)
        {
            if (!condition)
            {
                ThrowInvalidOperation(resourceName, arg0, arg1, arg2);
            }
        }

        internal static void ThrowArgument(string resourceName, params object[] args)
        {
            ThrowArgument(null, resourceName, args);
        }

        private static void ThrowArgument(Exception innerException, string resourceName, params object[] args)
        {
            if (throwExceptions)
            {
                throw new ArgumentException(nameof(args), innerException);
            }
        }

        internal static void VerifyThrowArgument(bool condition, string resourceName)
        {
            VerifyThrowArgument(condition, null, resourceName);
        }
        internal static void VerifyThrowArgument(bool condition, string resourceName, object arg0)
        {
            VerifyThrowArgument(condition, null, resourceName, arg0);
        }

        internal static void VerifyThrowArgument(bool condition, string resourceName, object arg0, object arg1)
        {
            VerifyThrowArgument(condition, null, resourceName, arg0, arg1);
        }

        internal static void VerifyThrowArgument(bool condition, string resourceName, object arg0, object arg1, object arg2)
        {
            VerifyThrowArgument(condition, null, resourceName, arg0, arg1, arg2);
        }

        internal static void VerifyThrowArgument(bool condition, string resourceName, object arg0, object arg1, object arg2, object arg3)
        {
            VerifyThrowArgument(condition, null, resourceName, arg0, arg1, arg2, arg3);
        }

        internal static void VerifyThrowArgument(bool condition, Exception innerException, string resourceName)
        {
            if (!condition)
            {
                ThrowArgument(innerException, resourceName, null);
            }
        }

        internal static void VerifyThrowArgument(bool condition, Exception innerException, string resourceName, object arg0)
        {
            if (!condition)
            {
                ThrowArgument(innerException, resourceName, arg0);
            }
        }

        internal static void VerifyThrowArgument(bool condition, Exception innerException, string resourceName, object arg0, object arg1)
        {
            if (!condition)
            {
                ThrowArgument(innerException, resourceName, arg0, arg1);
            }
        }

        internal static void VerifyThrowArgument(bool condition, Exception innerException, string resourceName, object arg0, object arg1, object arg2)
        {
            if (!condition)
            {
                ThrowArgument(innerException, resourceName, arg0, arg1, arg2);
            }
        }

        internal static void VerifyThrowArgument(bool condition, Exception innerException, string resourceName, object arg0, object arg1, object arg2, object arg3)
        {
            if (!condition)
            {
                ThrowArgument(innerException, resourceName, arg0, arg1, arg2, arg3);
            }
        }

        internal static void ThrowArgumentOutOfRange(string parameterName)
        {
            if (throwExceptions)
            {
                throw new ArgumentOutOfRangeException(parameterName);
            }
        }

        internal static void VerifyThrowArgumentOutOfRange(bool condition, string parameterName)
        {
            if (!condition)
            {
                ThrowArgumentOutOfRange(parameterName);
            }
        }

        internal static void VerifyThrowArgumentLength(string parameter, string parameterName)
        {
            VerifyThrowArgumentNull(parameter, parameterName);
            if (parameter.Length == 0 && throwExceptions)
            {
                throw new ArgumentException(Microsoft.Build.Shared.ResourceUtilities.FormatResourceString("Shared.ParameterCannotHaveZeroLength", parameterName));
            }
        }

        internal static void VerifyThrowArgumentLengthIfNotNull(string parameter, string parameterName)
        {
            if (parameter != null && parameter.Length == 0 && throwExceptions)
            {
                throw new ArgumentException(Microsoft.Build.Shared.ResourceUtilities.FormatResourceString("Shared.ParameterCannotHaveZeroLength", parameterName));
            }
        }
#endif
        internal static void VerifyThrowArgumentNull(object parameter, string parameterName)
        {
            VerifyThrowArgumentNull(parameter, parameterName, "Shared.ParameterCannotBeNull");
        }

        internal static void VerifyThrowArgumentNull(object parameter, string parameterName, string resourceName)
        {
            if (parameter == null && throwExceptions)
            {
                throw new ArgumentNullException(nameof(parameterName));
            }
        }
#if NOTUSED
        internal static void VerifyThrowArgumentArraysSameLength(Array parameter1, Array parameter2, string parameter1Name, string parameter2Name)
        {
            VerifyThrowArgumentNull(parameter1, parameter1Name);
            VerifyThrowArgumentNull(parameter2, parameter2Name);
            if (parameter1.Length != parameter2.Length && throwExceptions)
            {
                throw new ArgumentException(Microsoft.Build.Shared.ResourceUtilities.FormatResourceString("Shared.ParametersMustHaveTheSameLength", parameter1Name, parameter2Name));
            }
        }
#endif
    }

}
namespace Microsoft.Build.Shared
{
    // Microsoft.Build.Shared.InternalErrorException
    using System;
    using System.Diagnostics;
    using System.Globalization;
    using System.Reflection;
    using System.Resources;
    using System.Runtime.Serialization;

    [Serializable]
    internal sealed class InternalErrorException : Exception
    {
        internal InternalErrorException()
        {
        }

        internal InternalErrorException(string message)
            : base("MSB0001: Internal MSBuild Error: " + message)
        {
            ConsiderDebuggerLaunch(message, null);
        }
#if NOTUSED
        internal InternalErrorException(string message, Exception innerException)
            : base("MSB0001: Internal MSBuild Error: " + message + ((innerException == null) ? string.Empty : ("\n=============\n" + innerException.ToString() + "\n\n")), innerException)
        {
            ConsiderDebuggerLaunch(message, innerException);
        }

        private InternalErrorException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }
#endif
        private static void ConsiderDebuggerLaunch(string message, Exception innerException)
        {
            if (innerException != null)
            {
                innerException.ToString();
            }
            else
            {
                string _ = string.Empty;
            }
            if (Environment.GetEnvironmentVariable("MSBUILDLAUNCHDEBUGGER") != null)
            {
                Debugger.Launch();
            }
        }
    }
    internal static class ResourceUtilities
    {
#if NOTUSED
        internal static string ExtractMessageCode(bool msbuildCodeOnly, string message, out string code)
        {
            Microsoft.Build.Shared.ErrorUtilities.VerifyThrowInternalNull(message, "message");
            code = null;
            int i;
            for (i = 0; i < message.Length && char.IsWhiteSpace(message[i]); i++)
            {
            }
            if (msbuildCodeOnly)
            {
                if (message.Length < i + 8 || message[i] != 'M' || message[i + 1] != 'S' || message[i + 2] != 'B' || message[i + 3] < '0' || message[i + 3] > '9' || message[i + 4] < '0' || message[i + 4] > '9' || message[i + 5] < '0' || message[i + 5] > '9' || message[i + 6] < '0' || message[i + 6] > '9' || message[i + 7] != ':')
                {
                    return message;
                }
                code = message.Substring(i, 7);
                i += 8;
            }
            else
            {
                int j;
                for (j = i; j < message.Length; j++)
                {
                    char c = message[j];
                    if ((c < 'a' || c > 'z') && (c < 'A' || c > 'Z'))
                    {
                        break;
                    }
                }
                if (j == i)
                {
                    return message;
                }
                int k;
                for (k = j; k < message.Length; k++)
                {
                    char c2 = message[k];
                    if (c2 < '0' || c2 > '9')
                    {
                        break;
                    }
                }
                if (k == j)
                {
                    return message;
                }
                if (k == message.Length || message[k] != ':')
                {
                    return message;
                }
                code = message.Substring(i, k - i);
                i = k + 1;
            }
            for (; i < message.Length && char.IsWhiteSpace(message[i]); i++)
            {
            }
            if (i < message.Length)
            {
                message = message.Substring(i, message.Length - i);
            }
            return message;
        }

        private static string GetHelpKeyword(string resourceName)
        {
            return "MSBuild." + resourceName;
        }

        internal static string GetResourceString(string resourceName)
        {
            return Microsoft.Build.Shared.AssemblyResources.GetString(resourceName);
        }

        internal static string FormatResourceString(out string code, out string helpKeyword, string resourceName, params object[] args)
        {
            helpKeyword = GetHelpKeyword(resourceName);
            return ExtractMessageCode(true, FormatString(GetResourceString(resourceName), args), out code);
        }

        internal static string FormatResourceString(string resourceName, params object[] args)
        {
            string code;
            string helpKeyword;
            return FormatResourceString(out code, out helpKeyword, resourceName, args);
        }
#endif
        internal static string FormatString(string unformatted, params object[] args)
        {
            string result = unformatted;
            if (args != null && args.Length != 0)
            {
                result = string.Format(CultureInfo.CurrentCulture, unformatted, args);
            }
            return result;
        }
#if NOTUSED
        internal static void VerifyResourceStringExists(string resourceName)
        {
        }
#endif
    }

    internal static class AssemblyResources
    {
        private static readonly ResourceManager resources = new ResourceManager("Microsoft.Build.Tasks.Strings", Assembly.GetExecutingAssembly());

        private static readonly ResourceManager sharedResources = new ResourceManager("Microsoft.Build.Tasks.Strings.shared", Assembly.GetExecutingAssembly());

        //internal static ResourceManager PrimaryResources => resources;

        //internal static ResourceManager SharedResources => sharedResources;

        internal static string GetString(string name)
        {
            string @string = resources.GetString(name, CultureInfo.CurrentUICulture);
            if (@string == null)
            {
                @string = sharedResources.GetString(name, CultureInfo.CurrentUICulture);
            }
            Microsoft.Build.Shared.ErrorUtilities.VerifyThrow(@string != null, "Missing resource '{0}'", name);
            return @string;
        }
    }
}
