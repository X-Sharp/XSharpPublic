using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Build
{
    // Microsoft.Build.Tasks.CommandLineBuilderExtension
    using Microsoft.Build.Framework;
    using Microsoft.Build.Shared;
    using Microsoft.Build.Utilities;
    using System;
    using System.Collections;
    using System.Globalization;
    using System.Text;

    /// <summary>Comprises extended utility methods for constructing a command line.</summary>
    public class CommandLineBuilderExtension : CommandLineBuilder
    {
        internal virtual void AppendWhenTrue(string switchName, BuildPropertyCollection bag, string parameterName)
        {
            object obj = bag[parameterName];
            if (obj != null && (bool)obj)
            {
                AppendSwitch(switchName);
            }
        }

        internal virtual void AppendPlusOrMinusSwitch(string switchName, BuildPropertyCollection bag, string parameterName)
        {
            object obj = bag[parameterName];
            if (obj != null)
            {
                bool flag = (bool)obj;
                AppendSwitchUnquotedIfNotNull(switchName, flag ? "+" : "-");
            }
        }


        internal virtual void AppendSwitchWithInteger(string switchName, BuildPropertyCollection bag, string parameterName)
        {
            object obj = bag[parameterName];
            if (obj != null)
            {
                AppendSwitchIfNotNull(switchName, ((int)obj).ToString(CultureInfo.InvariantCulture));
            }
        }


        /// <summary>Returns a quoted string appropriate for appending to a command line.</summary>
        /// <returns>A string representing the new quoted string.</returns>
        /// <param name="unquotedText">The string to convert.</param>
        protected string GetQuotedText(string unquotedText)
        {
            StringBuilder stringBuilder = new StringBuilder();
            AppendQuotedTextToBuffer(stringBuilder, unquotedText);
            return stringBuilder.ToString();
        }

        public virtual void AppendSwitchIfNotNull(string switchName, ITaskItem[] parameters, string[] attributes)
        {
            AppendSwitchIfNotNull(switchName, parameters, attributes, null);
        }


        internal static bool IsParameterEmpty(string parameter, params char[] splitOn)
        {
            if (parameter != null)
            {
                string[] array = parameter.Split(splitOn, StringSplitOptions.RemoveEmptyEntries);
                for (int i = 0; i < array.Length; i++)
                {
                    if (!string.IsNullOrEmpty(array[i].Trim()))
                    {
                        return false;
                    }
                }
            }
            return true;
        }

        public virtual void AppendSwitchIfNotNull(string switchName, ITaskItem[] parameters, string[] metadataNames, bool[] treatAsFlags)
        {
            Microsoft.Build.Shared.ErrorUtilities.VerifyThrow(treatAsFlags == null || metadataNames.Length == treatAsFlags.Length, "metadataNames and treatAsFlags should have the same length.");
            if (parameters == null)
            {
                return;
            }
            foreach (ITaskItem taskItem in parameters)
            {
                AppendSwitchIfNotNull(switchName, taskItem.ItemSpec);
                if (metadataNames == null)
                {
                    continue;
                }
                for (int j = 0; j < metadataNames.Length; j++)
                {
                    string metadata = taskItem.GetMetadata(metadataNames[j]);
                    if (metadata != null && metadata.Length > 0)
                    {
                        if (treatAsFlags == null || !treatAsFlags[j])
                        {
                            base.CommandLine.Append(',');
                            AppendTextWithQuoting(metadata);
                        }
                        else if (MetadataConversionUtilities.TryConvertItemMetadataToBool(taskItem, metadataNames[j]))
                        {
                            base.CommandLine.Append(',');
                            AppendTextWithQuoting(metadataNames[j]);
                        }
                    }
                    else if (treatAsFlags == null || !treatAsFlags[j])
                    {
                        break;
                    }
                }
            }
        }
    }

}
