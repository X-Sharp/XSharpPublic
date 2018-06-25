//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Text;
using Microsoft.Build.Framework;
using Microsoft.Build.Tasks;
using Microsoft.Build.Utilities;
using Microsoft.Win32;
using System.IO;
using System.Diagnostics;
using System.Globalization;
namespace XSharp.Build
{

    internal class XSharpCommandLineBuilder : CommandLineBuilderExtension
    {
        bool fNewLine = false;
        internal XSharpCommandLineBuilder(bool fWithNewLine) : base()
        {
            fNewLine = fWithNewLine;
        }

        internal void AppendNewLine()
        {
            if (fNewLine)
                base.AppendTextUnquoted("\n");
            else
                base.AppendTextUnquoted(" ");
        }
        public new void AppendSwitch(string switchName)
        {
            if (!String.IsNullOrEmpty(switchName))
            {
                base.AppendSwitch(switchName);
                this.AppendNewLine();
            }
        }
        public void AppendSwitchIfNotNull(string switchName, string parameter, bool fNewLine = true)
        {
            if (parameter != null && switchName != null)
            {
                base.AppendSwitchIfNotNull(switchName, parameter);
                if (fNewLine)
                    this.AppendNewLine(); 
            }
        }
        public new void AppendSwitchIfNotNull(string switchName, ITaskItem parameter)
        {
            if (parameter != null && switchName != null)
            {
                base.AppendSwitchIfNotNull(switchName, parameter);
                this.AppendNewLine();
            }
        }

        /// <summary>
        /// Set a boolean switch only if its value exists.
        /// </summary>
        internal void AppendPlusOrMinusSwitch(string switchName, System.Collections.Hashtable bag, string parameterName)
        {
            object obj = bag[parameterName];
            // If the switch isn't set, don't add it to the command line.
            if (obj != null)
            {
                bool value = (bool)obj;
                // Do not quote - or + as they are part of the switch
                this.AppendSwitchUnquotedIfNotNull(switchName, (value ? "+" : "-"));
            }
        }

        /// <summary>
        /// Set a switch if its value exists by choosing from the input choices
        /// </summary>
        internal void AppendByChoiceSwitch( string switchName, System.Collections.Hashtable bag, string parameterName, string choice1,string choice2)
        {
            object obj = bag[parameterName];
            // If the switch isn't set, don't add it to the command line.
            if (obj != null)
            {
                bool value = (bool)obj;
                AppendSwitchUnquotedIfNotNull(switchName, (value ? choice1 : choice2));
                this.AppendNewLine();
            }
        }
        public new void AppendTextUnquoted(string text)
        {
            if (!String.IsNullOrEmpty(text))
            {
                base.AppendTextUnquoted(text);
                this.AppendNewLine();
            }
        }

        internal void AppendWhenTrue(string switchName,
            System.Collections.Hashtable bag, string parameterName)
        {
            object obj = bag[parameterName];
            // If the switch isn't set, don't add it to the command line.
            if (obj != null)
            {
                bool value = (bool)obj;

                if (value)
                {
                    this.AppendSwitch(switchName);
                }
            }
        }
        /// <summary>
        /// Append a switch if 'parameter' is not null.
        /// Split on the characters provided.
        /// </summary>

        internal void AppendSwitchWithSplitting(string switchName,
            string parameter, string delimiter, params char[] splitOn)
        {
            if (parameter != null)
            {
                string[] splits = parameter.Split(splitOn, /* omitEmptyEntries */ StringSplitOptions.RemoveEmptyEntries);
                string[] splitAndTrimmed = new string[splits.Length];
                for (int i = 0; i < splits.Length; ++i)
                {
                    splitAndTrimmed[i] = splits[i].Trim();
                }
                this.AppendSwitchIfNotNull(switchName, splitAndTrimmed, delimiter);
            }
        }

        /// <summary>
        /// Set an integer switch only if its value exists.
        /// </summary>
        internal void AppendSwitchWithInteger(string switchName,
            System.Collections.Hashtable bag, string parameterName)
        {
            object obj = bag[parameterName];
            // If the switch isn't set, don't add it to the command line.
            if (obj != null)
            {
                int value = (int)obj;
                this.AppendSwitchIfNotNull(switchName, value.ToString(CultureInfo.InvariantCulture));
            }
        }

        /// <summary>
        /// Adds an aliased switch, used for ResGen:
        ///      /reference:Foo=System.Xml.dll
        /// </summary>
        internal void AppendSwitchAliased(string switchName, string alias, string parameter)
        {
            this.AppendSwitchUnquotedIfNotNull(switchName, alias + "=");
            this.AppendTextWithQuoting(parameter);
            this.AppendNewLine();
        }

        /// <summary>
        /// Adds a nested switch, used by SGen.exe.  For example:
        ///     /compiler:"/keyfile:\"c:\some folder\myfile.snk\""
        /// </summary>
        internal void AppendNestedSwitch(string outerSwitchName, string innerSwitchName, string parameter)
        {
            string quotedParameter = GetQuotedText(parameter);
            this.AppendSwitchIfNotNull(outerSwitchName, innerSwitchName + quotedParameter);
        }
        /// <summary>
        /// Returns a quoted string appropriate for appending to a command line.
        /// </summary>
        /// <remarks>
        /// Escapes any double quotes in the string.
        /// </remarks>
        protected new string GetQuotedText(string unquotedText)
        {
            StringBuilder quotedText = new StringBuilder();

            AppendQuotedTextToBuffer(quotedText, unquotedText);

            return quotedText.ToString();
        }

        public new void AppendSwitchUnquotedIfNotNull(string switchName, string parameter)
        {
            base.AppendSwitchUnquotedIfNotNull(switchName, parameter);
            this.AppendNewLine();
        }
        public new void AppendSwitchIfNotNull(string switchName, string[] parameters, string delimiter)
        {
            base.AppendSwitchIfNotNull(switchName, parameters, delimiter);
            this.AppendNewLine();
        }

        /// <summary>
        /// Appends a command-line switch that takes a compound string parameter. The parameter is built up from the item-spec and
        /// the specified attributes. The switch is appended as many times as there are parameters given.
        /// </summary>
        internal void AppendSwitchIfNotNull(string switchName, ITaskItem[] parameters, string[] attributes)
        {
            this.AppendSwitchIfNotNull(switchName, parameters, attributes, null /* treatAsFlag */);
            this.AppendNewLine();
        }

        /// <summary>
        /// Designed to handle the /link and /embed switches:
        ///
        ///      /embed[resource]:&lt;filename>[,&lt;name>[,Private]]
        ///      /link[resource]:&lt;filename>[,&lt;name>[,Private]]
        ///
        /// Where the last flag--Private--is either present or not present
        /// depending on whether the ITaskItem has a Private="True" attribute.
        /// </summary>
        public void AppendSwitchIfNotNull(string switchName,
            ITaskItem[] parameters, string[] metadataNames, bool[] treatAsFlags)      // May be null. In this case no metadata are treated as flags.

        {
            Debug.Assert(treatAsFlags == null
                         || (metadataNames.Length == treatAsFlags.Length),
                         "metadataNames and treatAsFlags should have the same length.");

            if (parameters != null)
            {
                foreach (ITaskItem parameter in parameters)
                {
                    // do not add a newline after the ItemSpec
                    this.AppendSwitchIfNotNull(switchName, parameter.ItemSpec,false);
                    if (metadataNames != null)
                    {
                        for (int i = 0; i < metadataNames.Length; ++i)
                        {
                            string metadataValue = parameter.GetMetadata(metadataNames[i]);

                            if ((metadataValue != null) && (metadataValue.Length > 0))
                            {
                                // Treat attribute as a boolean flag?
                                if (treatAsFlags == null || treatAsFlags[i] == false)
                                {
                                    // Not a boolean flag.
                                    CommandLine.Append(',');
                                    this.AppendTextWithQuoting(metadataValue);
                                }
                                else
                                {
                                    // A boolean flag.
                                    bool flagSet = false;

                                    flagSet = Utilities.TryConvertItemMetadataToBool(parameter, metadataNames[i]);

                                    if (flagSet)
                                    {
                                        CommandLine.Append(',');
                                        this.AppendTextWithQuoting(metadataNames[i]);
                                    }
                                }
                            }
                            else
                            {
                                if (treatAsFlags == null || treatAsFlags[i] == false)
                                {
                                    // If the caller of this method asked us to add metadata
                                    // A, B, and C, and metadata A doesn't exist on the item,
                                    // then it doesn't make sense to check for B and C.  Because
                                    // since these metadata are just being appended on the
                                    // command-line switch with comma-separation, you can't pass
                                    // in the B metadata unless you've also passed in the A
                                    // metadata.  Otherwise the tool's command-line parser will
                                    // get totally confused.

                                    // This only applies to non-flag attributes.
                                    break;
                                }
                            }
                        }
                        this.AppendNewLine(); 
                    }
                }
            }

            
        }
    }

}