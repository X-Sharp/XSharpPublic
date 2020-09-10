//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Text;
using Microsoft.Build.Framework;
using System.Diagnostics;
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

        public new void AppendSwitchIfNotNull(string switchName, string parameter)
        {
            AppendSwitchIfNotNull(switchName, parameter, true);
        }
        public void AppendSwitchIfNotNull(string switchName, string parameter, bool fNewLine)
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

        internal override void AppendWhenTrue(string switchName,PropertyDictionary bag, string parameterName)
        {
            if (bag[parameterName] != null)
            {
                base.AppendWhenTrue(switchName, bag, parameterName);
                this.AppendNewLine();
            }
        }

        internal override void AppendSwitchWithInteger(string switchName, PropertyDictionary bag, string parameterName)
        {
            if (bag[parameterName] != null)
            {
                base.AppendSwitchWithInteger(switchName, bag, parameterName);
                this.AppendNewLine();
            }
        }
        internal override void AppendPlusOrMinusSwitch(string switchName, PropertyDictionary bag, string parameterName)
        {
            // Overridden so we can add a NewLine when needed
            if (bag[parameterName] != null)
            {
                base.AppendPlusOrMinusSwitch(switchName, bag, parameterName);
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

        /// <summary>
        /// Adds an aliased switch, used for ResGen:
        ///      /reference:Foo=System.Xml.dll
        /// </summary>
        internal override void AppendSwitchAliased(string switchName, string alias, string parameter)
        {
            // Overridden so we can add a NewLine when needed
            if (switchName != null)
            {
                base.AppendSwitchAliased(switchName, alias, parameter);
                this.AppendNewLine();
            }
        }

        public new void AppendSwitchUnquotedIfNotNull(string switchName, string parameter)
        {
            if (parameter != null)
            {
                base.AppendSwitchUnquotedIfNotNull(switchName, parameter);
                this.AppendNewLine();
            }
        }
        public new void AppendSwitchIfNotNull(string switchName, string[] parameters, string delimiter)
        {
            if (parameters != null)
            {
                base.AppendSwitchIfNotNull(switchName, parameters, delimiter);
                this.AppendNewLine();
            }
        }

        internal override void AppendSwitchIfNotNull(string switchName, ITaskItem[] parameters, string[] attributes)
        {
            if (parameters != null)
            {
                base.AppendSwitchIfNotNull(switchName, parameters, attributes);
                this.AppendNewLine();
            }
        }

        internal override void AppendSwitchIfNotNull(string switchName,
            ITaskItem[] parameters, string[] metadataNames, bool[] treatAsFlags)      // May be null. In this case no metadata are treated as flags.
        {
            if (parameters != null)
            {
                base.AppendSwitchIfNotNull(switchName, parameters, metadataNames, treatAsFlags);
                this.AppendNewLine();

            }
        }
    }
}
