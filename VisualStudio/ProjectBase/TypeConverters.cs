/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution. 
 * 
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.Versioning;
using Microsoft.VisualStudio.Shell.Interop;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project
{
    public class OutputTypeConverter : EnumConverter
    {
        public OutputTypeConverter()
            : base(typeof(OutputType))
        {

        }

        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType.IsEquivalentTo(typeof(string))) return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            string str = value as string;

            if(str != null)
            {
                if(String.Compare(str,SR.GetString(SR.Exe, culture), true) == 0) return OutputType.Exe;
                if(String.Compare(str, SR.GetString(SR.Library, culture), true) == 0) return OutputType.Library;
                if(String.Compare(str, SR.GetString(SR.WinExe, culture), true) == 0) return OutputType.WinExe;
                if (String.Compare(str, SR.GetString(SR.WinMDObj, culture), true) == 0) return OutputType.WinMDObj;
                if (String.Compare(str, SR.GetString(SR.AppContainerExe, culture),true) == 0) return OutputType.AppContainerExe;
            }

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType.IsEquivalentTo(typeof(string)))
            {
                string result = null;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if(value != null)
                {
                    result = SR.GetString(((OutputType)value).ToString(), culture);
                }
                else
                {
                    result = SR.GetString(OutputType.Library.ToString(), culture);
                }

                if(result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new OutputType[] { OutputType.Exe, OutputType.Library, OutputType.WinExe, OutputType.AppContainerExe, OutputType.WinMDObj});
        }
    }

    public class DebugModeConverter : EnumConverter
    {

        public DebugModeConverter()
            : base(typeof(DebugMode))
        {

        }
        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType.IsEquivalentTo(typeof(string))) return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            string str = value as string;

            if(str != null)
            {
                if (String.Compare(str ,SR.GetString(SR.Program, culture),true) == 0) return DebugMode.Program;

                if(String.Compare(str, SR.GetString(SR.Project, culture), true) == 0) return DebugMode.Project;

                if(String.Compare(str, SR.GetString(SR.URL, culture), true) == 0) return DebugMode.URL;
            }

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType.IsEquivalentTo(typeof(string)))
            {
                string result = null;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if(value != null)
                {
                    result = SR.GetString(((DebugMode)value).ToString(), culture);
                }
                else
                {
                    result = SR.GetString(DebugMode.Program.ToString(), culture);
                }

                if(result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }
        public override bool GetStandardValuesSupported(ITypeDescriptorContext context) {
            return true;
        }

        public override TypeConverter.StandardValuesCollection GetStandardValues(ITypeDescriptorContext context) {
            return new StandardValuesCollection(new DebugMode[] { DebugMode.Program, DebugMode.Project, DebugMode.URL });
        }
    }

    public class BuildActionConverter : TypeConverter
    {
        List<BuildAction> buildActions = new List<BuildAction>();

        public BuildActionConverter()
        {
            ResetBuildActionsToDefaults();
        }

        public void ResetBuildActionsToDefaults()
        {
            this.buildActions.Clear();
            this.buildActions.Add(BuildAction.None);
            this.buildActions.Add(BuildAction.Compile);
            this.buildActions.Add(BuildAction.Content);
            this.buildActions.Add(BuildAction.EmbeddedResource);
        }

        public void RegisterBuildAction(BuildAction buildAction)
        {
            if (!this.buildActions.Contains(buildAction))
            {
                this.buildActions.Add(buildAction);
            }
        }

        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType.IsEquivalentTo(typeof(string))) return true;
            return false;
        }

        public override bool CanConvertTo(ITypeDescriptorContext context, Type sourceType)
        {
            return sourceType.IsEquivalentTo(typeof(string));
        }


        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            string s = value as string;
            if (s != null) return new BuildAction(s);
            //SR.Compile;
            //SR.EmbeddedResource;
            //SR.None;
            //SR.Content;
            //SR.Page;
            //SR.ApplicationDefinition;
            //SR.Resource;

            return null;
        }


        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType.IsEquivalentTo(typeof(string)))
            {
                return ((BuildAction)value).Name;
            }

            return null;
        }

        public override bool GetStandardValuesSupported(ITypeDescriptorContext context)
        {
            return true;
        }

        public override TypeConverter.StandardValuesCollection GetStandardValues(ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(buildActions);
        }
    }

    public class CopyToOutputDirectoryConverter : EnumConverter
    {

        public CopyToOutputDirectoryConverter()
            : base(typeof(CopyToOutputDirectory))
        {

        }

        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType.IsEquivalentTo(typeof(string))) return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            string str = value as string;

            if (str != null)
            {
                if (str == SR.GetString(SR.CopyAlways, culture)) return CopyToOutputDirectory.Always;

                if (str == SR.GetString(SR.CopyIfNewer, culture)) return CopyToOutputDirectory.PreserveNewest;

                if (str == SR.GetString(SR.DoNotCopy, culture)) return CopyToOutputDirectory.DoNotCopy;
            }
            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType.IsEquivalentTo(typeof(string)))
            {
                string result = null;

                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    if (((CopyToOutputDirectory)value) == CopyToOutputDirectory.DoNotCopy)
                        result = SR.GetString(SR.DoNotCopy, culture);
                    if (((CopyToOutputDirectory)value) == CopyToOutputDirectory.Always)
                        result = SR.GetString(SR.CopyAlways, culture);
                    if (((CopyToOutputDirectory)value) == CopyToOutputDirectory.PreserveNewest)
                        result = SR.GetString(SR.CopyIfNewer, culture);
                }
                else
                {
                    result = "";
                }

                if(result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(ITypeDescriptorContext context)
        {
            return true;
        }

        public override TypeConverter.StandardValuesCollection GetStandardValues(ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new CopyToOutputDirectory[] { CopyToOutputDirectory.Always, CopyToOutputDirectory.DoNotCopy, CopyToOutputDirectory.PreserveNewest });
        }
    }

    public class FrameworkNameConverter : TypeConverter
    {
        public FrameworkNameConverter()
        {
        }

        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType == typeof(string)) return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            string str = value as string;

            if (str != null)
            {
                return new FrameworkName(str);
            }

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                var name = value as FrameworkName;
                if (name != null)
                {
                    return name.FullName;
                }
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(ITypeDescriptorContext context)
        {
            return true;
        }

        public override TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            IServiceProvider sp = ProjectNode.ServiceProvider;
            ThreadHelper.ThrowIfNotOnUIThread();

            var multiTargetService = sp.GetService(typeof(SVsFrameworkMultiTargeting)) as IVsFrameworkMultiTargeting;
            if (multiTargetService == null)
            {
                Trace.TraceError("Unable to acquire the SVsFrameworkMultiTargeting service.");
                return new StandardValuesCollection(new string[0]);
            }
            Array frameworks;
            Marshal.ThrowExceptionForHR(multiTargetService.GetSupportedFrameworks(out frameworks));
            var result = new List<string>();
            foreach (string name in frameworks)
            {
                if (name.ToLower().IndexOf("profile") == -1 && name.StartsWith(".NETFramework"))
                {
                    result.Add(name);
                }

            }
            return new StandardValuesCollection(result.Select(x => new FrameworkName(x)).ToArray());
        }
    }
}
