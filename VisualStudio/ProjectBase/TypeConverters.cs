/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the Apache License, Version 2.0, please send an email to 
 * vspython@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.Versioning;
using Microsoft.VisualStudio.Shell.Interop;
using System.Runtime.InteropServices;

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
            if(sourceType == typeof(string)) return true;

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
            if(destinationType == typeof(string))
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
            if(sourceType == typeof(string)) return true;

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
            if(destinationType == typeof(string))
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

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new DebugMode[] { DebugMode.Program, DebugMode.Project, DebugMode.URL });
        }
    }

    public class BuildActionConverter : EnumConverter
    {

        public BuildActionConverter()
            : base(typeof(BuildAction))
        {

        }

        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if(sourceType == typeof(string)) return true;

            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            string str = value as string;

            if(str != null)
            {
                if(String.Compare(str, SR.GetString(SR.Compile, culture), true) == 0) return BuildAction.Compile;

                if(String.Compare(str, SR.GetString(SR.Content, culture), true) == 0) return BuildAction.Content;

                if(String.Compare(str, SR.GetString(SR.EmbeddedResource, culture), true) == 0) return BuildAction.EmbeddedResource;

                if(String.Compare(str, SR.GetString(SR.None, culture),true) == 0) return BuildAction.None;
            }

            return base.ConvertFrom(context, culture, value);
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if(destinationType == typeof(string))
            {
                string result = null;

                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                // Example of windows form engine passing us null: File set to Compile, Another file set to None, bot nodes are selected, and the build action combo is clicked.
                if(value != null)
                {
                    result = SR.GetString(((BuildAction)value).ToString(), culture);
                }
                else
                {
                    result = SR.GetString(BuildAction.None.ToString(), culture);
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
            return new StandardValuesCollection(new BuildAction[] { BuildAction.Compile, BuildAction.Content, BuildAction.EmbeddedResource, BuildAction.None });
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

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            IServiceProvider sp = ProjectNode.ServiceProvider;
            var multiTargetService = sp.GetService(typeof(SVsFrameworkMultiTargeting)) as IVsFrameworkMultiTargeting;
            if (multiTargetService == null)
            {
                Trace.TraceError("Unable to acquire the SVsFrameworkMultiTargeting service.");
                return new StandardValuesCollection(new string[0]);
            }
            Array frameworks;
            Marshal.ThrowExceptionForHR(multiTargetService.GetSupportedFrameworks(out frameworks));
            return new StandardValuesCollection(
                frameworks.Cast<string>().Select(fx => new FrameworkName(fx)).ToArray()
            );
        }
    }
}
