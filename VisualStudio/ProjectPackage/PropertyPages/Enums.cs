//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Globalization;

namespace XSharp.Project
{
    [Microsoft.VisualStudio.Project.PropertyPageTypeConverter(typeof(RunPostBuildEventConverter))]
    [Microsoft.VisualStudio.Shell.PropertyPageTypeConverter(typeof(RunPostBuildEventConverter))]
    public enum RunPostBuildEvent
    {
        Always,
        OnOutputUpdated,
        OnBuildSuccess
    }

    public class RunPostBuildEventConverter : EnumConverter
    {
        public RunPostBuildEventConverter()
            : base(typeof(RunPostBuildEvent))
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
                if (String.Compare(str, "Always", true) == 0) return RunPostBuildEvent.Always;
                if (String.Compare(str, "OnOutputUpdated", true) == 0) return RunPostBuildEvent.OnOutputUpdated;
                if (String.Compare(str, "OnBuildSuccess", true) == 0) return RunPostBuildEvent.OnBuildSuccess;

            }
            return RunPostBuildEvent.Always;
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                string result = null;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    result = ((RunPostBuildEvent)value).ToString();
                }
                else
                {
                    result = RunPostBuildEvent.Always.ToString();
                }

                if (result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new RunPostBuildEvent[] { RunPostBuildEvent.Always, RunPostBuildEvent.OnBuildSuccess, RunPostBuildEvent.OnOutputUpdated });
        }
    }

    [Microsoft.VisualStudio.Project.PropertyPageTypeConverter(typeof(DialectConverter))]
    [Microsoft.VisualStudio.Shell.PropertyPageTypeConverter(typeof(DialectConverter))]
    public enum Dialect
    {
        Core,
        VO,
        Vulcan,
        Harbour,
        //FoxPro,
        //dBase,
        XPP,
    }

    public class DialectConverter : EnumConverter
    {
        public DialectConverter()
            : base(typeof(Dialect))
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
                switch (str.ToLower())
                {
                    case "visual objects":
                    case "vo":
                        return Dialect.VO;
                    case "vulcan.net":
                    case "vulcan":
                        return Dialect.Vulcan;
                    case "harbour":
                        return Dialect.Harbour;
                    case "xpp":
                    case "xbase++":
                        return Dialect.XPP;
                    case "core":
                    default:
                        break;
                }
            }
            return Dialect.Core;
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                string result = null;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    switch ((Dialect)value)
                    {
                        case Dialect.Core:
                            result = "Core";
                            break;
                        case Dialect.VO:
                            result = "Visual Objects";
                            break;
                        case Dialect.Vulcan:
                            result = "Vulcan.NET";
                            break;
                        case Dialect.Harbour:
                            result = "Harbour";
                            break;
                        //case Dialect.FoxPro:
                        //    result = "FoxPro";
                        //    break;
                        //case Dialect.dBase:
                        //    result = "dBase";
                        //    break;
                        case Dialect.XPP:
                            result = "Xbase++";
                            break;
                        default:
                            result = ((Dialect)value).ToString();
                            break;
                    }
                }
                else
                {
                    result = Dialect.Core.ToString();
                }

                if (result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new Dialect[] { Dialect.Core, Dialect.VO, Dialect.Vulcan, Dialect.Harbour,Dialect.XPP});
            // Dialect.FoxPro, Dialect.dBase });
        }
    }

    [Microsoft.VisualStudio.Project.PropertyPageTypeConverter(typeof(PlatformConverter))]
    [Microsoft.VisualStudio.Shell.PropertyPageTypeConverter(typeof(PlatformConverter))]
    public enum Platform
    {
        x86,
        AnyCPU,
        x64,
        Arm,
        Itanium
    }

    public class PlatformConverter : EnumConverter
    {
        public PlatformConverter()
            : base(typeof(Platform))
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
                if (String.Compare(str, "x86", true) == 0) return Platform.x86;
                if (String.Compare(str, "anycpu", true) == 0) return Platform.AnyCPU;
                if (String.Compare(str, "x64", true) == 0) return Platform.x64;
                if (String.Compare(str, "arm", true) == 0) return Platform.Arm;
                if (String.Compare(str, "itanium", true) == 0) return Platform.Itanium;

            }
            return Platform.AnyCPU;
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                string result = null;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    result = ((Platform)value).ToString();
                }
                else
                {
                    result = Platform.AnyCPU.ToString();
                }

                if (result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new Platform[] { Platform.AnyCPU, Platform.x86, Platform.x64, Platform.Arm, Platform.Itanium });
        }
    }
    [Microsoft.VisualStudio.Project.PropertyPageTypeConverter(typeof(DebugTypeConverter))]
    [Microsoft.VisualStudio.Shell.PropertyPageTypeConverter(typeof(DebugTypeConverter))]
    public enum DebugType
    {
        none,
        full,
        pdbonly,
    }

    public class DebugTypeConverter : EnumConverter
    {
        public DebugTypeConverter()
            : base(typeof(DebugType))
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
                if (String.Compare(str, "none", true) == 0) return DebugType.none;
                if (String.Compare(str, "full", true) == 0) return DebugType.full;
                if (String.Compare(str, "pdbonly", true) == 0) return DebugType.pdbonly;

            }
            return DebugType.none;
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                string result = null;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    result = ((DebugType)value).ToString();
                }
                else
                {
                    result = DebugType.none.ToString();
                }

                if (result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override System.ComponentModel.TypeConverter.StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new DebugType[] { DebugType.none, DebugType.full, DebugType.pdbonly});
        }
    }

}

