//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel;
using System.Globalization;
using Microsoft.VisualStudio.Shell;
namespace XSharp.Project
{
    [PropertyPageTypeConverter(typeof(RunPostBuildEventConverter))]
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
            if (value is string str)
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
                string result;
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

        public override StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new RunPostBuildEvent[] { RunPostBuildEvent.Always, RunPostBuildEvent.OnBuildSuccess, RunPostBuildEvent.OnOutputUpdated });
        }
    }

    [PropertyPageTypeConverter(typeof(DialectConverter))]
    public enum Dialect
    {
        Core,
        VO,
        Vulcan,
        Harbour,
        FoxPro,
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
            if (value is string str)
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
                    case "foxpro":
                    case "fox":
                    case "vfp":
                    case "visual foxpro":
                        return Dialect.FoxPro;
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
                string result;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    switch ((Dialect)value)
                    {
                        case Dialect.Core:
                            result = XSharpProjectFileConstants.DialectCore;
                            break;
                        case Dialect.VO:
                            result = XSharpProjectFileConstants.DialectVO; 
                            break;
                        case Dialect.Vulcan:
                            result = XSharpProjectFileConstants.DialectVulcan;  
                            break;
                        case Dialect.Harbour:
                            result = XSharpProjectFileConstants.DialectHarbour;
                            break;
                        case Dialect.FoxPro:
                            result = XSharpProjectFileConstants.DialectFoxPro;
                            break;
                        //case Dialect.dBase:
                        //    result = "dBase";
                        //    break;
                        case Dialect.XPP:
                            result = XSharpProjectFileConstants.DialectXPP;
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

        public override StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new Dialect[] { Dialect.Core, Dialect.VO, Dialect.Vulcan, Dialect.Harbour,Dialect.XPP, Dialect.FoxPro});
        }
    }

    [PropertyPageTypeConverter(typeof(PlatformConverter))]
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
            if (value is string str)
            {
                if (string.Compare(str, "x86", true) == 0) return Platform.x86;
                if (string.Compare(str, "anycpu", true) == 0) return Platform.AnyCPU;
                if (string.Compare(str, "x64", true) == 0) return Platform.x64;
                if (string.Compare(str, "arm", true) == 0) return Platform.Arm;
                if (string.Compare(str, "itanium", true) == 0) return Platform.Itanium;

            }
            return Platform.AnyCPU;
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                string result;
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

        public override StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new Platform[] { Platform.AnyCPU, Platform.x86, Platform.x64, Platform.Arm, Platform.Itanium });
        }
    }
    [PropertyPageTypeConverter(typeof(DebugTypeConverter))]
    public enum DebugType
    {
        None,
        Full,
        Pdbonly,
        Portable,
        Embedded
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
            if (value is string str)
            {
                if (string.Compare(str, "none", true) == 0) return DebugType.None;
                if (string.Compare(str, "full", true) == 0) return DebugType.Full;
                if (string.Compare(str, "pdb-only", true) == 0) return DebugType.Pdbonly;
                if (string.Compare(str, "pdbonly", true) == 0) return DebugType.Pdbonly;
                if (string.Compare(str, "portable", true) == 0) return DebugType.Portable;
                if (string.Compare(str, "embedded", true) == 0) return DebugType.Embedded;

            }
            return DebugType.None;
        }

        public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
            {
                string result;
                // In some cases if multiple nodes are selected the windows form engine
                // calls us with a null value if the selected node's property values are not equal
                if (value != null)
                {
                    result = ((DebugType)value).ToString();
                }
                else
                {
                    result = DebugType.None.ToString();
                }

                if (result != null) return result;
            }

            return base.ConvertTo(context, culture, value, destinationType);
        }

        public override bool GetStandardValuesSupported(System.ComponentModel.ITypeDescriptorContext context)
        {
            return true;
        }

        public override StandardValuesCollection GetStandardValues(System.ComponentModel.ITypeDescriptorContext context)
        {
            return new StandardValuesCollection(new DebugType[] { DebugType.None, DebugType.Full, DebugType.Pdbonly, DebugType.Portable, DebugType.Embedded});
        }
    }

}

