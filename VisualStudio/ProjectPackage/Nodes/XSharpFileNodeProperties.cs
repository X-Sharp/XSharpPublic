//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Globalization;

using Microsoft.VisualStudio.Project;


namespace XSharp.Project
{

    public enum XSharpBuildAction
    {
        None,
        Compile,
        Content,
        EmbeddedResource,
        NativeResource,
        VOBinary,
        ApplicationDefinition,
        Page,
        Resource
    };

    [ComVisible(true)]
    [CLSCompliant(false)]
    [Guid(XSharpConstants.FileNodePropertiesGuidString)]
    public class XSharpFileNodeProperties : SingleFileGeneratorNodeProperties
    {
        [PropertyPageTypeConverter(typeof(CopyToOutputConverter))]
        public enum CopyToOutput
        {
            CopyNever,
            CopyAlways,
            CopyPreserveNewest
        };
        public XSharpFileNodeProperties(HierarchyNode node)
            : base(node)
        {
        }

        public new string Extension
        {
            get
            {
                //return ".Designer.prg";
                return Path.GetExtension(Node.Url);
            }
        }

        [Browsable(false)]
        public string Url
        {
            get { return "file:///" + Node.Url; }
        }

        // 
       [Browsable(false)]
        [SRCategoryAttribute(SR.Advanced)]
        [LocDisplayName("SubType")]
        [SRDescriptionAttribute("SubTypeDescription")]
        public string SubType
        {
            get
            {
                return ((XSharpFileNode)Node).SubType;
            }

            set
            {
                ((XSharpFileNode)Node).SubType = value;
            }
        }

        [SRCategoryAttribute(Microsoft.VisualStudio.Project.SR.Advanced)]
        [LocDisplayName(Microsoft.VisualStudio.Project.SR.BuildAction)]
        [SRDescriptionAttribute(Microsoft.VisualStudio.Project.SR.BuildActionDescription)]
        public virtual XSharpBuildAction XSharpBuildAction
        {
            get
            {
                string value = Node.ItemNode.ItemName;
                if (string.IsNullOrEmpty(value))
                    return XSharpBuildAction.None;
                return (XSharpBuildAction)Enum.Parse(typeof(XSharpBuildAction), value);
            }

            set
            {
                Node.ItemNode.ItemName = value.ToString();
            }
        }

        [Browsable(false)]
        public bool IsDependent { get; set; }



        [Browsable(false)]
        public string ItemType
        {
            get
            {
                return XSharpBuildAction.ToString();
            }

            set
            {
                this.Node.ItemNode.ItemName = value;
            }

        }

        [Browsable(false)]
        public override BuildActionEnum BuildAction
        {
            get
            {
                switch (XSharpBuildAction)
                {
                    case XSharpBuildAction.ApplicationDefinition:
                    case XSharpBuildAction.Page:
                    case XSharpBuildAction.Resource:
                        return BuildActionEnum.Compile;
                    case XSharpBuildAction.EmbeddedResource:
                        return BuildActionEnum.EmbeddedResource;
                    default:
                        var xnode = this.Node as XSharpFileNode;
                        if (xnode.FileType == XSharpModel.XFileType.License)
                            return BuildActionEnum.EmbeddedResource; 
                        var action = BuildActionEnum.None;
                        if (Enum.TryParse<BuildActionEnum>(XSharpBuildAction.ToString(), out action))
                            return action;
                        return BuildActionEnum.None;
                }

            }

            set
            {
                try
                {
                    var action = XSharpBuildAction.None;
                    if (Enum.TryParse<XSharpBuildAction>(value.ToString(), out action))
                        this.XSharpBuildAction = action;
                    this.XSharpBuildAction = XSharpBuildAction.None;
                }
                catch (Exception)
                {
                    this.XSharpBuildAction = XSharpBuildAction.None;
                }
            }
        }

         public class CopyToOutputConverter : EnumConverter
        {

            public CopyToOutputConverter()
               : base(typeof(CopyToOutput))
            {
            }
            const string CopyNever = "Do not copy";
            const string PreserveNewest = "Copy if newer";
            const string CopyAlways = "Copy always";
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
                    if (String.Equals(str,CopyNever,StringComparison.OrdinalIgnoreCase))
                        return CopyToOutput.CopyNever;
                    if (String.Equals(str, CopyAlways, StringComparison.OrdinalIgnoreCase))
                        return CopyToOutput.CopyAlways;
                    if (String.Equals(str, PreserveNewest, StringComparison.OrdinalIgnoreCase))
                        return CopyToOutput.CopyPreserveNewest;
                }

                return base.ConvertFrom(context, culture, value);
            }

            public override object ConvertTo(ITypeDescriptorContext context, CultureInfo culture, object value, Type destinationType)
            {
                if (destinationType == typeof(string))
                {
                    string result = null;

                    // In some cases if multiple nodes are selected the windows form engine
                    // calls us with a null value if the selected node's property values are not equal
                    // Example of windows form engine passing us null: File set to Compile, Another file set to None, bot nodes are selected, and the build action combo is clicked.
                    if (value != null)
                    {
                        CopyToOutput iValue = (CopyToOutput)value;
                        switch (iValue)
                        {
                            case CopyToOutput.CopyNever:
                                result = CopyNever;
                                break;
                            case CopyToOutput.CopyAlways:
                                result = CopyAlways;
                                break;
                            case CopyToOutput.CopyPreserveNewest:
                                result = PreserveNewest;
                                break;
                            default:
                                result = CopyNever;
                                break;
                        }
                    }
                    else
                    {
                        result = CopyNever;
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
                return new StandardValuesCollection(new CopyToOutput[]
                   { CopyToOutput.CopyNever,
              CopyToOutput.CopyAlways,
              CopyToOutput.CopyPreserveNewest });
            }
        }


    }
}