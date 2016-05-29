//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.ComponentModel;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Project;


namespace XSharp.Project
{

    public enum XSharpBuildAction
    {
        None,
        Compile,
        Content,
        EmbeddedResource,
        ApplicationDefinition,
        Page,
        Resource
    };


    [ComVisible(true)]
    [CLSCompliant(false)]
    [Guid(XSharpConstants.FileNodePropertiesGuidString)]
    public class XSharpFileNodeProperties : SingleFileGeneratorNodeProperties
    {
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
        public string URL
        {
            get { return this.Url; }
        }


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
        public override BuildAction BuildAction
        {
            get
            {
                switch (XSharpBuildAction)
                {
                    case XSharpBuildAction.ApplicationDefinition:
                    case XSharpBuildAction.Page:
                    case XSharpBuildAction.Resource:
                        return BuildAction.Compile;

                    default:
                        return (BuildAction)Enum.Parse(typeof(BuildAction), XSharpBuildAction.ToString());
                }
            }

            set
            {
                try
                {
                    this.XSharpBuildAction = (XSharpBuildAction)Enum.Parse(typeof(XSharpBuildAction), value.ToString());
                }
                catch (Exception)
                {
                    this.XSharpBuildAction = XSharpBuildAction.None;
                }
            }
        }
    }
}