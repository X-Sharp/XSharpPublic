//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
namespace Microsoft.VisualStudio.Project
{
    using System;
	using System.ComponentModel;
	using System.Diagnostics.CodeAnalysis;
	using System.Runtime.InteropServices;
	using Microsoft.VisualStudio.Project;

    /// <summary>
    /// Represents folder node properties.
    /// </summary>
    /// <remarks>This class must be public and marked as ComVisible in order for the DispatchWrapper to work correctly.</remarks>
    [CLSCompliant(false)]
    [ComVisible(true)]
    [Guid("228F7975-CB1C-45AE-ADCA-51FD9B6C3101")]
    [SuppressMessage("Microsoft.Interoperability", "CA1409:ComVisibleTypesShouldBeCreatable")]
    public class XFolderNodeProperties : FolderNodeProperties
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpFolderNodeProperties"/> class.
        /// </summary>
        /// <param name="node">The node that contains the properties to expose via the Property Browser.</param>
        public XFolderNodeProperties(XFolderNode  node)
            : base(node)
        {
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// Creates a custom property descriptor for the node properties, which affects the behavior
        /// of the property grid.
        /// </summary>
        /// <param name="propertyDescriptor">The <see cref="PropertyDescriptor"/> to wrap.</param>
        /// <returns>A custom <see cref="PropertyDescriptor"/> object.</returns>
        [SuppressMessage("Microsoft.Naming", "CA1725:ParameterNamesShouldMatchBaseDeclaration", MessageId = "0#", Justification = "In the 2005 SDK, it's called p and in the 2008 SDK it's propertyDescriptor")]
        public override DesignPropertyDescriptor CreateDesignPropertyDescriptor(PropertyDescriptor propertyDescriptor)
        {
            return new XDesignPropertyDescriptor(propertyDescriptor);
        }
    }
}
