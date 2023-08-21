//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel;
using Microsoft.VisualStudio.Project;

namespace Microsoft.VisualStudio.Project
{

    /// <summary>
    /// Subclasses <see cref="DesignPropertyDescriptor"/> to allow for non-bolded text in the property grid.
    /// </summary>
    public class XNonMemberDesignPropertyDescriptor : DesignPropertyDescriptor
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpNonMemberDesignPropertyDescriptor"/> class.
        /// </summary>
        /// <param name="propertyDescriptor">The <see cref="PropertyDescriptor"/> to wrap.</param>
        public XNonMemberDesignPropertyDescriptor(PropertyDescriptor propertyDescriptor)
            : base(propertyDescriptor)
        {
        }

        /// <summary>
        /// Properties of non-member nodes should be read-only.
        /// </summary>
        public override bool IsReadOnly
        {
            get
            {
                return true;
            }
        }

        // =========================================================================================
        // Methods
        // =========================================================================================

        /// <summary>
        /// By returning false here, we're always going to show the property as non-bolded.
        /// </summary>
        /// <param name="component">The component to check.</param>
        /// <returns>Always returns false.</returns>
        public override bool ShouldSerializeValue(object component)
        {
            return false;
        }
    }
}
