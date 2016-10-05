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
using System.ComponentModel;

namespace XSharp.Project
{
    [AttributeUsage(AttributeTargets.All)]
    internal sealed class ResourcesDescriptionAttribute : DescriptionAttribute
    {
        #region Fields
        private bool replaced;
        #endregion

        #region Constructors
        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="description">Attribute description.</param>
        public ResourcesDescriptionAttribute(string description)
            : base(description)
        {
        }
        #endregion

        #region Overriden Implementation
        /// <summary>
        /// Gets attribute description.
        /// </summary>
        public override string Description
        {
            get
            {
                if(!replaced)
                {
                    replaced = true;
                    DescriptionValue = Resources.GetString(base.Description);
                }

                return base.Description;
            }
        }
        #endregion
    }
}
