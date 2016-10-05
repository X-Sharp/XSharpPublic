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
    /// <summary>
    /// Specifies the display name for a property, event, 
    /// or public void method which takes no arguments.
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Property | AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
    internal sealed class LocDisplayNameAttribute : DisplayNameAttribute
    {
        #region Fields
        private string name;
        #endregion Fields

        #region Constructors
        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="name">Attribute display name.</param>
        public LocDisplayNameAttribute(string name)
        {
            this.name = name;
        }
        #endregion

        #region Overriden Implementation
        /// <summary>
        /// Gets attribute display name.
        /// </summary>
        public override string DisplayName
        {
            get
            {
                string result = Resources.GetString(this.name);

                if(result == null)
                {
                    result = this.name;
                }

                return result;
            }
        }
        #endregion
    }
}
