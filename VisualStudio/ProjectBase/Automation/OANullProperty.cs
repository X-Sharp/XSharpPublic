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
using EnvDTE;

using System;
using System.Runtime.InteropServices;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// This object defines a so called null object that is returned as instead of null. This is because callers in VSCore usually crash if a null propery is returned for them.
    /// </summary>
    [CLSCompliant(false), ComVisible(true)]
    public class OANullProperty : Property
    {
        #region fields
        private OAProperties parent;
        #endregion

        #region ctors

        public OANullProperty(OAProperties parent)
        {
            this.parent = parent;
        }
        #endregion

        #region Property

        public object Application
        {
            get { return String.Empty; }
        }

        public Properties Collection
        {
            get
            {
                //todo: EnvDTE.Property.Collection
                return this.parent;
            }
        }

        public DTE DTE
        {
            get { return null; }
        }

        public object get_IndexedValue(object index1, object index2, object index3, object index4)
        {
            return String.Empty;
        }

        public void let_Value(object value)
        {
            //todo: let_Value
        }

        public string Name
        {
            get { return String.Empty; }
        }

        public short NumIndices
        {
            get { return 0; }
        }

        public object Object
        {
            get { return this.parent.Target; }
            set
            {
            }
        }

        public Properties Parent
        {
            get { return this.parent; }
        }

        public void set_IndexedValue(object index1, object index2, object index3, object index4, object value)
        {

        }

        public object Value
        {
            get { return String.Empty; }
            set { }
        }
        #endregion
    }
}
