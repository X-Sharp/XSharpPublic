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
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Microsoft.VisualStudio.Project.Automation
{
    /// <summary>
    /// Contains all of the properties of a given object that are contained in a generic collection of properties.
    /// </summary>
    [SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix")]
    [CLSCompliant(false), ComVisible(true)]
    public class OAProperties : Properties
    {
        #region fields
        private NodeProperties _target;
        private Dictionary<string, EnvDTE.Property> _properties = new Dictionary<string, EnvDTE.Property>();
        #endregion

        #region properties
        /// <summary>
        /// Defines the NodeProperties object that contains the defines the properties.
        /// </summary>
        public NodeProperties Target
        {
            get
            {
                return _target;
            }
        }

        /// <summary>
        /// The hierarchy node for the object which properties this item represent
        /// </summary>
        public HierarchyNode Node
        {
            get
            {
                return this.Target.Node;
            }
        }

        /// <summary>
        /// Defines a dictionary of the properties contained.
        /// </summary>
        public Dictionary<string, Property> Properties
        {
            get
            {
                return _properties;
            }
        }
        #endregion

        #region ctor
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2214:DoNotCallOverridableMethodsInConstructors")]
        public OAProperties(NodeProperties target)
        {
            Utilities.ArgumentNotNull("target", target);

            this._target = target;
            this.AddPropertiesFromType(target.GetType());
        }
        #endregion

        #region EnvDTE.Properties
        /// <summary>
        /// Microsoft Internal Use Only.
        /// </summary>
        public virtual object Application
        {
            get { return null; }
        }

        /// <summary>
        /// Gets a value indicating the number of objects in the collection.
        /// </summary>
        public int Count
        {
            get { return _properties.Count; }
        }

        /// <summary>
        /// Gets the top-level extensibility object.
        /// </summary>
        public virtual DTE DTE
        {
            get
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    if (_target == null || _target.Node == null || _target.Node.ProjectMgr == null || _target.Node.ProjectMgr.IsClosed ||
                        _target.Node.ProjectMgr.Site == null)
                    {
                        throw new InvalidOperationException();
                    }
                    return _target.Node.ProjectMgr.Site.GetService(typeof(DTE)) as DTE;
                });
            }
        }

        /// <summary>
        /// Gets an enumeration for items in a collection.
        /// </summary>
        /// <returns>An enumerator. </returns>
        public IEnumerator GetEnumerator()
        {
            if(_properties == null)
            {
                yield return null;
            }

            if(_properties.Count == 0)
            {
                yield return new OANullProperty(this);
            }

            IEnumerator enumerator = _properties.Values.GetEnumerator();

            while(enumerator.MoveNext())
            {
                yield return enumerator.Current;
            }
        }

        /// <summary>
        /// Returns an indexed member of a Properties collection.
        /// </summary>
        /// <param name="index">The index at which to return a mamber.</param>
        /// <returns>A Property object.</returns>
        public virtual Property Item(object index)
        {
            if(index is string)
            {
                string indexAsString = (string)index;
                if(_properties.ContainsKey(indexAsString))
                {
                    return ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        return _properties[indexAsString];
                    });
                }
            }
            else if(index is int)
            {
                int realIndex = (int)index - 1;
                if(realIndex >= 0 && realIndex < _properties.Count)
                {
                    IEnumerator enumerator = _properties.Values.GetEnumerator();

                    int i = 0;
                    while(enumerator.MoveNext())
                    {
                        if(i++ == realIndex)
                        {
                            return ThreadHelper.JoinableTaskFactory.Run(async delegate
                            {
                                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                            return (EnvDTE.Property)enumerator.Current;
                            });
                        }
                    }
                }
            }
            // do not throw exception.
            //throw new ArgumentException(SR.GetString(SR.InvalidParameter, CultureInfo.CurrentUICulture), "index");
            return new OANullProperty(this);
        }
        /// <summary>
        /// Gets the immediate parent object of a Properties collection.
        /// </summary>
        public virtual object Parent
        {
            get { return null; }
        }
        #endregion

        #region methods
        /// <summary>
        /// Add properties to the collection of properties filtering only those properties which are com-visible and AutomationBrowsable
        /// </summary>
        /// <param name="targetType">The type of NodeProperties the we should filter on</param>
        protected void AddPropertiesFromType(Type targetType)
        {
            Utilities.ArgumentNotNull("targetType", targetType);

            // If the type is not COM visible, we do not expose any of the properties
            if(!IsComVisible(targetType))
                return;

            // Add all properties being ComVisible and AutomationVisible
            PropertyInfo[] propertyInfos = targetType.GetProperties();
            foreach(PropertyInfo propertyInfo in propertyInfos)
            {
                if(!IsInMap(propertyInfo) && IsComVisible(propertyInfo) && IsAutomationVisible(propertyInfo))
                {
                    AddProperty(propertyInfo);
                }
            }
        }
        #endregion

        #region virtual methods
        /// <summary>
        /// Creates a new OAProperty object and adds it to the current list of properties
        /// </summary>
        /// <param name="propertyInfo">The property to be associated with an OAProperty object</param>
        protected virtual void AddProperty(PropertyInfo propertyInfo)
        {
            Utilities.ArgumentNotNull("propertyInfo", propertyInfo);

            _properties.Add(propertyInfo.Name, new OAProperty(this, propertyInfo));
        }
        #endregion

        #region helper methods

        private bool IsInMap(PropertyInfo propertyInfo)
        {
            return _properties.ContainsKey(propertyInfo.Name);
        }

        private static bool IsAutomationVisible(PropertyInfo propertyInfo)
        {
            object[] customAttributes = propertyInfo.GetCustomAttributes(typeof(AutomationBrowsableAttribute), inherit: true);
            for (int i = 0; i < customAttributes.Length; i++)
            {
                if (!((AutomationBrowsableAttribute)customAttributes[i]).Browsable)
                {
                    return false;
                }
            }
            return true;
        }

        private static bool IsComVisible(Type targetType)
        {
            object[] customAttributes = targetType.GetCustomAttributes(typeof(ComVisibleAttribute), inherit: true);
            for (int i = 0; i < customAttributes.Length; i++)
            {
                if (!((ComVisibleAttribute)customAttributes[i]).Value)
                {
                    return false;
                }
            }
            return true;
        }

        private static bool IsComVisible(PropertyInfo propertyInfo)
        {
            object[] customAttributes = propertyInfo.GetCustomAttributes(typeof(ComVisibleAttribute), inherit: true);
            for (int i = 0; i < customAttributes.Length; i++)
            {
                if (!((ComVisibleAttribute)customAttributes[i]).Value)
                {
                    return false;
                }
            }
            return true;
        }
        #endregion
    }
}
