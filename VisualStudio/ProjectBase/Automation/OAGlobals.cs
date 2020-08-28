using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Xml;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Project.Automation
{

     
	[CLSCompliant(false), ComVisible(true)]
	public class OAGlobals : EnvDTE.Globals
	{

        Dictionary<string, object> _variables;
        readonly List<string> _persisted;
        ProjectNode _parent;

        public OAGlobals(ProjectNode prj)
        {
            _parent = prj;
            //
            _variables = new Dictionary<string, object>();
            _persisted = new List<string>();
            //
            //Initialize();
        }


        public void Initialize()
        {
            if (_parent.BuildProject == null)
                return;
            // Retrieve the elements from the XML file
            Microsoft.Build.Construction.ProjectExtensionsElement projectExtensions = null;
            foreach (Microsoft.Build.Construction.ProjectElement element in _parent.BuildProject.Xml.ChildrenReversed)
            {
                projectExtensions = element as Microsoft.Build.Construction.ProjectExtensionsElement;
                if (projectExtensions != null)
                {
                    break;
                }
            }
            //
            if (projectExtensions != null)
            {
                string str = projectExtensions["VisualStudio"];
                if (!string.IsNullOrEmpty(str))
                {
                    XmlDocument document = new XmlDocument();
                    try
                    {
                        document.LoadXml(str);
                        XmlNode node = document.SelectSingleNode("/UserProperties");
                        if (node != null)
                        {
                            foreach (XmlAttribute attribute in node.Attributes)
                            {
                                this._variables[attribute.Name] = attribute.Value;
                                this._persisted.Add(attribute.Name);
                            }
                        }
                    }
                    catch (XmlException exception)
                    {
                        //
                        string e = exception.Message;
                        // OutputWindow.WriteOnOutputWindowThreadSafe(Guid.Empty, string.Format(CultureInfo.CurrentCulture, "jectExtensions, new object[] { exception.Message }));
                        //
                    }
                }
            }
        }

        public void ClearNonPersistedVariables()
        {
            Dictionary<string, object> result = new Dictionary<string, object>();

            foreach (string key in _variables.Keys)
            {
                if (_persisted.Contains(key))
                {
                    result.Add(key, _variables[key]);
                }
            }
            _variables = result;
            this._parent.SetProjectFileDirty(true);
        }

        public void ClearAll()
        {
            _variables.Clear();
            _persisted.Clear();
            this._parent.SetProjectFileDirty(true);
        }

        internal string BuildProjectExtensions()
        {
            if (this._persisted.Count == 0)
            {
                return string.Empty;
            }
            XmlDocument document = new XmlDocument();
            XmlElement element = document.CreateElement("VisualStudio");
            XmlElement newChild = document.CreateElement("UserProperties");
            element.AppendChild(newChild);
            foreach (string str in this._persisted)
            {
                XmlAttribute node = document.CreateAttribute(str);
                node.Value = this._variables[str].ToString();
                newChild.Attributes.Append(node);
            }
            return element.InnerXml;
        }


        #region Globals Members

        public EnvDTE.DTE DTE
        {
            get 
            {
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    return (EnvDTE.DTE)this._parent.Site.GetService(typeof(EnvDTE.DTE));
                });
            }
        }

        public object Parent
        {
            get 
            {
                return this._parent;
            }
        }

        public object VariableNames
        {
            get 
            { 
                //throw new Exception("The method or operation is not implemented."); 
                string[] names = new string[_variables.Keys.Count];
                _variables.Keys.CopyTo(names,0);
                return names;
            }
        }

        public bool get_VariableExists(string Name)
        {
            return _variables.ContainsKey(Name);
        }

        public bool get_VariablePersists(string VariableName)
        {
            return _persisted.Contains(VariableName);
        }

        public void set_VariablePersists(string VariableName, bool pVal)
        {
            if (pVal)
            {
                if (!_persisted.Contains(VariableName))
                {
                    _persisted.Add(VariableName);
                }
            }
            else
            {
                _persisted.Remove(VariableName);
            }
            this._parent.SetProjectFileDirty(true);
        }

        public object this[string VariableName]
        {
            get
            {
                return _variables[VariableName];
            }
            set
            {
                _variables[VariableName] = value;
                this._parent.SetProjectFileDirty(true);
            }
        }

        #endregion
    }

}
