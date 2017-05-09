//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class XProject
    {
        private Dictionary<string, XFile> xFilesDict;
        private IXSharpProject _projectNode;
        //private XType _globalType;
        private bool _loaded;
        //
        private SystemTypeController _typeController;
        // List of external Projects, currently unloaded
        private List<String> _unprocessedProjectReferences = new List<String>();
        // List of external Projects, currently loaded
        private List<XProject> _ReferencedProjects = new List<XProject>();

        // See above
        private List<String> _unprocessedStrangerProjectReferences = new List<String>();
        private List<EnvDTE.Project> _StrangerProjects = new List<EnvDTE.Project>();

        public XProject(IXSharpProject project)
        {
            _projectNode = project;
            xFilesDict = new Dictionary<String, XFile>(StringComparer.OrdinalIgnoreCase);
            //this._globalType = XType.CreateGlobalType();
            //
            this._typeController = new SystemTypeController();
            this._loaded = true;
        }

        public String Name
        {
            get
            {
                return System.IO.Path.GetFileNameWithoutExtension(ProjectNode.Url);
            }
        }

        public bool Loaded
        {
            get { return _loaded; }
            set { _loaded = value; }
        }

        public List<XFile> Files
        {
            get
            {
                return xFilesDict.Values.ToList();
            }
        }

        public IXSharpProject ProjectNode
        {
            get
            {
                return _projectNode;
            }

            set
            {
                _projectNode = value;
            }
        }

        //public XType GlobalType
        //{
        //    get
        //    {
        //        return _globalType;
        //    }

        //    //set
        //    //{
        //    //    _globalType = value;
        //    //}
        //}

        public SystemTypeController TypeController
        {
            get
            {
                return _typeController;
            }

            set
            {
                _typeController = value;
            }
        }

        public bool AddFile(string filePath)
        {
            XFile file = new XFile(filePath);
            return this.AddFile(file);
        }

        public bool AddFile(XFile xFile)
        {
            if (xFile != null)
            {
                if (xFilesDict.ContainsKey(xFile.FullPath))
                {
                    xFilesDict.Remove(xFile.FullPath);
                }
                xFilesDict.Add(xFile.FullPath, xFile);
                xFile.Project = this;
                return true;
            }
            return false;
        }

        public bool AddProjectReference(string url)
        {
            if (!_unprocessedProjectReferences.Contains(url))
            {
                _unprocessedProjectReferences.Add(url);
                return true;
            }
            return false;
        }

        public bool RemoveProjectReference(string url)
        {
            if (_unprocessedProjectReferences.Contains(url))
            {
                _unprocessedProjectReferences.Remove(url);
                return true;
            }
            else
            {
                // Does this url belongs to a project in the Solution ?
                XProject prj = XSolution.FindProject(url);
                if (_ReferencedProjects.Contains(prj))
                {
                    _ReferencedProjects.Remove(prj);
                    return true;
                }
            }
            return false;
        }

        public bool AddStrangerProjectReference(string url)
        {
            if (!_unprocessedStrangerProjectReferences.Contains(url))
            {
                _unprocessedStrangerProjectReferences.Add(url);
                return true;
            }
            return false;
        }

        public bool RemoveStrangerProjectReference(string url)
        {
            if (_unprocessedStrangerProjectReferences.Contains(url))
            {
                _unprocessedStrangerProjectReferences.Remove(url);
                return true;
            }
            else
            {
                // Does this url belongs to a project in the Solution ?
                EnvDTE.Project prj = this.ProjectNode.FindProject(url);
                if (_StrangerProjects.Contains(prj))
                {
                    _StrangerProjects.Remove(prj);
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// List of XSharp Projects that our "current" project is referencing
        /// </summary>
        public IList<XProject> ReferencedProjects
        {
            get
            {
                List<String> existing = new List<String>();
                foreach (String s in _unprocessedProjectReferences)
                {
                    XProject p = XSolution.FindProject(s);
                    if (p != null)
                    {
                        existing.Add(s);
                        _ReferencedProjects.Add(p);
                    }
                }
                foreach (String s in existing)
                {
                    _unprocessedProjectReferences.Remove(s);
                }
                return _ReferencedProjects;
            }
        }


        /// <summary>
        /// List of stranger Projects that our "current" project is referencing.
        /// Could be any project that support EnvDTE.FileCodeModel (so CS, Vb.Net, ...)
        /// </summary>
        public IList<EnvDTE.Project> StrangerProjects
        {
            get
            {
                List<String> existing = new List<String>();
                foreach (String s in _unprocessedStrangerProjectReferences)
                {
                    EnvDTE.Project p = this.ProjectNode.FindProject(s);
                    if (p != null)
                    {
                        existing.Add(s);
                        _StrangerProjects.Add(p);
                    }
                }
                foreach (String s in existing)
                {
                    _unprocessedStrangerProjectReferences.Remove(s);
                }
                return _StrangerProjects;
            }
        }


        public XFile Find(string fileName)
        {
            return Files.Find(f => f.Name.ToLower() == fileName.ToLower());
        }

        public XFile FindFullPath(string fullPath)
        {
            if (xFilesDict.ContainsKey(fullPath))
                return xFilesDict[fullPath];
            return null;
        }

        public void Walk()
        {
            //
            ModelWalker walker = ModelWalker.GetWalker();
            walker.AddProject(this);

        }

        public void RemoveFile(string url)
        {
            if (this.xFilesDict.ContainsKey(url))
            {
                this.xFilesDict.Remove(url);
            }
            else
            {
                XFile xFile = this.Find(url);
                if (xFile != null)
                {
                    this.xFilesDict.Remove(xFile.FullPath);
                }
            }
        }


        /// <summary>
        /// Look for a TypeName in all Files that compose the current XProject
        /// </summary>
        /// <param name="typeName"></param>
        /// <param name="caseInvariant"></param>
        /// <returns></returns>
        public XType Lookup(string typeName, bool caseInvariant)
        {
            // Create local copies of the collections to make sure that other background
            // operations do not change the collections
            XType xType = null;
            XType xTemp = null;
            var aFiles = this.xFilesDict.Values.ToArray();
            foreach (XFile file in aFiles)
            {
                //
                if (caseInvariant)
                {
                    file.TypeList.TryGetValue(typeName.ToLowerInvariant(), out xTemp);
                    //file.TypeList.Find(x => x.FullName.ToLowerInvariant() == typeName.ToLowerInvariant());
                }
                else
                {
                    file.TypeList.TryGetValue(typeName.ToLower(), out xTemp);
                }
                if (xTemp != null)
                {
                    if (xTemp.IsPartial)
                    {
                        // Do we have the other parts ?
                        if (xType != null)
                        {
                            xType = xType.Merge(xTemp);
                        }
                        else
                        {
                            // We need to Copy the type, unless we will modify the original one !
                            xType = xTemp.Duplicate();
                        }
                    }
                    else
                    {
                        xType = xTemp;
                        break;
                    }
                }
            }
            //
            return xType;
        }

        public XType LookupReferenced(string typeName, bool caseInvariant)
        {
            XType xType = null;
            // Ok, might be a good idea to look into References, no ?
            foreach (var item in ReferencedProjects)
            {
                xType = item.Lookup(typeName, caseInvariant);
                if (xType != null)
                    break;
            }
            //
            return xType;
        }

        public XType LookupFullName(string typeName, bool caseInvariant)
        {
            // Create local copies of the collections to make sure that other background
            // operations do not change the collections
            XType xType = null;
            XType xTemp = null;
            var aFiles = this.xFilesDict.Values.ToArray();
            foreach (XFile file in aFiles)
            {
                var aTypes = file.TypeList.Values.ToArray();
                foreach (XType x in aTypes)
                {
                    if (caseInvariant)
                    {
                        if (x.FullName.ToLowerInvariant() == typeName.ToLowerInvariant())
                        {
                            xTemp = x;
                            break;
                        }
                    }
                    else
                    {
                        if (x.FullName.ToLower() == typeName.ToLower())
                        {
                            xTemp = x;
                            break;
                        }
                    }
                }
                if (xTemp != null)
                {
                    if (xTemp.IsPartial)
                    {
                        // Do we have the other parts ?
                        if (xType != null)
                        {
                            xType = xType.Merge(xTemp);
                        }
                        else
                        {
                            // We need to Copy the type, unless we will modify the original one !
                            xType = xTemp.Duplicate();
                        }
                    }
                    else
                    {
                        xType = xTemp;
                        break;
                    }
                }
            }
            //
            return xType;
        }

        public XType LookupFullNameReferenced(string typeName, bool caseInvariant)
        {
            XType xType = null;
            // Ok, might be a good idea to look into References, no ?
            foreach (var item in ReferencedProjects)
            {
                xType = item.LookupFullName(typeName, caseInvariant);
                if (xType != null)
                    break;
            }
            //
            return xType;
        }

        // Look for a TypeName in "stranger" projects
        public EnvDTE.CodeElement LookupForStranger(string typeName, bool caseInvariant)
        {
            // If not found....
            EnvDTE.CodeElement foundElement = null;
            // Enumerate all referenced external Projects
            foreach (EnvDTE.Project project in this.StrangerProjects)
            {
                // Retrieve items -> Projects
                foreach (EnvDTE.ProjectItem item in project.ProjectItems)
                {
                    // Does this project provide a FileCodeModel ?
                    // XSharp is (currently) not providing such object
                    EnvDTE.FileCodeModel fileCodeModel = item.FileCodeModel;
                    if (fileCodeModel == null)
                        continue;
                    // First, search for Namespaces, this is where we will find Classes
                    foreach (EnvDTE.CodeElement codeElementNS in fileCodeModel.CodeElements)
                    {
                        if (codeElementNS.Kind == EnvDTE.vsCMElement.vsCMElementNamespace)
                        {
                            // Classes are childs
                            foreach (EnvDTE.CodeElement elt in codeElementNS.Children)
                            {
                                // TODO: And what about Enums, Structures, ... ???
                                // is it a Class ?
                                if (elt.Kind == EnvDTE.vsCMElement.vsCMElementClass)
                                {
                                    // So the element name is
                                    string elementName = elt.FullName;
                                    // !!!! WARNING !!! We may have nested types
                                    elementName = elementName.Replace("+", ".");
                                    // Got it ?
                                    if (caseInvariant)
                                    {
                                        if (elementName.ToLowerInvariant() == typeName.ToLowerInvariant())
                                        {
                                            // Bingo !
                                            foundElement = elt;
                                            break;
                                        }
                                    }
                                    else
                                    {
                                        if (elementName.ToLower() == typeName.ToLower())
                                        {
                                            // Bingo !
                                            foundElement = elt;
                                            break;
                                        }
                                    }
                                }
                            }
                            //
                            if (foundElement != null)
                                break;
                        }
                    }
                    //
                    if (foundElement != null)
                        break;
                }
                //
                if (foundElement != null)
                    break;
            }
            //
            return foundElement;
        }


        public List<XType> Namespaces
        {
            get
            {
                // Create local copies of the collections to make sure that other background
                // operations do not change the collections
                List<XType> ns = new List<XType>();
                var aFiles = this.Files.ToArray();
                foreach (XFile file in aFiles)
                {
                    var aTypes = file.TypeList.Values;
                    foreach (XType elmt in aTypes)
                    {
                        if (elmt.Kind == Kind.Namespace)
                        {
                            // Check for Duplicates
                            XType duplicate = ns.Find(x => x.Name.ToLowerInvariant() == elmt.Name.ToLowerInvariant());
                            if (duplicate == null)
                            {
                                ns.Add(elmt);
                            }
                        }
                    }
                }
                return ns;
            }
        }


    }
}
