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
                        if ( x.FullName.ToLowerInvariant() == typeName.ToLowerInvariant() )
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
            return xType;
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
