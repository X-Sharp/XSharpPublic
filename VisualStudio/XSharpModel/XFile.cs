//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Collections.Immutable;
using System.Collections.Concurrent;
namespace XSharpModel
{
    [DebuggerDisplay("{FullPath,nq}")]
    public class XFile
    {
        private List<string> _usings;
        private List<string> _usingStatics;
        private string filePath;
        private ConcurrentDictionary<string, XType> _typeList;
        
        private XType _globalType;
        // 
        private object _lock;
        //private int _hashCode;
        private bool _parsed;
        private bool _xaml;
        private DateTime _lastWritten;

        public XFile(string fullPath)
        {
            // TODO: Change to support Case Sensitive types
            _usings = new List<string>();
            _usingStatics = new List<string>();
            this.filePath = fullPath;
            _xaml = System.IO.Path.GetExtension(fullPath).ToLower() == ".xaml";
            //
            InitTypeList();
            //
            _parsed = false;
            _lock = new object();
            _lastWritten = DateTime.MinValue;
            //_hashCode = 0;

        }

        /// <summary>
        /// Reset the TypeList associated with the File, reCreating the GlobalType
        /// </summary>
        public void InitTypeList()
        {
            this._typeList = new ConcurrentDictionary<string, XType>(StringComparer.InvariantCultureIgnoreCase);
            this._globalType = XType.CreateGlobalType(this);
            this._typeList.TryAdd(_globalType.Name, _globalType);
            _usings = new List<string>();
            _usingStatics = new List<string>();

        }
        private XProject project;
        public XProject Project {

            get
            {
                if (project == null)
                {
                    project = XSolution.OrphanedFilesProject;
                    project.AddFile(this.filePath);
                }
                return project;
            }

            set
            {
                project = value;
            }
        }

        public string Name
        {
            get
            {
                return System.IO.Path.GetFileNameWithoutExtension(this.filePath);
            }
        }

        public XType GlobalType => _globalType;

        public string FullPath
        {
            get
            {
                return filePath;
            }

            set
            {
                filePath = value;
            }
        }

        public void AddUsing(string name)
        {
            if (! string.IsNullOrEmpty(name))
            {
                lock (_lock)
                {
                    _usings.AddUnique(name);
                }
            }
        }

        public ImmutableList<string> Usings
        {
            get
            {
                lock (_lock)
                {
                    return _usings.ToImmutableList();
                }
            }

        }

        public ImmutableList<string> UsingStatics
        {
            get
            {
                return _usingStatics.ToImmutableList();
            }
        }

        public ImmutableList<string> AllUsingStatics
        {
            get
            {

                lock (_lock)
                {
                    List<string> statics = new List<string>();
                    statics.AddRange(_usingStatics);
                    if (this.Project != null && this.Project.ProjectNode != null && this.Project.ProjectNode.ParseOptions.IsDialectVO)
                    {
                        foreach (var asm in this.Project.AssemblyReferences)
                        {
                            var globalclass = asm.GlobalClassName;
                            if (!string.IsNullOrEmpty(globalclass))
                            {
                                statics.AddUnique(globalclass);
                            }
                        }
                    }
                    return statics.ToImmutableList();
                }
            }

        }

        public XType AddType(XType newType)
        {
            lock (_lock)
            {
                if ( _typeList.TryAdd(newType.FullName, newType))
                {
                    return newType;
                }
                return _typeList[newType.FullName];
            }
        }

        public IImmutableDictionary<string, XType> TypeList
        {
            get
            {
                
                lock (_lock)
                {
                    return _typeList.ToImmutableDictionary();
                }
            }

        }
        public DateTime LastWritten
        {
            get { return _lastWritten; }
            set
            {
                lock (_lock)
                {
                    _lastWritten = value;
                }

            }
        }


        /// <summary>
        /// Flag indicating if File has been parsed at least once
        /// </summary>
        public bool Parsed
        {
            get
            {
                bool retValue;
                lock (_lock)
                {
                    retValue = _parsed;
                }
                return retValue;
            }

        }

        /// <summary>
        /// Block the running Thread until the file has been parsed
        /// </summary>
        public void WaitParsing()
        {
            //_parsedEvent.WaitOne();
            lock (_lock)
            {
                if ( !Parsed )
                {
                    //
                    SourceWalker sw = new SourceWalker(this);
                    try
                    {
                        var xTree = sw.Parse();
                        sw.BuildModel(xTree, false);
                        //
                    }
                    catch (Exception e)
                    {
                        Support.Debug("XFile.WaitParsing"+e.Message);
                    }
                }
            }
        }

        public XTypeMember FirstMember()
        {
            lock (_lock)
            {
                foreach (var type in TypeList.Values)
                {
                    foreach (var member in type.Members)
                    {
                        return member;
                    }
                }
                return null;
            }
        }

        public bool IsXaml => _xaml;
    }

}
