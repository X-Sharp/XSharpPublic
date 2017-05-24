//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace XSharpModel
{
    [DebuggerDisplay("{FullPath,nq}")]
    public class XFile
    {
        private List<String> _usings;
        private List<String> _usingStatics;
        private string filePath;
        private Dictionary<string, XType> _typeList;
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
            this._typeList = new Dictionary<string, XType>(StringComparer.InvariantCultureIgnoreCase);
            this._globalType = XType.CreateGlobalType(this);
            this._typeList.Add(_globalType.Name, _globalType);
        }

        public XProject Project { get; internal set; }

        public String Name
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

        // 
        public List<string> Usings
        {
            get
            {
                return _usings;
            }

        }

        public List<string> UsingStatics
        {
            get
            {
                return _usingStatics;
            }

        }

        public Dictionary<string, XType> TypeList
        {
            get
            {
                Dictionary<string, XType> retValue;
                lock (_lock)
                {
                    retValue = _typeList;
                }
                return retValue;
            }

            set
            {
                lock (_lock)
                {
                    _typeList = value;
                }
            }
        }
        public DateTime LastWritten
        {
            get { return _lastWritten; }
            set { _lastWritten = value; }
        }

        ///// <summary>
        ///// Set the XFile in parsing state : 
        ///// </summary>
        //public bool Parsing
        //{
        //    set
        //    {
        //        if (value == true)
        //        {
        //            _lock.WaitOne();
        //            _parsedEvent.Reset();
        //        }
        //        else
        //        {
        //            _lock.ReleaseMutex();
        //            _parsedEvent.Set();
        //        }
        //    }
        //}

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

        //public int HashCode
        //{
        //    get
        //    {
        //        return _hashCode;
        //    }

        //    set
        //    {
        //        if (_hashCode != value)
        //            _parsed = false;
        //        _hashCode = value;
        //    }
        //}

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
                    SourceWalker sw = new SourceWalker(null);
                    //
                    sw.File = this;
                    try
                    {
                        sw.InitParse();
                        sw.BuildModelOnly();
                        //
                    }
                    catch (Exception e)
                    {
                        System.Diagnostics.Debug.WriteLine(e.Message);
                    }
                }
            }
        }

        // Unused ?
        //public void Parse( string contentText )
        //{
        //    XSharpModel.SourceWalker sw = new XSharpModel.SourceWalker();
        //    //
        //    sw.Source = contentText;
        //    sw.File = this;
        //    try
        //    {
        //        sw.InitParse();
        //        sw.BuildModelOnly();
        //        //
        //    }
        //    catch (Exception e)
        //    {
        //        System.Diagnostics.Debug.WriteLine(e.Message);
        //    }
        //}
        public XTypeMember FirstMember()
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

        public bool IsXaml => _xaml;
    }

}
