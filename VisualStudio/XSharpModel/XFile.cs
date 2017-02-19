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
        private string filePath;
        private Dictionary<string, XType> _typeList;
        private XType _globalType;
        // 

        //private ManualResetEvent _parsedEvent;
        //private Mutex _lock;
        private object _lock;
        private bool _parsed;


        public XFile(string fullPath)
        {
            // TODO: Change to support Case Sensitive types
            _typeList = new Dictionary<string, XType>( StringComparer.InvariantCultureIgnoreCase);
            _usings = new List<string>();
            this.filePath = fullPath;
            _globalType = XType.CreateGlobalType();
            _typeList.Add( _globalType.Name, _globalType);
            //
            //_lock = new Mutex();
            //_parsed = false;
            //_parsedEvent = new ManualResetEvent(false);
            _lock = new object();
            _parsed = false;

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

            set
            {
                lock (_lock)
                {
                    _parsed = value;
                }
            }
        }

        /// <summary>
        /// Block the running Thread until the file has been parsed
        /// </summary>
        public void WaitParsing()
        {
            //_parsedEvent.WaitOne();
            lock ( _lock )
            {
                if ( !_parsed )
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
                        this.Parsed = true;
                    }
                    catch (Exception e)
                    {
                        System.Diagnostics.Debug.WriteLine(e.Message);
                    }
                }
            }
        }

    }
}
