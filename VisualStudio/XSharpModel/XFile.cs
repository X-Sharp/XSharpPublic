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
        private List<XType> _typeList;
        private XType _globalType;
        private Mutex _lock;
        // 
        //private bool _parsed;
        private ManualResetEvent _parsedEvent;

        public XFile( string fullPath )
        {
            _typeList = new List<XType>();
            _usings = new List<string>();
            this.filePath = fullPath;
            _globalType = XType.CreateGlobalType();
            _typeList.Add(_globalType);
            //
            _lock = new Mutex();
            //_parsed = false;
            _parsedEvent = new ManualResetEvent(false);
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

        public List<string> Usings
        {
            get
            {
                return _usings;
            }

        }

        public List<XType> TypeList
        {
            get
            {
                List<XType> retValue;
                _lock.WaitOne();
                retValue = _typeList;
                _lock.ReleaseMutex();
                return retValue;
            }

            set
            {
                _lock.WaitOne();
                _typeList = value;
                _lock.ReleaseMutex();
            }
        }

        /// <summary>
        /// Set the XFile in parsing state : 
        /// It means that the access to the TypeList is locked by a Mutex.
        /// The Thread who set set the value is the Owner of the Mutex.
        /// </summary>
        public bool Parsing
        {
            set
            {
                if ( value == true )
                {
                    _lock.WaitOne();
                    _parsedEvent.Reset();
                }
                else
                {
                    _lock.ReleaseMutex();
                    _parsedEvent.Set();
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
                return _parsedEvent.WaitOne(0);
            }
        }

        /// <summary>
        /// Block the running Thread until the file has been parsed
        /// </summary>
        public void WaitParsing()
        {
            _parsedEvent.WaitOne();
        }

    }
}
