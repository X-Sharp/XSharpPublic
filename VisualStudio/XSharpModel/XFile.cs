using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class XFile
    {
        private List<String> _usings;
        private string filePath;
        private List<XType> _typeList;

        public XFile( string fullPath )
        {
            _typeList = new List<XType>();
            _usings = new List<string>();
            this.filePath = fullPath;
        }

        public XProject Project { get; internal set; }

        public String Name
        {
            get
            {
                return System.IO.Path.GetFileNameWithoutExtension(this.filePath);
            }
        }

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
                return _typeList;
            }

            set
            {
                _typeList = value;
            }
        }

    }
}
