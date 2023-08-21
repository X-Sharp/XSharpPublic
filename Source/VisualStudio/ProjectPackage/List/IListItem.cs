using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    internal interface IListItem
    {
        bool GetValue(string columnName, out object value);
    }
}
