using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class XTypeMemberList : List<XTypeMember>
    {

        /// <summary>
        /// Add a unique XTypeMember in the list :
        /// Unique means :
        ///     - unique Name
        ///     - in the same file
        ///     - same prototype
        /// </summary>
        /// <param name="item"></param>
        public new void Add(XTypeMember item)
        {
            ////
            //XTypeMember element = item.Parent.Members.Find(x =>
            //{
            //    if (x.File == item.File)
            //    {
            //        return (x.Prototype == item.Prototype);
            //    }
            //    return false;
            //});
            ////
            //if ( element == null )
                base.Add(item);
        }
    }
}
