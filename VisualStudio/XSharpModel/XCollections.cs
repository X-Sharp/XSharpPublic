////
//// Copyright (c) XSharp B.V.  All Rights Reserved.  
//// Licensed under the Apache License, Version 2.0.  
//// See License.txt in the project root for license information.
////
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public static class CollectionExtensions
    {
        public static XTypeMember Find( this IEnumerable<XTypeMember> collection, Func<XTypeMember, bool> pred)
        {
            return collection.Where(pred).FirstOrDefault();
       }
    }
}
