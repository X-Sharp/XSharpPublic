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
            base.Add(item);
        }
    }
}
