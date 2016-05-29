//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using Microsoft.Windows.Design.Host;

namespace XSharp.Project.WPF
{
#pragma warning disable 612, 618
    class XSharpRuntimeNameProvider : RuntimeNameProvider
    {
        public override string CreateValidName(string proposal)
        {
            return proposal;
        }

        public override bool IsExistingName(string name)
        {
            //We will get uniqueness in the XAML file via the matchScope predicate.
            //In a more complete implementation, this method would verify that there isn't
            //a member in the code behind file with the given name.
            return false;
        }

        public override RuntimeNameFactory NameFactory
        {
            get { return new XSharpRuntimeNameFactory(); }
        }
    }
#pragma warning restore 612, 618
}