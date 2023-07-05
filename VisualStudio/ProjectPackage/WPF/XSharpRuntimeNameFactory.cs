//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel;
using System.Globalization;
using Microsoft.VisualStudio.Project;
namespace XSharp.Project.WPF
{
#pragma warning disable 612, 618

    [Serializable]
    class XSharpRuntimeNameFactory : RuntimeNameFactory
    {
        public override string CreateUniqueName(
            Type itemType, string proposedName, Predicate<string> matchScope, bool rootScope, RuntimeNameProvider provider)
        {
            Utilities.ArgumentNotNull(nameof(itemType), itemType);
            Utilities.ArgumentNotNull(nameof(matchScope), matchScope);
            Utilities.ArgumentNotNull(nameof(provider), provider);

            string baseName = proposedName;

            if (string.IsNullOrEmpty(baseName))
            {
                baseName = TypeDescriptor.GetClassName(itemType);

                int lastDot = baseName.LastIndexOf('.');

                if (lastDot != -1)
                {
                    baseName = baseName.Substring(lastDot + 1);
                }

                // Names should start with a lower-case character
                //
                baseName = char.ToLower(baseName[0], CultureInfo.InvariantCulture) + baseName.Substring(1);
            }

            string name = null;
            int idx = 1;
            bool isUnique = false;

            while (!isUnique)
            {
                name = string.Format(CultureInfo.InvariantCulture, "{0}{1}", baseName, idx++);

                // Test for uniqueness
                isUnique = !matchScope(name);

                string tempName = name;
                name = provider.CreateValidName(tempName);

                if (!string.Equals(name, tempName, StringComparison.Ordinal))
                {
                    // RNP has changed the name, test again for uniqueness
                    isUnique = !matchScope(name);
                }

                if (isUnique && rootScope)
                {
                    // Root name scope means we have to let the RNP test for uniqueness too
                    isUnique = !provider.IsExistingName(name);
                }
            }

            return name;
        }
    }

#pragma warning restore 612, 618

}
