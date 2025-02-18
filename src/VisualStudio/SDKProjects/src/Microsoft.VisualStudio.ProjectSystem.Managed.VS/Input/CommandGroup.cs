// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

namespace Microsoft.VisualStudio.Input;

    /// <summary>
    ///     Provides common well-known command groups.
    /// </summary>
    internal static class CommandGroup
    {
        public const string UIHierarchyWindow = VSConstants.CMDSETID.UIHierarchyWindowCommandSet_string;
        public const string VisualStudioStandard97 = VSConstants.CMDSETID.StandardCommandSet97_string;
        public const string VisualStudioStandard2k = VSConstants.CMDSETID.StandardCommandSet2K_string;
        public const string FSharpProject = "{75AC5611-A912-4195-8A65-457AE17416FB}";
        public const string ManagedProjectSystemOrder = "{07530DC2-31D8-429D-8250-848AF494F008}";
        public const string ManagedProjectSystem = "{07530DC2-31D8-429D-8250-848AF494F007}";
        public const string WPF = "{A8878AC2-6163-4C15-9767-1871DD750C6A}";
}
