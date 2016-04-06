
using System;

namespace XSharp.Project
{
    /// <summary>
    /// The package GUID 
    /// </summary>
    public static class GuidStrings
    {
        public const string guidXSharpProjectPkgString = "E299FB7B-D273-4678-9ACF-B37B4AF04A12";
        public const string guidXSharpProjectCmdSetString =     "32A7B1CD-3663-4A70-B855-486671F5839C";
        public const string guidXSharpProjectFactoryString = "AA6C8D78-22FF-423A-9C7C-5F2393824E04";

        public static readonly Guid guidXSharpProjectCmdSet = new Guid(guidXSharpProjectCmdSetString);
        public static readonly Guid guidXSharpProjectFactory = new Guid(guidXSharpProjectFactoryString);
    }
}
