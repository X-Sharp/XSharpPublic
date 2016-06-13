//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// Guids.cs
// MUST match guids.h
using System;

namespace XSharp.LanguageService
{
    static class GuidList
    {
        public const string guidXSharpLanguageServicePkgString = "e9b6ee0f-9bfa-4cff-a60b-51a08bbc5050";
        public const string guidXSharpLanguageServiceCmdSetString = "6511ea00-4558-4ae7-84ee-0e2aebd40d88";

        public static readonly Guid guidXSharpLanguageServiceCmdSet = new Guid(guidXSharpLanguageServiceCmdSetString);
    };
}