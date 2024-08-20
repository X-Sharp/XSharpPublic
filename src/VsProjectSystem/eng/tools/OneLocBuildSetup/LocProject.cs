﻿// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

namespace OneLocBuildSetup
{
    internal sealed record LocProject(Project[] Projects);

    internal sealed record Project(LocItem[] LocItems, string LanguageSet = "VS_Main_Languages");

    internal sealed record LocItem(string SourceFile, string OutputPath, string LclFile, string Languages = "", string CopyOption = "LangIDOnName");
}
