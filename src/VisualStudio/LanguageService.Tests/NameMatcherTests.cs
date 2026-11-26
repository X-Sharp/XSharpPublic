//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using XSharp.LanguageService.NavigateTo;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class NameMatcherTests
    {
        [Theory]
        [InlineData("WriteText", "writetext", "Exact")]
        [InlineData("Write", "WriteText", "Prefix")]
        [InlineData("Text", "WriteText", "Substring")]
        [InlineData("WT", "WriteText", "CamelCase")]
        [InlineData("wt", "WriteText", "CamelCase")]
        [InlineData("WrTe", "WriteText", "CamelCase")]
        [InlineData("xyz", "WriteText", "None")]
        [InlineData("", "WriteText", "None")]
        [InlineData("  ", "WriteText", "None")]
        [InlineData("Name", "_Name", "Substring")]
        [InlineData("WT", "Write_text", "CamelCase")]
        [InlineData(" Write ", "WriteText", "Prefix")]
        public void MatchesLikeCSharp(string pattern, string name, string expected)
        {
            Assert.Equal(expected, NameMatcher.Match(pattern, name).ToString());
        }
    }
}
