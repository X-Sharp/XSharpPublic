using System.Collections.Generic;
using XSharp.LanguageService.ForeignSource;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class ForeignProjectsTests
    {
        private static KeyValuePair<string, string>[] Outputs(params (string project, string dll)[] pairs)
        {
            var result = new KeyValuePair<string, string>[pairs.Length];
            for (int i = 0; i < pairs.Length; i++)
                result[i] = new KeyValuePair<string, string>(pairs[i].project, pairs[i].dll);
            return result;
        }

        [Fact]
        public void FindsTheProjectForItsOutput()
        {
            var outputs = Outputs((@"C:\s\Lib\Lib.csproj", @"C:\s\Lib\bin\Debug\Lib.dll"));
            Assert.Equal(@"C:\s\Lib\Lib.csproj", ForeignProjects.FindProjectFile(outputs, @"C:\s\Lib\bin\Debug\Lib.dll"));
        }

        [Fact]
        public void MatchesDespiteCaseAndRelativeSegments()
        {
            var outputs = Outputs((@"C:\s\Lib\Lib.csproj", @"C:\s\Lib\bin\Debug\Lib.dll"));
            Assert.Equal(@"C:\s\Lib\Lib.csproj", ForeignProjects.FindProjectFile(outputs, @"c:\S\lib\bin\Release\..\Debug\LIB.dll"));
        }

        [Fact]
        public void XSharpProjectsAreIgnored()
        {
            var outputs = Outputs((@"C:\s\App\App.xsproj", @"C:\s\App\bin\App.dll"), (@"C:\s\Old\Old.xsprj", @"C:\s\Old\bin\Old.dll"));
            Assert.Null(ForeignProjects.FindProjectFile(outputs, @"C:\s\App\bin\App.dll"));
            Assert.Null(ForeignProjects.FindProjectFile(outputs, @"C:\s\Old\bin\Old.dll"));
        }

        [Fact]
        public void UnknownDllGivesNull()
        {
            var outputs = Outputs((@"C:\s\Lib\Lib.csproj", @"C:\s\Lib\bin\Debug\Lib.dll"));
            Assert.Null(ForeignProjects.FindProjectFile(outputs, @"C:\s\Other\bin\Debug\Other.dll"));
        }

        [Fact]
        public void NullOrEmptyAssemblyGivesNull()
        {
            var outputs = Outputs((@"C:\s\Lib\Lib.csproj", @"C:\s\Lib\bin\Debug\Lib.dll"));
            Assert.Null(ForeignProjects.FindProjectFile(outputs, null));
            Assert.Null(ForeignProjects.FindProjectFile(outputs, ""));
        }
    }
}
