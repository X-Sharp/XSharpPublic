//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace XSharp.Project.ShadowDesigner
{
    /// <summary>
    /// Generates/refreshes a plain C# companion project (sibling to the real .xsproj's
    /// folder) that VS's out-of-process WinForms Designer can open directly, mirroring the
    /// real project's TargetFramework and filtered reference set. Pure disk I/O, no VS API.
    /// </summary>
    internal static class CompanionProjectWriter
    {
        public readonly struct CompanionPaths
        {
            public CompanionPaths(string csprojPath, string designerCsPath)
            {
                CsprojPath = csprojPath;
                DesignerCsPath = designerCsPath;
            }

            public string CsprojPath { get; }
            public string DesignerCsPath { get; }
        }

        public readonly struct PackageRef
        {
            public PackageRef(string include, string version)
            {
                Include = include;
                Version = version;
            }

            public string Include { get; }
            public string Version { get; }
        }

        /// <summary>
        /// Companion project lives at {solutionParentDir}\{RealProjectName}.ShadowDesigner\ --
        /// a sibling to the real project's own folder, not nested inside it, so it can never
        /// be picked up by the real .xsproj's own default item globs.
        ///
        /// Computes the companion project's paths deterministically from the real .xsproj
        /// path and class name -- pure path arithmetic, no disk I/O, no XProject/parsing
        /// needed. Lets callers other than EnsureCompanionProject (e.g. the event-handler and
        /// Designer-changes sync commands) locate an already-existing companion project
        /// without re-running the parse/merge/generate pipeline.
        /// </summary>
        public static CompanionPaths ComputePaths(string realXsprojPath, string className)
        {
            string companionDir = GetCompanionDir(realXsprojPath);
            string csprojName = Path.GetFileNameWithoutExtension(realXsprojPath) + ".ShadowDesigner";
            return new CompanionPaths(
                Path.Combine(companionDir, csprojName + ".csproj"),
                Path.Combine(companionDir, className + ".Designer.cs"));
        }

        /// <summary>Companion project's Form1.cs-equivalent stub path (constructor/Dispose,
        /// and where the Designer sometimes puts new handler stubs) -- separate from
        /// CompanionPaths.DesignerCsPath since only the sync commands need it.</summary>
        public static string ComputeFormCsPath(string realXsprojPath, string className) =>
            Path.Combine(GetCompanionDir(realXsprojPath), className + ".cs");

        private static string GetCompanionDir(string realXsprojPath)
        {
            string realProjectDir = Path.GetDirectoryName(realXsprojPath)
                ?? throw new InvalidOperationException($"Could not determine directory of {realXsprojPath}.");
            string realProjectName = Path.GetFileNameWithoutExtension(realXsprojPath);
            string solutionParentDir = Path.GetDirectoryName(realProjectDir)
                ?? throw new InvalidOperationException($"Could not determine parent directory of {realProjectDir}.");
            return Path.Combine(solutionParentDir, realProjectName + ".ShadowDesigner");
        }

        public static CompanionPaths EnsureCompanionProject(
            string realXsprojPath,
            IReadOnlyList<string> referencePaths,
            string generatedDesignerCsharp,
            string namespaceName,
            string className)
        {
            string companionDir = GetCompanionDir(realXsprojPath);
            Directory.CreateDirectory(companionDir);

            string targetFramework = ReadTargetFramework(realXsprojPath);

            // Read <PackageReference> items directly out of the real .xsproj's XML (Include +
            // Version only, no MSBuild evaluation) and emit equivalent items into the
            // companion .csproj, rather than relying solely on XProject.AssemblyReferences'
            // resolved DLL paths for anything NuGet-sourced -- lets the companion project do
            // its own independent NuGet restore, working even if the real project has never
            // been built in this session.
            var packageReferences = ReadPackageReferences(realXsprojPath);
            var packageIncludeNames = new HashSet<string>(
                packageReferences.Select(p => p.Include), StringComparer.OrdinalIgnoreCase);

            // Exclude anything already covered by a <PackageReference> above, to avoid the
            // companion project seeing the same assembly twice (NuGet restore + a stale
            // HintPath), which MSBuild treats as an ambiguous-reference warning/error.
            var filteredReferencePaths = referencePaths
                .Where(p => !packageIncludeNames.Contains(Path.GetFileNameWithoutExtension(p)))
                .ToList();

            var paths = ComputePaths(realXsprojPath, className);
            WriteIfChanged(paths.CsprojPath, BuildCsprojContent(targetFramework, filteredReferencePaths, packageReferences));
            WriteIfChanged(paths.DesignerCsPath, generatedDesignerCsharp);

            // Only written once -- an "ordinary user file" slot, not regenerated each run, so
            // a future manual tweak here survives re-runs.
            string stubCsPath = ComputeFormCsPath(realXsprojPath, className);
            if (!File.Exists(stubCsPath))
            {
                File.WriteAllText(stubCsPath, BuildStubContent(namespaceName, className));
            }

            return paths;
        }

        private static string ReadTargetFramework(string xsprojPath)
        {
            var doc = XDocument.Load(xsprojPath);
            var element = doc.Descendants("TargetFramework").FirstOrDefault();
            if (element == null)
            {
                throw new InvalidOperationException($"No <TargetFramework> element found in {xsprojPath}.");
            }
            string tfm = element.Value;

            // The out-of-process Designer's host-process launcher requires a Windows-Desktop
            // TFM (observed empirically as a "Timed out while connecting to the named pipe"
            // failure otherwise) -- the companion project is independent of the real one, so
            // just always force "-windows" here rather than matching the real TFM exactly.
            if (tfm.IndexOf("-windows", StringComparison.OrdinalIgnoreCase) < 0)
            {
                tfm += "-windows";
            }
            return tfm;
        }

        private static string BuildCsprojContent(
            string targetFramework, IReadOnlyList<string> referencePaths, IReadOnlyList<PackageRef> packageReferences)
        {
            var sb = new StringBuilder();
            sb.AppendLine("<Project Sdk=\"Microsoft.NET.Sdk\">");
            sb.AppendLine();
            sb.AppendLine("  <!-- Auto-generated by the X# WinForms Designer shadow-file bridge. Do not");
            sb.AppendLine("       edit by hand: regenerated whenever the Designer is opened for this form. -->");
            sb.AppendLine();
            sb.AppendLine("  <PropertyGroup>");
            sb.AppendLine("    <OutputType>Library</OutputType>");
            sb.AppendLine($"    <TargetFramework>{targetFramework}</TargetFramework>");
            sb.AppendLine("    <UseWindowsForms>true</UseWindowsForms>");
            sb.AppendLine("    <Nullable>disable</Nullable>");
            sb.AppendLine("    <ImplicitUsings>disable</ImplicitUsings>");
            sb.AppendLine("  </PropertyGroup>");

            if (packageReferences.Count > 0)
            {
                sb.AppendLine();
                sb.AppendLine("  <ItemGroup>");
                foreach (var pkg in packageReferences)
                {
                    if (string.IsNullOrEmpty(pkg.Version))
                    {
                        sb.AppendLine($"    <PackageReference Include=\"{Escape(pkg.Include)}\" />");
                    }
                    else
                    {
                        sb.AppendLine($"    <PackageReference Include=\"{Escape(pkg.Include)}\" Version=\"{Escape(pkg.Version)}\" />");
                    }
                }
                sb.AppendLine("  </ItemGroup>");
            }

            if (referencePaths.Count > 0)
            {
                sb.AppendLine();
                sb.AppendLine("  <ItemGroup>");
                foreach (var path in referencePaths)
                {
                    string name = Path.GetFileNameWithoutExtension(path);
                    sb.AppendLine($"    <Reference Include=\"{Escape(name)}\">");
                    sb.AppendLine($"      <HintPath>{Escape(path)}</HintPath>");
                    sb.AppendLine("    </Reference>");
                }
                sb.AppendLine("  </ItemGroup>");
            }

            sb.AppendLine();
            sb.AppendLine("</Project>");
            return sb.ToString();
        }

        internal static List<PackageRef> ReadPackageReferences(string xsprojPath)
        {
            var doc = XDocument.Load(xsprojPath);
            var result = new List<PackageRef>();
            foreach (var el in doc.Descendants("PackageReference"))
            {
                string include = el.Attribute("Include")?.Value;
                if (string.IsNullOrEmpty(include)) continue;
                string version = el.Attribute("Version")?.Value
                    ?? el.Element("Version")?.Value;
                result.Add(new PackageRef(include, version));
            }
            return result;
        }

        private static string BuildStubContent(string namespaceName, string className)
        {
            return
$@"namespace {namespaceName}
{{
    public partial class {className} : System.Windows.Forms.Form
    {{
    }}
}}
";
        }

        private static string Escape(string value) => System.Security.SecurityElement.Escape(value);

        private static void WriteIfChanged(string path, string content)
        {
            // Avoids spurious "file changed outside the environment, reload?" prompts in VS
            // on re-runs that produce byte-identical output.
            if (File.Exists(path) && File.ReadAllText(path) == content)
            {
                return;
            }
            File.WriteAllText(path, content);
        }
    }
}
