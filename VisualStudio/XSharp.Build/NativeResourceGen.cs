using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.Win32;

namespace XSharp.Build {

    public class NativeResourceCompiler : ToolTask {

        const string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
        const string outputName = "NativeResources.res";
        static string InstallPath = String.Empty;
        /// <summary>
        /// Read XSharp Installation location from the Registry
        /// </summary>
        static NativeResourceCompiler() {
            try {
                InstallPath = (string)Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
            } catch(Exception) {
                // Registry entry not found ...
                InstallPath = @"C:\Program Files (x86)\XSharp";
            }
        }

        public NativeResourceCompiler() : base()
        {
            //System.Diagnostics.Debugger.Launch();
        }


        private int numberofInputFiles {
            get {
                if(this.Sources == null)
                    return 0;
                return Sources.Length;
            }

        }

        /// <summary>
        /// Combination of Output Path and the (fixed) filename NativeResources.res
        /// </summary>
        private string outputFileName;

        [return: MarshalAs(UnmanagedType.U1)]
        public override bool Execute() {
            string rcPath = GenerateFullPathToTool();
            if (!System.IO.File.Exists(rcPath)){ 
                base.Log.LogError("Cannot find the Native Resource compiler in the XSharp Bin folder", null);
                return false;
            }
            bool ok = base.Execute();
            return ok;
        }
        /// <summary>
        /// /i = include directory
        /// /fo = output directory
        /// /d DEBUG for conditional compilation
        /// rest is passed through a response file
        /// </summary>
        /// <returns></returns>
        protected override string GenerateCommandLineCommands() {
            StringBuilder cmdline = null;
            cmdline = new StringBuilder();
            cmdline.Append("/i \"" + this.XSharpIncludedir + "\" /x ");
            cmdline.Append("/fo \"" + this.outputFileName + "\"");
            if(EmitDebugInformation)
                cmdline.Append(" /dDEBUG");
            else
                cmdline.Append(" /dNDEBUG");
             return cmdline.ToString();
        }

        protected override string GenerateFullPathToTool() {
            return FindRc(this.ToolName);
        }

        private String _compilerPath;
        public String CompilerPath {
            set { _compilerPath = value; }
            get { return _compilerPath; }
        }

        private string FindRc(string toolName) {
            if(string.IsNullOrEmpty(CompilerPath)) {
                // Nothing in the Registry ?
                if(!string.IsNullOrEmpty(InstallPath)) {
                    CompilerPath = Utilities.AddSlash(InstallPath) + "Bin\\";
                }
             }
            var rcPath = Path.Combine(Path.GetDirectoryName(CompilerPath), toolName);
            if(File.Exists(rcPath)) {
                // The tool has been found.
                return rcPath;
            }
            // Return the tool name itself.
            // Windows will search common paths for the tool.
            return toolName;
        }

        /// <summary>
        /// Response file contains list of #include statements.
        /// All resources must be compiled as one virtual large file.
        /// We can only include one native resource
        /// </summary>
        /// <returns></returns>
        protected override string GenerateResponseFileCommands() {
            StringBuilder cmds = null;
            string Result = null;
            cmds = new StringBuilder();
            if (this.numberofInputFiles > 0) {
                foreach(var item in this.Sources) {
                    try {
                        var fileName = item.GetMetadata("Fullpath");
                        cmds.Append("#include \"");
                        cmds.Append(fileName);
                        cmds.Append("\"\r\n");
                    } catch(Exception e) {
                        base.Log.LogErrorFromException(e);
                    }
                }
            }
            if(cmds.Length > 0) {
                Result = cmds.ToString();
            }
            return Result;
        }

        protected override string GetResponseFileSwitch(string responseFilePath) {
            return ("\"" + responseFilePath + "\"");
        }

        protected override string GetWorkingDirectory() {
            return null;
        }

        /// <summary>
        /// Check a list of files and their included files to see if they are newer than the output file name
        /// </summary>
        /// <param name="fileNames">List of files</param>
        /// <param name="outputFileName">outputFileName</param>
        /// <returns></returns>
        protected bool FilesAreNewer(List<string> fileNames, string outputFileName) {
            //System.Diagnostics.Debugger.Launch();
            string[] includeDirectories = null;
            string IncludeDir = XSharpIncludedir;
            DateTime outputTime = File.GetLastWriteTime(outputFileName);
            if(!string.IsNullOrEmpty(IncludeDir)) {
                includeDirectories = IncludeDir.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
            }
            var alreadychecked = new List<string>();
            foreach (var fileName in fileNames) {
                if (! System.IO.File.Exists(fileName)) {
                    base.Log.LogError("Input file: {0} not found", fileName);
                    return true;
                }
                DateTime fileTime = File.GetLastWriteTime(fileName);
                if(fileTime > outputTime) {
                    base.Log.LogMessage("Input file: {0} is newer than output file {1}, last updated on {2:f}" , fileName,outputFileName, fileTime);
                    return true;
                }
                // check file for include files
                // this can probably be done smarter using a RegEx but it works for now
                base.Log.LogMessage("Input file: {0} is older than output file {1}", fileName, outputFileName);
                var contents = System.IO.File.ReadAllLines(fileName);
                foreach(var line in contents) {
                    if(line.TrimStart().StartsWith("#include", StringComparison.OrdinalIgnoreCase)) {
                        var includefile = line.Substring(8).Trim();
                        string foundfile = "";
                        bool found = false;
                        if(includefile.StartsWith("\""))
                            includefile = includefile.Substring(1, includefile.Length - 2);
                        if(File.Exists(includefile)) {
                            foundfile = includefile;
                            found = true;
                        }
                        if(!found) {
                            // Check in the folder of the input file itself
                            if(!Path.IsPathRooted(includefile)) {
                                foundfile = Path.Combine(Path.GetDirectoryName(fileName)) + "\\"+includefile;
                                found = File.Exists(foundfile);
                            }
                        }
                        if (! found && includeDirectories.Length > 0) {
                            foreach(var includeDir in includeDirectories) {
                                foundfile = Path.Combine(includeDir, includefile);
                                found = File.Exists(foundfile);
                                if(found)
                                    break;
                            }
                        }
                        if(found) {
                            if(!alreadychecked.Contains(foundfile.ToLower())) {
                                DateTime includeTime = File.GetLastWriteTime(foundfile);
                                base.Log.LogMessage("Input file: {0}  depends on include file {1}", fileName, foundfile);
                                if(includeTime > outputTime) {
                                    base.Log.LogMessage("Include file: {0} is newer than output file {1} as was last updated on {2:f}", foundfile, outputFileName, includeTime);
                                    return true;
                                }
                                alreadychecked.Add(foundfile.ToLower());
                            }
                        }
                        else{
                            base.Log.LogError("Could not file include file {0} ", includefile);
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        /// <summary>
        /// Task does not have to run when the ouput file exists and is newer than the source files
        /// </summary>
        /// <returns></returns>
        protected override bool SkipTaskExecution() {
            bool mustCompile = false;
            if(this.numberofInputFiles > 0) {
                if(!File.Exists(this.outputFileName)) {
                    base.Log.LogMessage("Output file: {0} does not exist", outputFileName);
                    mustCompile = true;
                } else {
                    var files = new List<String>();
                    foreach(var file in Sources) {
                        string fileName = file.GetMetadata("Fullpath");
                        files.Add(fileName);
                    }
                    mustCompile = FilesAreNewer(files, outputFileName);

                }
            }
            if(!mustCompile) {
                base.Log.LogMessage("No need to recompile the resources, no file changes detected.");
            }
            return !mustCompile;
        }

        /// <summary>
        /// Check to see if all mandatory parameters are filled
        /// </summary>
        /// <returns>True when compilation can start</returns>
        protected override bool ValidateParameters() {
            string resourceCompilerExe = this.GenerateFullPathToTool();
            bool parametersValid = true;
            if(!File.Exists(resourceCompilerExe)) {
                base.Log.LogError(resourceCompilerExe + " not found.", null);
                parametersValid = false;
            }
            if(this.numberofInputFiles == 0) {
                base.Log.LogError("No input files specified", null);
                parametersValid = false;
            }
            if(this.OutputPath == null) {
                base.Log.LogError("No output path specified", null);
                return false;
            }
            this.outputFileName = this.OutputPath + outputName;
            base.Log.LogMessage("Started Creating Output file: {0}" , this.outputFileName);
            return parametersValid;
        }

        /// <summary>
        /// Additional Include Paths (next to XSharp\Include)
        /// </summary>
        public string IncludePaths { get; set; }

        /// <summary>
        /// Location of the Output file (not the name!)
        /// </summary>
        public string OutputPath { get; set; }
        
        /// <summary>
        /// Compile with Debug Info ?
        /// </summary>
        [Required]
        public bool EmitDebugInformation { get; set; }

        /// <summary>
        /// Encoding for ResponseFile
        /// </summary>
        protected override Encoding ResponseFileEncoding {
            get {
                return Encoding.ASCII;
            }
        }

        /// <summary>
        /// List of input files
        /// </summary>
        [Required]
        public ITaskItem[] Sources { get; set; }
        /// <summary>
        /// Name of the resource compiler executable
        /// </summary>
        protected override string ToolName {
            get {
                return "rc.exe";
            }
        }

        /// <summary>
        /// Combined path from IncludeDirs and the XSharp Includepath
        /// </summary>
        protected string XSharpIncludedir {
            get {
                string defincpath = null;
                string incpath = null;
                incpath = this.IncludePaths;
                defincpath = Path.Combine(InstallPath, "include");
                if(!string.IsNullOrEmpty(incpath)) {
                    defincpath = incpath + ";" + defincpath;
                }
                return defincpath;
            }
        }
    }
}


