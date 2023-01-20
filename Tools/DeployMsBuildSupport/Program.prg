USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.IO
USING System.Security.Principal

FUNCTION Start(args as string[]) AS VOID STRICT
    local aFolders as List<String>
    local cPf as string
    local XSharpDir as string
    local uninstall := false as logic
    IF args?:Length > 0
        uninstall := args:Count( { sArg => sArg:ToUpper():Contains("UNINSTALL") }) > 0
    ENDIF
    var xsfile := "XSharp.BeforeCommon.Props"
    cPf := System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFiles)
    if ! IsAdministrator()
        Console.WriteLine("This program must be run as administrator")
        return
    endif
    GetSettingString("XSharpPath", out xsharpdir,"")
    if String.IsNullOrEmpty(xSharpDir)
         Console.WriteLine("Could not locate XSharp Installation")
        return
    endif
    xsharpDir := Path.Combine(xsharpdir, "MsBuild")+"\"
    var sourceFile := Path.Combine(xsharpDir, xsfile)
    if ! System.IO.File.Exists(sourcefile)
        Console.WriteLine("Cannot find the file "+sourcefile)
        return
    endif
    if uninstall
        Console.WriteLine("Removing X# MSBuild support from  VS folders and .Net folders")
    else
        Console.WriteLine("Deploying X# MSBuild support to VS folders and .Net folders")
        Console.WriteLine("Source file: "+sourcefile )
    endif
    var dirs := Directory.GetDirectories(cPF+"\Microsoft Visual Studio")
    aFolders := List<String>{}
    foreach var dir in dirs
        var subdirs := Directory.GetDirectories(dir)
        foreach var subdir in subdirs
            if subdir:Contains("\20")
                aFolders:Add(subdir+"\MsBuild\Current")
            endif
        next
    next
    cPf := System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
    dirs := Directory.GetDirectories(cPF+"\Microsoft Visual Studio")
    foreach var dir in dirs
        var subdirs := Directory.GetDirectories(dir)
        foreach var subdir in subdirs
            if subdir:Contains("\20")
                aFolders:Add(subdir+"\MsBuild\Current")
            endif
        next
    next

    dirs := Directory.GetDirectories("c:\Program Files\dotnet\sdk")
    foreach var dir in dirs
        aFolders:Add(dir+"\Current")
    next

    foreach var dir in aFolders
        var path := dir+"\Imports\Microsoft.Common.Props\ImportBefore\"
        if uninstall
            if Directory.Exists(path)
                IF File.Exists(path+xsfile)
                    File.Delete(path+xsfile)
                ENDIF
                RemoveTree(path)
            endif

        else
            if ! Directory.Exists(path)
                Console.WriteLine("Creating "+path)
                Directory.CreateDirectory(path)
            endif
            System.IO.File.Copy(sourceFile, path+xsfile,true)
        endif

        path := path:Replace("Imports\Microsoft.Common.Props\ImportBefore\","")
        if uninstall
            Console.WriteLine("Removing from "+ path)
        else
            Console.WriteLine("Copying to "+ path)
        endif
    next

function IsAdministrator() as logic
    var identity := WindowsIdentity.GetCurrent()
    var principal := WindowsPrincipal{identity}
    return principal.IsInRole(WindowsBuiltInRole.Administrator)

function GetSettingString<T>(name as string,  result out T, def as T) as logic
    var key := Microsoft.Win32.Registry.LocalMachine
    result := def
    var subkey := key:OpenSubKey(XSharp.Constants.RegistryKey64, true)
    if subkey == null
        subkey := key.CreateSubKey(XSharp.Constants.RegistryKey64, true)
    endif
    var oResult := subkey:GetValue(name)
    if oResult == null
        return false
    endif
    result := (T) oResult
    return true

function RemoveTree(path as string) as void
    if path:EndsWith("\")
        path := path:Substring(0, Path:Length-1)
    endif
    var files := Directory.GetFiles(path)
    var dirs  := Directory.GetDirectories(path)
    do while files:Length == 0 .and. dirs:Length == 0
        Directory.Delete(path)
        path := Path:Substring(0, path:LastIndexOf("\"))
        files := Directory.GetFiles(path,"*.*")
        dirs  := Directory.GetDirectories(path)
    enddo
    return

