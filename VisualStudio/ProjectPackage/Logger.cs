using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Serilog;
using System.IO;
using XSharpModel;
using System.ComponentModel;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using System.Diagnostics;

namespace XSharp.Project
{
    internal static class Logger
    {
        static bool log2debugger = false;
        static bool log2file = false;

        private static string singleline = new string('-', 80);
        private static string doubleline = new string('=', 80);

        static bool active = false;
        internal static bool Active => active;
        internal static void Start()
        {
            if (!active ||
                log2debugger  != XSettings.EnableDebugLogging ||
                log2file != XSettings.EnableFileLogging)
            {
                if (active)
                {
                    Stop();
                }
                var config = new LoggerConfiguration()
                        .MinimumLevel.Debug();
                log2debugger = false;
                log2file = false;
                if (XSettings.EnableDebugLogging)
                {
                    config = config.WriteTo.Debug();
                    log2debugger = true;
                }
                if (XSettings.EnableFileLogging)
                {
                    var temp = Path.GetTempPath();
                    temp = Path.Combine(temp, "XSharp.Intellisense");
                    if (!Directory.Exists(temp))
                    {
                        Directory.CreateDirectory(temp);
                    }
#pragma warning disable CS0618 // Type or member is obsolete
                    int threadid = AppDomain.GetCurrentThreadId();
#pragma warning restore CS0618 // Type or member is obsolete
                    string strId = threadid.ToString("X");
                    var log = Path.Combine(temp, "Project_"+strId+"_.log");
                    config = config.WriteTo.File(log,
                        rollingInterval: RollingInterval.Day,
                        rollOnFileSizeLimit: true,
                        flushToDiskInterval: TimeSpan.FromSeconds(15),
                        retainedFileCountLimit: 5);
                    log2file = true;
                }
                
                
                Log.Logger = config.CreateLogger();

                Log.Information(doubleline);
                Log.Information("Started Logging");
                string version="";
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    var ver = await VS.Shell.GetVsVersionAsync();
                    version = ver.ToString();
                });
                Log.Information("Visual Studio Exe     : " + System.Reflection.Assembly.GetExecutingAssembly().Location);
                Log.Information("Visual Studio version : " + version);
                Log.Information("XSharp Project System : " + Constants.FileVersion);
                

                Log.Information(doubleline);
                var sol = VS.Solutions.GetCurrentSolution();
                if (sol != null)
                {
                    Log.Information(singleline);
                    Log.Information("Current solution: " + sol.FullPath);

                    var children = EnumChildren(sol);
                    foreach (var child in children)
                    {
                        if (child.Type == SolutionItemType.Project)
                        {
                            Log.Information("Project " + child.FullPath);
                        }
                    }
                    Log.Information(singleline);

                }

                AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
                //AppDomain.CurrentDomain.FirstChanceException += CurrentDomain_FirstChanceException;
                active = true;
            }
            // Force all Logging options to be enabled
            XSettings.EnableBraceMatchLog = true;
            XSettings.EnableCodeCompletionLog = true;
            XSettings.EnableDatabaseLog = true;
            XSettings.EnableParameterLog = true;
            XSettings.EnableParseLog = true;
            XSettings.EnableQuickInfoLog = true;
            XSettings.EnableReferenceInfoLog = true;
            XSettings.EnableTypelookupLog = true;
        }

        static IList<SolutionItem> EnumChildren(SolutionItem item)
        {
            var items = new List<SolutionItem>();
            foreach (var child in item.Children)
            {
                items.Add(child);
                try
                {
                    items.AddRange(EnumChildren(child));
                }
                catch (Exception e)
                {
                    Exception(e, "EnumChildren");
                }
            }
            return items;
        }

        private static void CurrentDomain_FirstChanceException(object sender, System.Runtime.ExceptionServices.FirstChanceExceptionEventArgs e)
        {

            if (active)
            {
                if (e.Exception is OperationCanceledException)
                {
                    ; // do nothing
                }
                else if (e.Exception is Win32Exception)
                {
                    ; // do nothing
                }
                else
                {
                    Log.Error(e.Exception, "FirstChanceException");
                }
            }
        }

        private static void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            if (active)
            {
                if (e.ExceptionObject is Exception ex)
                    Log.Fatal(ex, "UnhandledException");
            }
        }


        internal static void Stop()
        {
            if (active)
            {
                Log.Information(doubleline);
                Log.Information("Stopped logging");
                Log.Information(doubleline);

                Log.CloseAndFlush();
                active = false;
                AppDomain.CurrentDomain.UnhandledException -= CurrentDomain_UnhandledException;
                //AppDomain.CurrentDomain.FirstChanceException -= CurrentDomain_FirstChanceException;
            }
        }

        internal static void Debug(string message)
        {
            if (active)
                Log.Debug(message);

        }
        internal static void Information(string message)
        {
            if (active)
                Log.Information(message);
        }


        internal static void Exception(Exception e, string message)
        {
            if (active)
                Log.Error(e, message);
        }
        internal static void SingleLine()
        {
            Information(singleline);
        }
        internal static void DoubleLine()
        {
            Information(doubleline);
        }

    }
}
