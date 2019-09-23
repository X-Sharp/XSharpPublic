using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DocumentationChanger
{
    class Program
    { 
        static void Main(string[] args)
        {
            // Check which modes all need to be compiled
            bool htmlHelp1 = false, MsHelpViewer = false, Website = false;
            //string basePath = @"c:\XSharp\DevRt\Binaries\Obj\VOHelp";
            string basePath = @"c:\XSharp\DevRt\Binaries\Obj\Help";
            string htmlHelp1Path = @"\Output\HtmlHelp1";
            string MsHelpViewerPath = @"\Output\MsHelpViewer";
            string websitePath = @"\Output\Website";
            if (Directory.Exists(basePath + htmlHelp1Path)) { htmlHelp1 = true; }
            if (Directory.Exists(basePath + MsHelpViewerPath)) { MsHelpViewer = true; }
            if (Directory.Exists(basePath + websitePath)) { Website = true; }
            string htmlPath = @"\html";
            if (htmlHelp1)
            {
                Console.WriteLine("Editing htmlHelp1 files...");
                string htmlFolderPath = basePath + htmlHelp1Path + htmlPath;
                string[] hhcPaths = Directory.GetFiles(basePath, "*.hhc", SearchOption.TopDirectoryOnly);
                string[] hhkPaths = Directory.GetFiles(basePath, "*.hhk", SearchOption.TopDirectoryOnly);

                Console.WriteLine("  Checking if html folder exists...");
                if (Directory.Exists(htmlFolderPath))
                {
                    Console.WriteLine("  Editing html topic pages...");
                    FileEditor.editHtmlFolder(htmlFolderPath);
                }
                else
                {
                    Console.WriteLine("  Could not find html folder!");
                }
                Console.WriteLine("  Checking if a TOC for HtmlHelp1 exists...");
                if (hhcPaths.Length>0)
                {
                    Console.WriteLine("  Editing {0} TOC's for HtmlHelp1...",hhcPaths.Length);
                    foreach (string hhc in hhcPaths)
                    {
                        FileEditor.editHhc(hhc);
                        FileEditor.editForTypeNames(hhc);
                    }  
                }
                else
                {
                    Console.WriteLine("   Found no TOC for HtmlHelp1");
                }
                
                Console.WriteLine("  Checking if index file for HtmlHelp1 exists...");
                if (hhkPaths.Length>0)
                {
                    Console.WriteLine("  Editing {0} index file(s) for HtmlHelp1...",hhkPaths.Length);
                    foreach (string hhk in hhkPaths)
                    {
                        FileEditor.editHhk(hhk);
                        FileEditor.editForTypeNames(hhk);
                    }
                }
                else
                {
                    Console.WriteLine("  Found no index file for HtmlHelp1");
                }
            }
            if (MsHelpViewer)
            {
                Console.WriteLine("Editing MsHelpViewer files...");
                string htmlFolderPath = basePath + MsHelpViewerPath + htmlPath;
                Console.WriteLine("  Checking if html folder exists...");
                if (Directory.Exists(htmlFolderPath))
                {
                    Console.WriteLine("  Editing html topic pages...");
                    FileEditor.editHtmlFolder(htmlFolderPath);
                    Console.WriteLine("   Editing TOC and index");
                    FileEditor.editHtmlFolderForMSHV(htmlFolderPath);
                }
                else
                {
                    Console.WriteLine("  Could not find html folder!");
                }
            }
            if (Website)
            {
                Console.WriteLine("Editing Website files...");
                string htmlFolderPath = basePath + websitePath + htmlPath;
                Console.WriteLine("  Checking if html folder exists...");
                if (Directory.Exists(htmlFolderPath))
                {
                    Console.WriteLine("  Editing html topic pages...");
                    FileEditor.editHtmlFolder(htmlFolderPath);
                    Console.WriteLine("  Editing website TOC...");
                    FileEditor.editForWebsiteFolder(htmlFolderPath);
                }
            }
            if (!htmlHelp1 && !MsHelpViewer && !Website)
            {
                Console.WriteLine("Could not find any build types, terminating...");
                return;
            }
            Console.WriteLine("Done Editing files!");
            Console.Read();
        }
    }
}
