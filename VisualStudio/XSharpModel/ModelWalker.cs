using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using LanguageService.SyntaxTree.Tree;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class ModelWalker
    {
        static ModelWalker _walker;
        static ModelWalker()
        {
        }
        static internal ModelWalker GetWalker()
        {
            if (_walker == null)
                _walker = new ModelWalker();
            return _walker;
        }

        private Queue<XProject> _projects;
        private Thread _WalkerThread;
        private ModelWalker()
        {
            _projects = new Queue<XProject>();
        }

        internal void AddProject(XProject xProject)
        {
            lock (this)
            {
                _projects.Enqueue(xProject);
                if (!IsWalkerRunning)
                {
                    Walk();
                }
            }
        }


        internal bool IsWalkerRunning
        {
            get
            {
                try
                {
                    if (_WalkerThread == null)
                        return false;
                    return _WalkerThread.IsAlive;
                }
                catch (Exception e)
                {
                    Debug.WriteLine("Cannot check Backgroung walker Thread : ");
                    Debug.WriteLine(e.Message);
                }
                return false;

            }
        }

        internal void Walk()
        {
            try
            {
                StopThread();
                ThreadStart ts = new ThreadStart(this.Walker);
                _WalkerThread = new Thread(ts);
                _WalkerThread.IsBackground = true;
                _WalkerThread.Priority = ThreadPriority.Highest;
                _WalkerThread.Name = "ModelWalker";
                _WalkerThread.Start();
            }
            catch (Exception e)
            {
                Debug.WriteLine("Cannot start Background walker Thread : ");
                Debug.WriteLine(e.Message);
            }
            return;
        }

        private void Walker()
        {
            XProject project = null;
            //
            do
            {
                // 
                lock (this)
                {
                    // need to continue ?
                    if (_projects.Count == 0)
                        break;
                    project = _projects.Dequeue();
                    //
                }
#if DEBUG
                Stopwatch stopWatch = new Stopwatch();
#endif
                foreach (XFile file in project.Files)
                {
                    //
                    project.ProjectNode.SetStatusBarText( String.Format( "Walking {0} : Processing File {1} ", project.Name, file.Name ) );
#if DEBUG                    
                    Debug.WriteLine(String.Format("Walking {0} : Processing File {1} ", project.Name, file.Name));
                    stopWatch.Start();
#endif
                    //file.Name
                    var code = System.IO.File.ReadAllText(file.FullPath);
                    var stream = new AntlrInputStream(code.ToString());
                    var lexer = new XSharpLexer(stream);
                    // if you want VO style lexing uncomment the following lines.
                    //lexer.AllowFourLetterAbbreviations = true; // enables 4 letter abbreviations
                    //lexer.AllowOldStyleComments = true; // enables && commments

                    var tokens = new CommonTokenStream(lexer);
                    var parser = new XSharpParser(tokens);
                    var tree = parser.source();
                    var walker = new ParseTreeWalker();
                    var entityparser = new EntityParser(file);
                    walker.Walk(entityparser, tree);
                    //
#if DEBUG             
                    stopWatch.Stop();       
                    Debug.WriteLine(String.Format("   needs {0}", stopWatch.Elapsed));
#endif
                }
                project.ProjectNode.SetStatusBarText("");
            } while (true);
        }

        internal void StopThread()
        {
            try
            {
                if (_WalkerThread == null)
                    return;
                if (_WalkerThread.IsAlive)
                {

                }
            }
            catch (Exception e)
            {
                Debug.WriteLine("Cannot stop Background walker Thread : ");
                Debug.WriteLine(e.Message);
            }
            _WalkerThread = null;
            return;
        }
    }


}
