using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public static class XSolution
    {
        static Dictionary<string, XProject> xProjects;

        static XSolution()
        {
            xProjects = new Dictionary<string, XProject>();
        }

        public static bool Add(string projectName, XProject project)
        {
            projectName = projectName.ToLower();
            if (xProjects.ContainsKey(projectName))
            {
                return false;
            }
            xProjects.Add(projectName, project);
            return true;
        }

        public static bool Add( XProject project)
        {
            string projectName = project.Name;
            return XSolution.Add(projectName, project);
        }

        public static bool Remove(string projectName)
        {
            projectName = projectName.ToLower();
            if (xProjects.ContainsKey(projectName))
            {
                xProjects.Remove(projectName);
                return true;
            }
            return false;
        }

        public static XFile FindFile(string fileName)
        {
            XFile file = null;
            foreach (KeyValuePair<string, XProject> project in xProjects)
            {
                XProject prj = project.Value;
                file = prj.Find( fileName );
                if ( file != null)
                {
                    break;
                }
            }
            return file;
        }

        public static XFile FindFullPath(string fullPath)
        {
            XFile file = null;
            foreach (KeyValuePair<string, XProject> project in xProjects)
            {
                XProject prj = project.Value;
                file = prj.FindFullPath(fullPath);
                if (file != null)
                {
                    break;
                }
            }
            return file;
        }

        public static XProject FindProject(string projectFile )
        {
            XProject project;
            if (xProjects.TryGetValue(projectFile.ToLower(), out project))
            {
                return project;
            }
            return null;
        }
    }
}
