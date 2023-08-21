#define TRACE
using Microsoft.Build.Evaluation;
using Microsoft.Build.Execution;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;

namespace XSharp.Project
{
    public static class MSBuildProjectExtensions
    {
        public static ReadOnlyCollection<ProjectItem> ThreadSafeItems(this Microsoft.Build.Evaluation.Project thisp)
        {
            lock (thisp)
            {
                return thisp.Items.ToList().AsReadOnly();
            }
        }

        public static ProjectItem ThreadSafeAdd(this Microsoft.Build.Evaluation.Project thisp, string itemType, string unevaluatedInclude)
        {
            lock (thisp)
            {
                return thisp.AddItem(itemType, unevaluatedInclude).First();
            }
        }

        public static ICollection<ProjectItem> ThreadSafeGetItems(this Microsoft.Build.Evaluation.Project thisp, string itemType)
        {
            lock (thisp)
            {
                return thisp.GetItems(itemType);
            }
        }

        public static bool ThreadSafeRemoveItem(this Microsoft.Build.Evaluation.Project thisp, ProjectItem item)
        {
            lock (thisp)
            {
                try
                {
                    if (thisp != item.Project)
                    {
                        ICollection<ProjectItem> foundItem = thisp.GetItemsByEvaluatedInclude(item.EvaluatedInclude);
                        if (foundItem == null || foundItem.Count != 1)
                        {
                            Trace.WriteLine("Attempt to remove item with wrong project evaluated include returned with more than one item or null item");
                            return false;
                        }
                        return thisp.RemoveItem(foundItem.FirstOrDefault());
                    }
                    return thisp.RemoveItem(item);
                }
                catch (Exception)
                {
                    return false;
                }
            }
        }

        public static ProjectProperty ThreadSafeGetProperty(this Microsoft.Build.Evaluation.Project thisp, string property)
        {
            lock (thisp)
            {
                return thisp.GetProperty(property);
            }
        }

        public static string ThreadSafeGetPropertyValue(this Microsoft.Build.Evaluation.Project thisp, string property)
        {
            lock (thisp)
            {
                return thisp.GetPropertyValue(property);
            }
        }

        public static bool ThreadSafeRemoveProperty(this Microsoft.Build.Evaluation.Project thisp, ProjectProperty property)
        {
            lock (thisp)
            {
                return thisp.RemoveProperty(property);
            }
        }

        public static ProjectProperty ThreadSafeSetProperty(this Microsoft.Build.Evaluation.Project thisp, string propertyName, string propertyValue)
        {
            lock (thisp)
            {
                return thisp.SetProperty(propertyName, propertyValue);
            }
        }

        public static string ThreadSafeGetGlobalPropertyValue(this Microsoft.Build.Evaluation.Project thisp, string property)
        {
            lock (thisp.GlobalProperties)
            {
                string propValue = string.Empty;
                thisp.GlobalProperties.TryGetValue(property, out propValue);
                return propValue;
            }
        }

        public static ProjectPropertyInstance ThreadSafeGetGlobalProperty(this Microsoft.Build.Evaluation.Project thisp, string property)
        {
            lock (thisp.ProjectCollection)
            {
                return thisp.ProjectCollection.GetGlobalProperty(property);
            }
        }

        public static bool ThreadSafeSetGlobalProperty(this Microsoft.Build.Evaluation.Project thisp, string propertyName, string propertyValue)
        {
            lock (thisp)
            {
                return thisp.SetGlobalProperty(propertyName, propertyValue);
            }
        }

        public static void ThreadSafeSetCollectionGlobalProperty(this Microsoft.Build.Evaluation.Project thisp, string propertyName, string propertyValue)
        {
            lock (thisp)
            {
                thisp.ProjectCollection.SetGlobalProperty(propertyName, propertyValue);
            }
        }

        public static string ThreadSafeExpandString(this Microsoft.Build.Evaluation.Project thisp, string path)
        {
            lock (thisp)
            {
                return thisp.ExpandString(path);
            }
        }

        public static ProjectInstance ThreadSafeCreateProjectInstance(this Microsoft.Build.Evaluation.Project thisp)
        {
            lock (thisp)
            {
                return thisp.CreateProjectInstance();
            }
        }
    }
}
