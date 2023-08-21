/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution. 
 * 
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System.Diagnostics;


namespace Microsoft.VisualStudio.Project
{
    public  class CCITracing
    {
        private CCITracing() { }

        [ConditionalAttribute("Enable_CCIDiagnostics")]
        static void InternalTraceCall(int levels)
        {
            System.Diagnostics.StackFrame stack;
            stack = new System.Diagnostics.StackFrame(levels);
            System.Reflection.MethodBase method = stack.GetMethod();
            if(method != null)
            {
                string name = method.Name + " \tin class " + method.DeclaringType.Name;
                Logger.Information("Call Trace: \t" + name);
            }
        }

        [ConditionalAttribute("CCI_TRACING")]
        static public void TraceCall()
        {
            // skip this one as well
            CCITracing.InternalTraceCall(2);
        }

        [ConditionalAttribute("CCI_TRACING")]
        static public void TraceCall(string strParameters)
        {
            CCITracing.InternalTraceCall(2);
            Logger.Information("\tParameters: \t" + strParameters);
        }

        [ConditionalAttribute("CCI_TRACING")]
        static public void Trace(System.Exception e)
        {
            CCITracing.InternalTraceCall(2);
            Logger.Exception(e,"");
        }

        [ConditionalAttribute("CCI_TRACING")]
        static public void Trace(string strOutput)
        {
            Logger.Information(strOutput);
        }

        [ConditionalAttribute("CCI_TRACING")]
        static public void TraceData(string strOutput)
        {
            Logger.Information("Data Trace: \t" + strOutput);
        }

        [ConditionalAttribute("Enable_CCIFileOutput")]
        [ConditionalAttribute("CCI_TRACING")]
        static public void AddTraceLog(string strFileName)
        {
            TextWriterTraceListener tw = new TextWriterTraceListener("c:\\mytrace.log");
            System.Diagnostics.Trace.Listeners.Add(tw);
        }
    }
}
