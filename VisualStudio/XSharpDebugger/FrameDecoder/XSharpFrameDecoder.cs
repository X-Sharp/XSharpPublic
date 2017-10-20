// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.CallStack;
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using System;
using System.Text;

namespace XSharpDebugger.FrameDecoder
{
    /// <summary>
    /// This class is the entry point into the Frame Decoder.  The frame decoder is used to provide
    /// the text shown in the Call Stack window or other places in the debugger UI where stack
    /// frames are used.  See the method comments below for more details about each method.
    /// </summary>
    public sealed class XSharpFrameDecoder : IDkmLanguageFrameDecoder
    {
        /// <summary>
        /// This method is called by the debug engine to get the text representation of a stack
        /// frame.
        /// </summary>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <param name="workList">The current work list.  This is used to batch asynchronous
        /// work items.  If any asynchronous calls are needed later, this is the work list to pass
        /// to the asynchronous call.  It's not needed in our case.</param>
        /// <param name="frame">The frame to get the text representation for</param>
        /// <param name="argumentFlags">Option flags to change the way we format frames</param>
        /// <param name="completionRoutine">Completion routine to call when work is completed</param>
        void IDkmLanguageFrameDecoder.GetFrameName(
            DkmInspectionContext inspectionContext,
            DkmWorkList workList,
            DkmStackWalkFrame frame,
            DkmVariableInfoFlags argumentFlags,
            DkmCompletionRoutine<DkmGetFrameNameAsyncResult> completionRoutine)
        {
            if (frame is DkmStackFrame)
            {
                var dsf = frame as DkmStackFrame;
                var name = dsf.FrameName;
                name = name.Replace("Xs$Args", "arguments");
                completionRoutine(new DkmGetFrameNameAsyncResult(name));
            }
            else
            {
                inspectionContext.GetFrameName(workList, frame, argumentFlags, completionRoutine);
            }
            //string name = TryGetFrameNameHelper(inspectionContext, frame, argumentFlags) ?? "<Unknown Method>";
            //completionRoutine(new DkmGetFrameNameAsyncResult(name));
        }

        /// <summary>
        /// This method is called by the debug engine to get the text representation of the return
        /// value of a stack frame.
        /// </summary>
        /// <param name="inspectionContext">Context of the evaluation.  This contains options/flags
        /// to be used during compilation. It also contains the InspectionSession.  The inspection
        /// session is the object that provides lifetime management for our objects.  When the user
        /// steps or continues the process, the debug engine will dispose of the inspection session</param>
        /// <param name="workList">The current work list.  This is used to batch asynchronous
        /// work items.  If any asynchronous calls are needed later, this is the work list to pass
        /// to the asynchronous call.  It's not needed in our case.</param>
        /// <param name="frame">The frame to get the text representation of the return value for</param>
        /// <param name="completionRoutine">Completion routine to call when work is completed</param>
        void IDkmLanguageFrameDecoder.GetFrameReturnType(
            DkmInspectionContext inspectionContext,
            DkmWorkList workList,
            DkmStackWalkFrame frame,
            DkmCompletionRoutine<DkmGetFrameReturnTypeAsyncResult> completionRoutine)
        {

            if (frame is DkmStackFrame)
            {
                var dsf = frame as DkmStackFrame;
                var name = dsf.ReturnType;
                completionRoutine(new DkmGetFrameReturnTypeAsyncResult(name));
            }
            else
            {
                //string name = TryGetFrameReturnTypeHelper(inspectionContext, frame) ?? "<Unknown>";
                //completionRoutine(new DkmGetFrameReturnTypeAsyncResult(name));
                inspectionContext.GetFrameReturnType(workList, frame, completionRoutine);
            }
        }

        private static string TryGetFrameReturnTypeHelper(DkmInspectionContext inspectionContext, DkmStackWalkFrame frame)
        {
            ImportedMethod currentMethod = TryGetCurrentMethod(inspectionContext, frame);
            if (currentMethod == null)
                return null;

            return currentMethod.ReturnType.ToString();
        }

        private static string TryGetFrameNameHelper(DkmInspectionContext inspectionContext, DkmStackWalkFrame frame, DkmVariableInfoFlags argumentFlags)
        {
            ImportedMethod currentMethod = TryGetCurrentMethod(inspectionContext, frame);
            if (currentMethod == null)
                return null;

            string name = currentMethod.Name;
            if (string.Equals(name, "$.main", StringComparison.Ordinal))
                return "<Main Block>";

            if (argumentFlags == DkmVariableInfoFlags.None)
                return name;

            Variable[] args = currentMethod.GetParameters();
            if (args.Length == 0)
                return name;

            StringBuilder nameBuilder = new StringBuilder();
            nameBuilder.Append(name);
            nameBuilder.Append('(');

            bool first = true;
            bool showTypes = argumentFlags.HasFlag(DkmVariableInfoFlags.Types);
            bool showNames = argumentFlags.HasFlag(DkmVariableInfoFlags.Names);
            foreach (Variable arg in args)
            {
                if (first)
                    first = false;
                else
                    nameBuilder.Append("; ");

                IrisType argType = arg.Type;
                if (argType.IsByRef)
                {
                    nameBuilder.Append("var ");
                    argType = argType.GetElementType();
                }

                if (showNames)
                    nameBuilder.Append(arg.Name);

                if (showNames && showTypes)
                    nameBuilder.Append(" : ");

                if (showTypes)
                    nameBuilder.Append(argType);
            }

            nameBuilder.Append(')');
            return nameBuilder.ToString();
        }

        private static ImportedMethod TryGetCurrentMethod(DkmInspectionContext inspectionContext, DkmStackWalkFrame frame)
        {
            InspectionSession session = InspectionSession.GetInstance(inspectionContext.InspectionSession);
            InspectionScope scope = session.GetScope((DkmClrInstructionAddress)frame.InstructionAddress);

            return scope.TryImportCurrentMethod();
        }
    }
}
