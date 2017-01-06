/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and   
limitations under the License.
*/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSTestCodeAnalysis
{
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = false)]
    class TestAttribute: Attribute
    {
        public TestAttribute() { }

        public string Title { get; set; }

        public string Author { get; set; }

        public string Id { get; set; }

        public string Description { get; set; }
    }

    static class TestAttributeExtensions
    {
        public static System.Reflection.CustomAttributeData GetTestAttribute(this System.Reflection.MethodInfo m) {
            return m.CustomAttributes.First(a => a.AttributeType == typeof(TestAttribute));
        }

        public static string GetTitle(this System.Reflection.CustomAttributeData a) {
            if (a.AttributeType != typeof(TestAttribute)) {
                throw new ArgumentException();
            }
            foreach(var n in a.NamedArguments)
                if (n.MemberName == "Title")
                    return n.TypedValue.Value.ToString();
            return null;
        }

        public static string GetAuthor(this System.Reflection.CustomAttributeData a) {
            if (a.AttributeType != typeof(TestAttribute)) {
                throw new ArgumentException();
            }
            foreach(var n in a.NamedArguments)
                if (n.MemberName == "Author")
                    return n.TypedValue.Value.ToString();
            return null;
        }

        public static string GetId(this System.Reflection.CustomAttributeData a) {
            if (a.AttributeType != typeof(TestAttribute)) {
                throw new ArgumentException();
            }
            foreach(var n in a.NamedArguments)
                if (n.MemberName == "Id")
                    return n.TypedValue.Value.ToString();
            return null;
        }

        public static string GetDescription(this System.Reflection.CustomAttributeData a) {
            if (a.AttributeType != typeof(TestAttribute)) {
                throw new ArgumentException();
            }
            foreach(var n in a.NamedArguments)
                if (n.MemberName == "Description")
                    return n.TypedValue.Value.ToString();
            return null;
        }
    }
}
