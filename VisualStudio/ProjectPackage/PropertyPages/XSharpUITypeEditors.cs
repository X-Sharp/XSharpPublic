//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Windows.Forms;
using System.ComponentModel;
using System.Drawing.Design;
using System.Windows.Forms.Design;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
   class XSharpMLEPropertyEditor : UITypeEditor
	{
      private XSharpMLEPropertyForm _editorUI;

      public XSharpMLEPropertyEditor() : base()
      {

      }

      public override object EditValue( ITypeDescriptorContext context, IServiceProvider provider, object value )
      {
         if ( provider != null )
         {
            IWindowsFormsEditorService edSvc = (IWindowsFormsEditorService) provider.GetService( typeof( IWindowsFormsEditorService ) );

            if ( edSvc != null )
            {
               if ( _editorUI == null )
               {
                  _editorUI = new XSharpMLEPropertyForm();
               }

               _editorUI.Text = context.PropertyDescriptor.DisplayName;  // we can also use the Attributes property to pass in information via an attribute
               _editorUI.PropertyText.Text = (string) value;
               _editorUI.PropertyText.Select();
               _editorUI.PropertyText.DeselectAll();
               _editorUI.PropertyText.ClearUndo();

               XBuildMacroCollection mc = new XBuildMacroCollection( (ProjectNode) ( (SettingsPage) context.Instance ).ProjectMgr );
               _editorUI.SetMacros( mc );

               DialogResult result = edSvc.ShowDialog( _editorUI );

               if ( result == DialogResult.OK )
               {
                  value = _editorUI.PropertyText.Text;
               }
            }
         }

         return value;
      }

      public override UITypeEditorEditStyle GetEditStyle( ITypeDescriptorContext context )
      {
         return UITypeEditorEditStyle.Modal;
      }

      public override bool GetPaintValueSupported( ITypeDescriptorContext context )
      {
         return false;
      }
   }

   class XSharpSLEPropertyEditor : UITypeEditor
   {
      private XSharpSLEPropertyForm _editorUI;

      public XSharpSLEPropertyEditor()
         : base()
      {

      }

      public override object EditValue( ITypeDescriptorContext context, IServiceProvider provider, object value )
      {
         if ( provider != null )
         {
            IWindowsFormsEditorService edSvc = (IWindowsFormsEditorService) provider.GetService( typeof( IWindowsFormsEditorService ) );

            if ( edSvc != null )
            {
               if ( _editorUI == null )
               {
                  _editorUI = new XSharpSLEPropertyForm();
               }

               _editorUI.Text = context.PropertyDescriptor.DisplayName;  // we can also use the Attributes property to pass in information via an attribute
               _editorUI.PropertyText.Text = (string) value;
               _editorUI.PropertyText.Select();
               _editorUI.PropertyText.DeselectAll();
               _editorUI.PropertyText.ClearUndo();

                    XBuildMacroCollection mc = new XBuildMacroCollection( (ProjectNode) ( (SettingsPage) context.Instance ).ProjectMgr );
               _editorUI.SetMacros( mc );

               DialogResult result = edSvc.ShowDialog( _editorUI );

               if ( result == DialogResult.OK )
               {
                  value = _editorUI.PropertyText.Text;
               }
            }
         }

         return value;
      }

      public override UITypeEditorEditStyle GetEditStyle( ITypeDescriptorContext context )
      {
         return UITypeEditorEditStyle.Modal;
      }

      public override bool GetPaintValueSupported( ITypeDescriptorContext context )
      {
         return false;
      }
   }
}
