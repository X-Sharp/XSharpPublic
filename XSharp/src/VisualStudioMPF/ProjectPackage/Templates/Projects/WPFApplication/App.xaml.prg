#using System
#using System.Collections.Generic
#using System.Configuration
#using System.Data
$if$ ($targetframeworkversion$ >= 3.5)#using System.Linq
$endif$$if$ ($targetframeworkversion$ >= 4.5)#using System.Threading.Tasks
$endif$#using System.Windows


BEGIN NAMESPACE $safeprojectname$

   /// <summary>
   /// Interaction logic for App.xaml
   /// </summary>
   PARTIAL CLASS App INHERIT Application

   END CLASS

END NAMESPACE
