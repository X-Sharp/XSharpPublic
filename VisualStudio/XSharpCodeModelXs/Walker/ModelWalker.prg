//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using LanguageService.CodeAnalysis.XSharp.SyntaxParser
using LanguageService.SyntaxTree
using LanguageService.SyntaxTree.Misc
using LanguageService.SyntaxTree.Tree
using System
using System.Collections.Generic
using System.Diagnostics
using System.Linq
using System.Text
using System.Threading
using System.Threading.Tasks
using System.Collections.Concurrent

begin namespace XSharpModel
	class ModelWalker
		// Fields
		private _projects := ConcurrentQueue<XProject>{} as ConcurrentQueue<XProject>
		private _projectsForTypeResolution := ConcurrentQueue<XProject>{} as ConcurrentQueue<XProject>
		static private _walker as ModelWalker
		private _WalkerThread as System.Threading.Thread
		static private suspendLevel  as long
		
		// Methods
		
		static constructor
			suspendLevel := 0
		
		private  constructor()
			super()
		
		
		internal method AddProject(xProject as XProject) as void
			//
			WriteOutputMessage("-->> AddProject() "+xProject:Name)
			begin lock self
				//
				var lAdd2Queue := true
				foreach project as XProject in self:_projects
					//
					if (String.Equals(project:Name, xProject:Name, StringComparison.OrdinalIgnoreCase))
						//
						lAdd2Queue := false
						exit
						
					endif
				next
				if (lAdd2Queue)
					//
					self:_projects:Enqueue(xProject)
				endif
				if (! self:IsWalkerRunning .AND. ! xProject:ProjectNode:IsVsBuilding)
					//
					self:Walk()
				endif
			end lock
			WriteOutputMessage("<<-- AddProject()")
		
		internal method FileWalk(file as XFile) as void
			var lastWriteTime := System.IO.File.GetLastWriteTime(file:SourcePath)
			if lastWriteTime > file:LastWritten
				//
				begin using var walker := SourceWalker{file}
					try
						//
						var lines := System.IO.File.ReadAllLines(file:SourcePath)
						var xTree := walker:Parse(lines, false)
						walker:BuildModel(xTree)
						file:LastWritten := lastWriteTime
						if file:Project != null
							//
							if file:Project:FileWalkComplete != null
								//
								file:Project:FileWalkComplete?:Invoke(file)
							endif
						endif
						return
						
					catch e as Exception
						XSolution.WriteException(e)	
					finally
						if walker != null
							//
							walker:Dispose()
						endif
					end try
				end using
			endif
		
		static method GetWalker() as ModelWalker
			//
			if (ModelWalker._walker == null)
				//
				ModelWalker._walker := ModelWalker{}
			endif
			return ModelWalker._walker
		
		static method Resume() as void
			//
			ModelWalker.suspendLevel--
		
		internal method StopThread() as void
			try
				if (self:_WalkerThread == null)
					return
				endif
				if (self:_WalkerThread:IsAlive)
					self:_WalkerThread:Abort()
				endif
			CATCH exception AS System.Exception
				WriteOutputMessage("Cannot stop Background walker Thread : ")
				XSolution.WriteException(exception)
			end try
			self:_WalkerThread := null
		
		static method Suspend() as void
			//
			ModelWalker.suspendLevel++
		
		method Walk() as void
			local start as System.Threading.ThreadStart
			if (ModelWalker.suspendLevel <= 0)
				try
					self:StopThread()
					start := System.Threading.ThreadStart{ self, @Walker() }
					self:_WalkerThread := System.Threading.Thread{start}
					self:_WalkerThread:IsBackground := true
					self:_WalkerThread:Priority := System.Threading.ThreadPriority.Highest
					self:_WalkerThread:Name := "ModelWalker"
					self:_WalkerThread:Start()
				catch exception as System.Exception
					WriteOutputMessage("Cannot start Background walker Thread : ")
					XSolution.WriteException(exception)
					
				end try
			endif

		internal iProcessed as long
		internal aFiles as XFile[]
		private method Walker() as void
			local project as XProject
			local parallelOptions as System.Threading.Tasks.ParallelOptions
			project := null
			if ((self:_projects:Count != 0) .AND. ! System.Linq.Enumerable.First<XProject>(self:_projects):ProjectNode:IsVsBuilding)
				do while (true)
					if (ModelWalker.suspendLevel > 0)
						if (project != null)
							self:_projects:Enqueue(project)
						endif
						exit
					endif
					begin lock self
						if ((self:_projects:Count == 0) .OR. ! self:_projects:TryDequeue( out project))
							exit
						endif
						project:ProjectNode:SetStatusBarText(String.Format("Start scanning project {0}", project:Name))
					end lock
					WriteOutputMessage("-->> Walker("+project.Name+")")
					aFiles := project:SourceFiles:ToArray()
					iProcessed := 0
					parallelOptions := ParallelOptions{}
					if (System.Environment.ProcessorCount > 1)
						parallelOptions:MaxDegreeOfParallelism := ((System.Environment.ProcessorCount * 3) / 4)
					endif
					project:ProjectNode:SetStatusBarAnimation(true, 0)
					Parallel.ForEach(aFiles, walkOneFile)
					begin lock self
						_projectsForTypeResolution:Enqueue(project)
					end lock
					project:ProjectNode:SetStatusBarText("")
					project:ProjectNode:SetStatusBarAnimation(false, 0)
					WriteOutputMessage("<<-- Walker("+project.Name+")")
				enddo


			endif
			if ((self:_projectsForTypeResolution:Count != 0) .AND. ! System.Linq.Enumerable.First<XProject>(self:_projectsForTypeResolution):ProjectNode:IsVsBuilding)
				do while (true)
					if (ModelWalker.suspendLevel > 0)
						if (project != null)
							self:_projectsForTypeResolution:Enqueue(project)
						endif
						exit
					endif
					
					begin lock self
						if ((self:_projectsForTypeResolution:Count == 0) .OR. ! self:_projectsForTypeResolution:TryDequeue( out project))
							exit
						endif
					end lock
					WriteOutputMessage("-->> Walker() Resolve types "+project:Name)
					project:ResolveReferences()
					WriteOutputMessage("<<-- Walker() Resolve types "+project:Name)
				enddo

			endif
		
		private method walkOneFile(file as XFile) as void
			var project := file:Project
			if (project:Loaded)
				iProcessed++
				do while (project:ProjectNode:IsVsBuilding)
					System.Threading.Thread.Sleep(1000)
				enddo
				project:ProjectNode:SetStatusBarText(String.Format("Walking {0} : Processing File {1} ({2} of {3})", project:Name, file:Name, iProcessed, aFiles:Length))
				self:FileWalk(file)
			endif
			do while ModelWalker.IsSuspended .AND. System.Threading.Thread.CurrentThread:IsBackground
				System.Threading.Thread.Sleep(100)
			enddo
			return

		// Properties
		property HasWork as logic
			get
				return (self:_projects:Count > 0)
			end get
		end property
		
		static property IsSuspended as logic
			get
				return ModelWalker.suspendLevel > 0
			end get
		end property
		
		property IsWalkerRunning as logic
			get
				try
					if self:_WalkerThread != null
						return self:_WalkerThread:IsAlive
					endif
				catch exception as System.Exception
					//
					WriteOutputMessage("Cannot check Background walker Thread : ")
					XSolution.WriteException(exception)
				end try
				return false
			end get
		end property
		
		STATIC METHOD WriteOutputMessage(message AS STRING) AS void
			XSolution.WriteOutputMessage("XModel.Walker "+message)
		
	end class
	
end namespace 

