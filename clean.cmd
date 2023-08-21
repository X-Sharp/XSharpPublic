java -jar bfg-1.14.0.jar  --delete-files XSharp.CodeAnalysis.dll
java -jar bfg-1.14.0.jar  --delete-files XSharp.CodeAnalysis.pdb
java -jar bfg-1.14.0.jar  --delete-files XSharp.VSParser.dll
java -jar bfg-1.14.0.jar  --delete-files XSharp.VSParser.pdb
java -jar bfg-1.14.0.jar  --delete-files XSharp.Evaluator.dll
java -jar bfg-1.14.0.jar  --delete-files XSharp.Evaluator.pdb
java -jar bfg-1.14.0.jar  --delete-files XSharp.MonoCecil.dll
java -jar bfg-1.14.0.jar  --delete-files XSharp.MonoCecil.pdb
git reflog expire --expire=now --all && git gc --prune=now --aggressive
