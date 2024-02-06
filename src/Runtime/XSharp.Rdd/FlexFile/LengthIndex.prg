//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

begin namespace XSharp.RDD.FlexFile

internal class LengthIndex inherit Index
	protected override method TestIndexNode(ulDiskPos as DWord ) as Logic
		local flag as Logic
		local indexNode as IndexNode
		local num as DWord
		local leafKey as IndexKey
		local branchKey as BranchKey
		//
		flag := true
		indexNode := DiskCache:Get(ulDiskPos, false)
		if indexNode:bIsLeafNode
			//Init
			num := 0u
			while num < indexNode:usKeyCount
				if !flag
					exit
				endif
				leafKey := indexNode:GetLeafKey(num)
				if !area:LocIndex:Seek(leafKey:item2, leafKey:item1)
					flag := false
				endif
				//Iterators
				num++
			enddo
		else
			//Init
			num := 0u
			while num < indexNode:usKeyCount
				if !flag
					exit
				endif
				branchKey := indexNode:GetBranchKey(num)
				if !self:TestIndexNode(branchKey:ulDiskOffset)
					flag := false
				endif
				indexNode := DiskCache:Get(ulDiskPos, false)
				//Iterators
				num++
			enddo
		endif
		return flag


	internal override property CurrentLen as DWord => CurrentKey:item1
	internal override property CurrentPos as DWord => CurrentKey:item2

    internal override property RootNodePos as DWord get area:LenIndexRoot set area:LenIndexRoot := value
end class

end namespace // xsharp.rdd.flexfile
