// See https://github.com/X-Sharp/XSharpPublic/issues/1007
// Incorrect error message  Error: XS9083 - Cannot implicitly convert between 'byte' and 'PTR' in platform 'AnyCpu' because the size of 'PTR' cannot be determined at compile time.
CLASS StaticMemoryPool
  export  cName       as string
  protect pPoolEntry  as byte ptr  // For easy pointer arithmetic
  protect nTotalSize  as int
  protect nAllocated  as int     // Number of bytes already allocated


  METHOD allocate(nSizeToAlloc as int) as ptr
    local pFreeSpaceEntry  as ptr
    pFreeSpaceEntry := self:pPoolEntry + self:nAllocated   // Error here
    self:nAllocated += nSizeToAlloc
  RETURN pFreeSpaceEntry
end class
