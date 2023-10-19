// Application : R914 - Static define in files with the same name
// Test.prg , Created : 19-10-2023   13:41
// User : robert
static define Duplicate := 1
static function TestDuplicate()
    return Duplicate
function Test1
    return TestDuplicate()
