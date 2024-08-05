FUNCTION Start( ) AS VOID
    try
        LOCAL coll as Collection
        coll = CREATEOBJECT("Collection")
        ? coll.Count
        coll.Add(1,"aaa")
        coll.Add(2,"bbb")
        coll.Add(3,"aAa")
        coll.Add(4,"bBb")
        ? coll.KeySort
        ? coll.Count
        coll.KeySort = 4
        FOR each var item in coll
            ?  item, coll.GetKey(item)
        next
    catch e as exception
        ? e:ToString()
    end try
    wait
    RETURN

