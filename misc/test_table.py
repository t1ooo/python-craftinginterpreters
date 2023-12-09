from .table import Key, NotFound, Table


def test_hast_table():
    t = Table()
    n = 10

    for i in range(n):
        # should return empty if not set
        assert t.get(Key(str(i))) == NotFound()

    for i in range(n):
        # should return true if set a non-existent key
        assert t.set(Key(str(i)), i)

    for i in range(n):
        # should return false if set an existing key
        assert not t.set(Key(str(i)), i)

    for i in range(n):
        # should return correct value
        assert t.get(Key(str(i))) == i

    for i in range(n):
        # should return true if deleting an existing key
        assert t.delete(Key(str(i)))

    for i in range(n):
        # should return true if deleting a non-existent key
        assert not t.delete(Key(str(i)))

    for i in range(n):
        # should return empty after deleting
        assert t.get(Key(str(i))) == NotFound()

    # add_all
    t1 = Table()
    t2 = Table()

    for i in range(n):
        t1.set(Key(str(i)), i)

    t2.add_all(t1)

    for i in range(n):
        # should return correct value after copy from another Table
        assert t2.get(Key(str(i))) == i
