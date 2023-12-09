from dataclasses import dataclass
from typing import Any


@dataclass
class Key:
    value: str
    hash: int

    def __init__(self, value: str) -> None:
        self.value = value
        self.hash = hash_string(value)


@dataclass
class _Value:
    data: Any


@dataclass
class _Tombstone(_Value):
    pass


@dataclass
class NotFound:
    pass


@dataclass
class Entry:
    key: Key | None
    value: _Value | None

    @classmethod
    def empty(cls):
        return Entry(None, None)


TABLE_MAX_LOAD = 0.75


@dataclass
class Table:
    count: int
    entries: tuple[Entry, ...]

    def __init__(self) -> None:
        self.free()

    def set(self, key: Key, value: Any) -> bool:
        if self.count + 1 > len(self.entries) * TABLE_MAX_LOAD:
            self._adjust_capacity()

        entry = self.find_entry(self.entries, key)
        is_new_key = entry.key is None
        if is_new_key and entry.value is None:
            self.count += 1

        entry.key = key
        entry.value = _Value(value)
        return is_new_key

    def add_all(self, from_t: "Table"):
        for entry in from_t.entries:
            if entry.key is None:
                continue
            assert entry.value is not None
            self.set(entry.key, entry.value.data)

    def get(self, key: Key) -> Any | NotFound:
        if self.count == 0:
            return NotFound()

        entry = self.find_entry(self.entries, key)
        if entry.key is None:
            return NotFound()

        assert entry.value is not None
        return entry.value.data

    def delete(self, key: Key) -> bool:
        if self.count == 0:
            return False

        entry = self.find_entry(self.entries, key)
        if entry.key is None:
            return False

        entry.key = None
        entry.value = _Tombstone(True)
        return True

    def _adjust_capacity(self):
        capacity = 8 if len(self.entries) < 8 else len(self.entries) * 2
        new_entries = tuple(Entry.empty() for _ in range(capacity))

        self.count = 0
        for entry in self.entries:
            if entry.key is None:
                continue

            dest = self.find_entry(new_entries, entry.key)
            dest.key = entry.key
            dest.value = entry.value
            self.count += 1

        self.entries = new_entries

    def find_entry(self, entries: tuple[Entry, ...], key: Key) -> Entry:
        tombstone: Entry | None = None
        index = key.hash % len(entries)

        for _ in range(len(entries)):
            entry = entries[index]
            if entry.key is None:
                if entry.value is None:
                    # empty entry
                    if tombstone is not None:
                        return tombstone
                    else:
                        return entry
                else:
                    if tombstone is None:
                        tombstone = entry
            elif entry.key == key:
                return entry

            index = (index + 1) % len(entries)

        raise KeyError(key)

    def free(self):
        self.count = 0
        self.entries = tuple()


def hash_string(key: str) -> int:
    hash = 2166136261
    for i in range(len(key)):
        hash ^= ord(key[i])
        hash *= 16777619
    return hash
