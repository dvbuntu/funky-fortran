# Funky Fortran

Fortran is capable of some amazing and convenient things for numerical computing.  But sometimes it's not as convenient for data structures and other more pure "computer science" uses.  This repo is a simple set of home-grown Fortran90/95 data structures for use in whatever.

* `ll.f90` - Linked-list of integers.  Push, pop, find, remove (a particular value).
* `dict.f90` - Hash table of integers (dumb hash of value modulo the hash len, but it's the principle that counts).  Check, add, remove.
* `bst.f90` - Simple Binary Search Tree implementation with just integers.
* `ll_str.f90` - Linked list of strings.  Uses a dummy type behind the scenes but you shouldn't have to worry about it.
* `dict_str.f90` - Hash table of strings.  Hash is pretty basic.
* `bst_str.f90` - BST to store strings with keyed value (presumably some kind of hash.  Uses nodes and pointers rather than storing in giant array.

## Other structures to add

* General sparse graph (using linked nodes rather than an adjacency matrix, but maybe it's better to compress such an array)
