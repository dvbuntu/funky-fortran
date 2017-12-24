# Funky Fortran

Fortran is capable of some amazing and convenient things for numerical computing.  But sometimes it's not as convenient for data strucctures and other more pure "computer science" uses.  This repo is a simple set of home-grown Fortran90/95 data structures for use in whatever.

* `ll.f90` - Linked-list of integers.  Push, pop, find, remove (a particular value).
* `dict.f90` - Hash table of integers (dumb hash of value modulo the hash len, but it's the principle that counts).  Check, add, remove.
