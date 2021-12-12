# cc2go

Hack to do a first pass of transpiling C++ to Go. It is not meant to produce
working Go code but simply to make it faster to translate it.

What works:

- Add missing curly brackets for one liner condition if / else.
- Comment out most forward declaration.
- Update function argument and return value order.
