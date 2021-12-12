# cc2go

Hack to do a first pass of transpiling C++ to Go. It is not meant to produce
working Go code but simply to make it faster to translate it. It is a tad
smarter than running a bunch of sed/awk commands.

What works:

- Merge the copyright header at the top.
- End of line // style comment.
- Add missing curly brackets for one liner condition if / else.
- Comment out most forward declaration.
- Update function argument and return value order.
- Method receiver.

## Process

Run it once. Take one file and manually fix it and remove the `//go:build
nobuild` line. When this line is not present, `cc2go` will not overwrite it
anymore.
