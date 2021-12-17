// Copyright 2021 Marc-Antoine Ruel. All rights reserved.
// Use of this source code is governed under the Apache License, Version 2.0
// that can be found in the LICENSE file.

package main

import (
	"log"
	"strconv"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestFixIf(t *testing.T) {
	data := []struct {
		input string
		want  string
	}{
		{
			`
func foo() {
	bar();
}
`,
			`
func foo() {
	bar();
}
`,
		},
		{
			`
func foo() {
	if (bar())
		baz();
}
`,
			`
func foo() {
	if bar() {
		baz();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar())
		if (baz())
			boo();
}
`,
			`
func foo() {
	if bar() {
		if baz() {
			boo();
		}
	}
}
`,
		},
		{
			`
func foo() {
	if (bar())
		baz1();
	else
		baz2();
}
`,
			`
func foo() {
	if bar() {
		baz1();
	} else {
		baz2();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar()) baz();
}
`,
			`
func foo() {
	if bar() {
		baz();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar()) {
		baz1();
	} else
		baz2();
}
`,
			`
func foo() {
	if bar() {
		baz1();
	} else {
		baz2();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar())
		baz1();
	else {
		baz2();
	}
}
`,
			`
func foo() {
	if bar() {
		baz1();
	} else {
		baz2();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar())
		baz1();
	else if (faz())
		baz2();
}
`,
			`
func foo() {
	if bar() {
		baz1();
	} else if faz() {
		baz2();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar())
		if (baz())
			baz1();
		else
			baz2();
}
`,
			`
func foo() {
	if bar() {
		if baz() {
			baz1();
		} else {
			baz2();
		}
	}
}
`,
		},
		{
			`
func foo() {
	if bar() {
		if baz() {
			baz1();
		} else {
			baz2();
		}
	} else {
		bar2();
	}
}
`,
			`
func foo() {
	if bar() {
		if baz() {
			baz1();
		} else {
			baz2();
		}
	} else {
		bar2();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar())
		if (baz())
			baz1();
		else
			baz2();
	else
		bar2();
}
`,
			`
func foo() {
	if bar() {
		if baz() {
			baz1();
		} else {
			baz2();
		}
	} else {
		bar2();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar == '}') baz();
}
`,
			`
func foo() {
	if bar == '}' {
		baz();
	}
}
`,
		},
		{
			`
func foo() {
	if (bar == ' ') baz();
	{
		continue;
	}
}
`,
			`
func foo() {
	if bar == ' ' {
		baz();
	}
	{
		continue;
	}
}
`,
		},
	}
	for i, l := range data {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			var input []Line
			for _, j := range strings.Split(l.input, "\n") {
				input = append(input, processLine(j))
			}
			got := ""
			lines := fixInsideFuncs(input, fixIf)
			for k, j := range lines {
				if !j.skip {
					got += j.String()
					if k != len(lines)-1 {
						got += "\n"
					}
				}
			}
			if diff := cmp.Diff(l.want, got); diff != "" {
				t.Fatalf("fixIf mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestExtractEmbedded(t *testing.T) {
	data := []struct {
		input string
		want  string
	}{
		{
			`
func foo() {
	bar();
}
`,
			`
func foo() {
	bar();
}
`,
		},
		{
			`
type Foo struct {
}
`,
			`
type Foo struct {
}
`,
		},
		{
			`
type Foo struct {
	int i
	int j
}
`,
			`
type Foo struct {
	int i
	int j
}
`,
		},
		/*
					{
						`
			type Foo struct {
				type Bar struct {
				}
			}
			`,
						`
			type Foo struct {
			}
				type Bar struct {
				}
			`,
					},
		*/
		/*
					{
						`
			type Foo struct {
				func foo() {
					bar();
				}
			}
			`,
						`
			type Foo struct {
			}
				func foo() {
					bar();
				}
			`,
					},
		*/
	}
	for i, l := range data {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			var input []Line
			for _, j := range strings.Split(l.input, "\n") {
				input = append(input, processLine(j))
			}
			got := ""
			lines := extractEmbedded(input)
			for k, j := range lines {
				if !j.skip {
					got += j.String()
					if k != len(lines)-1 {
						got += "\n"
					}
				}
			}
			if diff := cmp.Diff(l.want, got); diff != "" {
				t.Fatalf("extractEmbedded mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func init() {
	log.SetFlags(0)
}
