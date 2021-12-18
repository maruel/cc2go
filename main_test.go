// Copyright 2021 Marc-Antoine Ruel. All rights reserved.
// Use of this source code is governed under the Apache License, Version 2.0
// that can be found in the LICENSE file.

package main

import (
	"flag"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestCommentNamespace(t *testing.T) {
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
namespace foo {
}
`,
			`
`,
		},
		{
			`
namespace foo {
func foo() {
	bar();
}
}
`,
			`
func foo() {
	bar();
}
`,
		},
	}
	for i, l := range data {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			lines := commentNamespace(split(l.input))
			if diff := cmp.Diff(l.want, merge(lines)); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

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
			lines := fixInsideFuncs(split(l.input), fixIf)
			if diff := cmp.Diff(l.want, merge(lines)); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
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
};
`,
			`
type Foo struct {
};
`,
		},
		{
			`
type Foo struct {
	int i
	int j
};
`,
			`
type Foo struct {
	int i
	int j
};
`,
		},
		{
			`
type Foo struct {
	type Bar struct {
	};
};
`,
			`
type Foo struct {
};
type Bar struct {
};
`,
		},
		{
			`
type Foo struct {
	int i;
	type Bar struct {
	};
	int j;
};
`,
			`
type Foo struct {
	int i;
	int j;
};
type Bar struct {
};
`,
		},
		{
			`
type Foo struct {
	// Doc
	func foo() {
		bar();
	}
};
`,
			`
type Foo struct {
};
// Doc
func foo() {
	bar();
}
`,
		},
		{
			`
type Foo struct {
	enum Bar {
		A
	}
};
`,
			`
type Foo struct {
};
enum Bar {
	A
}
`,
		},
		{
			`
type Foo struct {
	// Doc
	enum Bar
	{
		A
	}
};
`,
			`
type Foo struct {
};
// Doc
enum Bar
{
	A
}
`,
		},
	}
	for i, l := range data {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			lines := extractEmbedded(split(l.input))
			if diff := cmp.Diff(l.want, merge(lines)); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestFixInsideFuncs(t *testing.T) {
	data := []struct {
		input  string
		want   string
		inside []Line
	}{
		{
			`
func foo() {
	bar();
}
`,
			`
func foo() {
	bar();X
}
`,
			[]Line{{original: []string{"\tbar();"}, indent: "\t", code: "bar();"}},
		},
		{
			`
func foo() { bar();
}
`,
			`
func foo() { bar();
}
`,
			nil,
		},
		{
			`
func foo() {
	bar(); }
`,
			`
func foo() {
	bar(); }
`,
			nil,
		},
		{
			`
func foo() { bar(); }
`,
			`
func foo() { bar(); }
`,
			nil,
		},
	}
	for i, l := range data {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			var gotInside []Line
			lines := fixInsideFuncs(split(l.input), func(in []Line, _, _, _ string) []Line {
				gotInside = make([]Line, len(in))
				copy(gotInside, in)
				var out []Line
				for _, j := range in {
					j.code += "X"
					out = append(out, j)
				}
				return out
			})
			if diff := cmp.Diff(l.inside, gotInside, cmp.AllowUnexported(Line{})); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
			}
			if diff := cmp.Diff(l.want, merge(lines)); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestFixInsideStructs(t *testing.T) {
	data := []struct {
		input  string
		want   string
		inside []Line
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
			nil,
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
			nil,
		},
		{
			`
type Foo struct {
	func bar() {
		baz();
	}
}
`,
			`
type Foo struct {
	func bar() {X
		baz();X
	}X
}
`,
			[]Line{
				{original: []string{"\tfunc bar() {"}, indent: "\t", code: "func bar() {"},
				{original: []string{"\t\tbaz();"}, indent: "\t\t", code: "baz();"},
				{original: []string{"\t}"}, indent: "\t", code: "}"},
			},
		},
		{
			`
type Foo struct {
	type Inner struct {
		func bar() {
			baz();
		}
	}
}
`,
			`
type Foo struct {
	type Inner struct {X
		func bar() {X
			baz();X
		}X
	}X
}
`,
			[]Line{
				{original: []string{"\ttype Inner struct {"}, indent: "\t", code: "type Inner struct {"},
				{original: []string{"\t\tfunc bar() {"}, indent: "\t\t", code: "func bar() {"},
				{original: []string{"\t\t\tbaz();"}, indent: "\t\t\t", code: "baz();"},
				{original: []string{"\t\t}"}, indent: "\t\t", code: "}"},
				{original: []string{"\t}"}, indent: "\t", code: "}"},
			},
		},
	}
	for i, l := range data {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			var gotInside []Line
			lines := fixInsideStructs(split(l.input), func(in []Line, structName string) []Line {
				gotInside = make([]Line, len(in))
				copy(gotInside, in)
				var out []Line
				for _, j := range in {
					j.code += "X"
					out = append(out, j)
				}
				return out
			})
			if diff := cmp.Diff(l.inside, gotInside, cmp.AllowUnexported(Line{})); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
			}
			if diff := cmp.Diff(l.want, merge(lines)); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestLoad(t *testing.T) {
	raw := `// Copyright.

/// Foo does stuff.
struct Foo {
	// Virtual class.
  virtual ~Foo() {}
  virtual bool Start(Edge* edge) const = 0;

  /// The bar.
  struct Bar {
    Bar() : edge(NULL) {}
    Edge* edge;
    std::string output;
    bool success() const { return status == Success; }
  };

  virtual std::vector<Edge*> Get() { return std::vector<Edge*>(); }
  virtual void Abort() {}
	typedef Fun stuff;
};

typedef bozo bit;
`
	wantHdr := "// Copyright.\n"
	wantContent := `
// Foo does stuff.
type Foo struct {

}
// The bar.
type Bar struct {
  edge *Edge
  output string
  }
func NewBar() Bar {
	return Bar{
		edge: nil,
	}
}
func (f *Foo) success() bool {
	return status == Success
}
func (f *Foo) Get() []*Edge {
	return vector<Edge*>()
}
func (f *Foo) Abort() {}
type stuff Fun

type bit bozo

`
	doc := map[string][]Line{}
	gotHdr, gotContent := load([]byte(raw), false, doc)
	if diff := cmp.Diff(wantHdr, gotHdr); diff != "" {
		t.Fatalf("header mismatch (-want +got):\n%s", diff)
	}
	if diff := cmp.Diff(wantContent, gotContent); diff != "" {
		t.Fatalf("content mismatch (-want +got):\n%s", diff)
	}
}

func TestFindClosingBracket(t *testing.T) {
	data := []struct {
		input string
		want  int
	}{
		{"", -1},
		{"a\nb", -1},
		{"a{}", 0},
		{"a{\n}", 1},
		{"a{\n}\nc", 1},
		{"a\n{\n}\nc", 2},
		{"a {\nif foo {\n}\n}\nc", 3},
		{"a\n{\nif foo {\n}\n}\nc", 4},
	}
	for i, l := range data {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			got := findClosingBracket(split(l.input))
			if diff := cmp.Diff(l.want, got); diff != "" {
				t.Fatalf("mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func split(i string) []Line {
	var o []Line
	for _, j := range strings.Split(i, "\n") {
		o = append(o, processLine(j))
	}
	return o
}

func merge(in []Line) string {
	out := ""
	for k, l := range in {
		if !l.skip {
			out += l.String()
			if k != len(in)-1 {
				out += "\n"
			}
		}
	}
	return out
}

func TestMain(m *testing.M) {
	log.SetFlags(log.Lshortfile)
	flag.Parse()
	if !testing.Verbose() {
		log.SetOutput(ioutil.Discard)
	}
	os.Exit(m.Run())
}
