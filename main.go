// Copyright 2021 Marc-Antoine Ruel. All rights reserved.
// Use of this source code is governed under the Apache License, Version 2.0
// that can be found in the LICENSE file.

package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

var (
	// Used in phase 1

	// e.g. "struct Foo;"
	reForwardStruct = regexp.MustCompile(`^struct [A-Za-z]+;$`)
	// e.g. "/// Foo." used for doxyge.
	reTrippleComment = regexp.MustCompile(`^(\s*)///(.*)`)
	reDoubleComment  = regexp.MustCompile(`^(\s*)//(.*)`)
	// e.g. "protected:"
	reStructAccess = regexp.MustCompile(`^(\s*)(public|protected|private):$`)
	// e.g. "void bar() const {"
	reConstMethod = regexp.MustCompile(`^(.+)\) const {$`)
	// A string.
	reConstChar = regexp.MustCompile(`const char\s?\*`)

	// Used later

	// e.g. "extern "C" {"
	reExtern = regexp.MustCompile(`^(\s*)extern (.*)`)

	// e.g. "void bar() {" or "virtual void bar() const = 0;"
	reFunc          = regexp.MustCompile(`^(\s*)(?:virtual |)([a-zA-Z]+)\s+[A-Za-z_]+\s?\([a-zA-Z *,=<>_:&\[\]]*\)(?: const |)(?: = 0|);$`)
	reSimpleWord    = regexp.MustCompile(`^[a-z]+$`)
	reNotSimpleWord = regexp.MustCompile(`^![a-z]+$`)
)

var asciiSpace = [256]uint8{'\t': 1, '\n': 1, '\v': 1, '\f': 1, '\r': 1, ' ': 1}

func countSpaces(l string) int {
	i := 0
	for ; i < len(l); i++ {
		if asciiSpace[l[i]] == 0 {
			break
		}
	}
	return i
}

// phase1ProcessLine processes the low hanging fruits first.
func phase1ProcessLine(l string) string {
	// Just ignore.
	if strings.HasPrefix(l, "using namespace") {
		return "// " + l
	}
	if reForwardStruct.MatchString(l) {
		return "// " + l
	}
	if m := reStructAccess.FindStringSubmatch(l); m != nil {
		c := len(m[1])
		return l[:c] + "//" + l[c:]
	}

	// Fix comments.
	if reTrippleComment.MatchString(l) {
		return reTrippleComment.ReplaceAllString(l, "$1//$2")
	}
	if reDoubleComment.MatchString(l) {
		return l
	}

	// Actual code.
	l = strings.ReplaceAll(l, "std::", "")
	l = strings.ReplaceAll(l, "const string&", "string")
	l = strings.ReplaceAll(l, ".c_str()", "")
	l = strings.ReplaceAll(l, "->", ".")
	l = strings.ReplaceAll(l, "NULL", "nil")
	l = reConstChar.ReplaceAllString(l, "string")
	if reConstMethod.MatchString(l) {
		return reConstMethod.ReplaceAllString(l, "$1) {")
	}
	return l
}

// commentDefines handles includes and defines, including multi-lines defines.
func commentDefines(lines []string) []string {
	var out []string
	indef := false
	for _, l := range lines {
		if strings.HasPrefix(l, "#") {
			indef = strings.HasSuffix(l, "\\")
			l = "//" + l
		} else if indef {
			indef = strings.HasSuffix(l, "\\")
			l = "//" + l
		} else {
			indef = false
		}
		out = append(out, l)
	}
	return out
}

func commentExtern(lines []string) []string {
	var out []string
	inextern := 0
	for _, l := range lines {
		if m := reExtern.FindStringSubmatch(l); m != nil {
			l = m[1] + "//" + l[len(m[1]):]
			if strings.HasSuffix(l, "{") {
				inextern++
			}
		} else if inextern > 0 {
			if strings.TrimSpace(l) == "}" {
				inextern--
			}
			c := countSpaces(l)
			l = l[:c] + "//" + l[c:]
		}
		out = append(out, l)
	}
	return out
}

// mergeParenthesis makes all () on one line to make function declaration
// easier to parse.
func mergeParenthesis(lines []string) []string {
	count := 0
	out := []string{""}
	for _, l := range lines {
		if count > 0 {
			// Merging.
			l = " " + l[countSpaces(l):]
		}
		out[len(out)-1] = out[len(out)-1] + l
		if strings.HasPrefix(l[countSpaces(l):], "//") {
			// Ignore comments within parenthesis.
			out = append(out, "")
			continue
		}
		count += strings.Count(l, "(")
		count -= strings.Count(l, ")")
		if count < 0 {
			panic(count)
		}
		if count == 0 {
			out = append(out, "")
		}
	}
	return out
}

// fixCondition does two things:
//  - Remove the extra parenthesis.
//  - Fix one liners.
func fixCondition(lines []string) []string {
	var out []string
	var insertClosingBracket []string
	for _, l := range lines {
		c := countSpaces(l)
		m := l[c:]
		if strings.HasPrefix(m, "if (") {
			// There's a comment at least once so don't use HasSuffix.
			if !strings.Contains(m, "{") {
				// One liner.
				l += " {"
				insertClosingBracket = append(insertClosingBracket, l[:c])
			}
			// Trim the very first and very last parenthesis. There can be inside due
			// to function calls.
			i := strings.LastIndex(l, ")")
			cond := l[c+4 : i]
			// For conditions with nothing but a word, let's assume it checks for
			// nil. It's going to be wrong often but I think it's more often right
			// than wrong.
			if reSimpleWord.MatchString(cond) {
				cond += " != nil"
			} else if reNotSimpleWord.MatchString(cond) {
				cond = cond[1:] + " == nil"
			} else if reSimpleWord.MatchString(strings.TrimSuffix(cond, ".empty()")) {
				// if (foo.empty())
				cond = "len(" + cond[:len(cond)-len(".empty()")] + ") == 0"
			} else if reNotSimpleWord.MatchString(strings.TrimSuffix(cond, ".empty()")) {
				// if (!foo.empty())
				cond = "len(" + cond[1:len(cond)-len(".empty()")] + ") != 0"
			}
			l = l[:c] + "if " + cond + l[i+1:]
			out = append(out, l)
		} else if len(insertClosingBracket) != 0 {
			// TODO(maruel): "else if"
			out = append(out, l)
			out = append(out, insertClosingBracket[len(insertClosingBracket)-1]+"}")
			insertClosingBracket = insertClosingBracket[:len(insertClosingBracket)-1]
		} else {
			out = append(out, l)
		}
	}
	return out
}

// fixStatements handles statements inside a function.
func fixStatements(lines []string) []string {
	insideBlock := 0
	var out []string
	for _, l := range lines {
		if reDoubleComment.MatchString(l) {
			out = append(out, l)
			continue
		}
		was := insideBlock
		insideBlock += strings.Count(l, "{")
		insideBlock -= strings.Count(l, "}")
		if insideBlock < 0 {
			panic(l)
		}
		if was > 0 {
			// Process a statement.
			if strings.HasSuffix(l, ".clear();") {
				l = l[:len(l)-len(".clear();")] + " = nil"
			}
			//Convert "foo bar = baz()" to "bar = baz()"
		}
		out = append(out, l)
	}
	return out
}

// processFunctionDeclaration rewrite a function declaration to be closer to Go
// style.
func processFunctionDeclaration(lines []string) []string {
	// Convert method
	// Remove const
	// Rewrite const string& to string
	// Process argument type
	var out []string
	for _, l := range lines {
		if reDoubleComment.MatchString(l) {
			out = append(out, l)
			continue
		}
		// TODO(maruel): Constructor, destructor, method.
		// commentOutForwardFunc comments any forward declaration found. Go doesn't
		// need these.
		if m := reFunc.FindStringSubmatch(l); m != nil {
			if m[2] != "return" {
				// White spaces in front.
				c := len(m[1])
				l = l[:c] + "//" + l[c:]
			}
		}
		out = append(out, l)
	}
	return out
}

// load loads a single .h/.cc and processes it to make it closer to Go.
//
// The resulting file is not syntactically valid Go but it will make manual fix
// ups easier.
func load(name string) string {
	log.Printf("%s", name)
	raw, err := os.ReadFile(name)
	if err != nil {
		return ""
	}

	// Do a first pass to trim obvious stuff.
	lines := strings.Split(string(raw), "\n")
	for i, l := range lines {
		lines[i] = phase1ProcessLine(l)
	}

	lines = commentDefines(lines)

	// Make a second context aware pass.
	lines = mergeParenthesis(lines)
	lines = fixCondition(lines)
	lines = commentExtern(lines)
	lines = processFunctionDeclaration(lines)
	lines = fixStatements(lines)

	//addThisPointer(out)
	//Comment assert()
	//Convert "set<foo>" to "map[foo]struct{}"
	//Convert "vector<foo>" to "[]foo"
	//Convert enum
	//Comment out namespace

	out := ""
	for _, l := range lines {
		// At the very end, remove the trailing ;
		if strings.HasSuffix(l, ";") {
			l = l[:len(l)-1]
		}
		out += l + "\n"
	}
	return out
}

// process processes a pair of .h/.cc files.
func process(outDir, root string) error {
	out := "package ginga\n\n"
	out += load(root + ".h")
	out += load(root + ".cc")
	return os.WriteFile(filepath.Join(outDir, root+".go"), []byte(out), 0o644)
}

const gingaContent = `package ginga

import (
  "io"
  "os"
)

var (
  stdout = os.Stdout
  stderr = os.Stderr
)

func assert(b bool) {
  panic(b)
}

func printf(fmt string, v...interface{}) {
  fmt.Printf(fmt, v...)
}

func fprintf(w io.Writer, fmt string, v...interface{}) {
  fmt.Fprintf(w, fmt, v...)
}
`

func mainImpl() error {
	v := flag.Bool("v", false, "verbose")
	flag.Parse()
	if !*v {
		log.SetOutput(ioutil.Discard)
	}
	outDir := "go2"
	entries, err := os.ReadDir(".")
	if err != nil {
		return err
	}
	roots := map[string]struct{}{}
	for _, e := range entries {
		n := e.Name()
		ext := filepath.Ext(n)
		n = n[:len(n)-len(ext)]
		if strings.HasSuffix(n, "_test") || strings.HasSuffix(n, "_perftest") || n == "test" {
			continue
		}
		roots[n] = struct{}{}
	}
	if err := os.Mkdir(outDir, 0o755); err != nil {
		return err
	}
	files := make([]string, 0, len(roots))
	for root := range roots {
		files = append(files, root)
	}
	sort.Strings(files)
	for _, root := range files {
		if err := process(outDir, root); err != nil {
			return err
		}
	}
	if err := ioutil.WriteFile(filepath.Join(outDir, "ginja.go"), []byte(gingaContent), 0o644); err != nil {
		return err
	}
	cmd := exec.Command("go", "mod", "init", "ginga")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Dir = outDir
	return cmd.Run()
}

func main() {
	if err := mainImpl(); err != nil {
		fmt.Fprintf(os.Stderr, "cc2go: %s\n", err)
		os.Exit(1)
	}
}
