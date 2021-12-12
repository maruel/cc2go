// Copyright 2021 Marc-Antoine Ruel. All rights reserved.
// Use of this source code is governed under the Apache License, Version 2.0
// that can be found in the LICENSE file.

package main

import (
	"bytes"
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
	"unicode"
)

var (
	// Used in phase 1

	// e.g. "struct Foo;"
	reForwardStruct = regexp.MustCompile(`^struct [A-Za-z]+;$`)
	// e.g. "protected:"
	reStructAccess = regexp.MustCompile(`^(public|protected|private):$`)
	// e.g. "void bar() const {"
	reConstMethod = regexp.MustCompile(`^(.+)\) const {$`)
	// A string.
	reConstChar = regexp.MustCompile(`const char\s?\*`)

	// Used later

	// e.g. "extern "C" {"
	reExtern = regexp.MustCompile(`^(\s*)extern (.*)`)

	// e.g. "void bar();" or "virtual void bar() const = 0;"
	// 1 is return type, 2 is name.
	reFuncDeclaration        = regexp.MustCompile(`^(?:virtual |)([a-zA-Z<>*]+)\s+([A-Za-z_0-9]+)\s?\([a-zA-Z *,=<>_:&\[\]]*\)(?: const|)\s*(?:= 0|)\s*;$`)
	reConstructorDeclaration = regexp.MustCompile(`^([A-Za-z_0-9]+)\s?\([a-zA-Z *,=<>_:&\[\]]*\);$`)
	// 1 is return type, 2 is name, 3 is args, 4 is brackets
	reFuncImplementation = regexp.MustCompile(`^(?:virtual |)([a-zA-Z<>*]+)\s+([A-Za-z_0-9]+)\s?\(([a-zA-Z *,=<>_:&\[\]]*)\)(?: const|)\s*({(?:|\s*}))$`)
	reStructDefinition   = regexp.MustCompile(`^struct ([A-Za-z]+)(?:\s*:[a-zA-Z, ]+|)\s*{$`)
	reSimpleWord         = regexp.MustCompile(`^[a-z]+$`)
	reNotSimpleWord      = regexp.MustCompile(`^![a-z]+$`)

	// e.g. "foo bar = baz();"
	reAssignment = regexp.MustCompile(`^(\s*)[a-zA-Z<>\*:&]+ ([a-zA-Z_]+ )=( .+;)$`)
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

// Line describes one source line.
type Line struct {
	original []string

	// The final line is indent + code + comment.

	// The whitespace indentation
	indent string
	// Any valid statement
	code string
	// Includes the "//"
	comment string

	skip bool
}

func (l *Line) doSkip() {
	l.comment = "//" + l.code + l.comment
	l.code = ""
	l.skip = true
}

func (l *Line) String() string {
	if l.code == "" && l.comment == "" {
		return ""
	}
	return l.indent + l.code + l.comment
}

// processLine processes the low hanging fruits first.
func processLine(l string) Line {
	c := countSpaces(l)
	out := Line{
		original: []string{l},
		indent:   l[:c],
	}
	l = l[c:]

	// Used for doxygen comments.
	if strings.HasPrefix(l, "///") {
		l = l[1:]
	}
	if a := strings.Index(l, "//"); a != -1 {
		if b := strings.LastIndexFunc(l[:a], func(r rune) bool { return !unicode.IsSpace(r) }); b != -1 {
			a = b + 1
		}
		out.code = l[:a]
		out.comment = l[a:]
	} else {
		out.code = l
	}

	// Ignore C++ statements that are unnecessary in Go.
	if strings.HasPrefix(out.code, "using namespace") {
		out.doSkip()
	}
	if reForwardStruct.MatchString(out.code) {
		out.doSkip()
	}
	if m := reStructAccess.FindStringSubmatch(out.code); m != nil {
		out.doSkip()
	}

	// Actual code.
	out.code = strings.ReplaceAll(out.code, "std::", "")
	out.code = strings.ReplaceAll(out.code, "const string&", "string")
	out.code = strings.ReplaceAll(out.code, ".c_str()", "")
	out.code = strings.ReplaceAll(out.code, "->", ".")
	out.code = strings.ReplaceAll(out.code, "NULL", "nil")
	out.code = reConstChar.ReplaceAllString(out.code, "string")
	if reConstMethod.MatchString(out.code) {
		out.code = reConstMethod.ReplaceAllString(out.code, "$1) {")
	}
	return out
}

// commentDefines handles includes and defines, including multi-lines defines.
func commentDefines(lines []Line) []Line {
	var out []Line
	indef := false
	for _, l := range lines {
		if strings.HasPrefix(l.code, "#") {
			indef = strings.HasSuffix(l.code, "\\")
			l.doSkip()
		} else if indef {
			indef = strings.HasSuffix(l.code, "\\")
			l.doSkip()
		} else {
			indef = false
		}
		out = append(out, l)
	}
	return out
}

func commentExtern(lines []Line) []Line {
	var out []Line
	inextern := 0
	for _, l := range lines {
		if m := reExtern.FindStringSubmatch(l.code); m != nil {
			l.code = m[1] + "//" + l.code[len(m[1]):]
			if strings.HasSuffix(l.code, "{") {
				inextern++
			}
		} else if inextern > 0 {
			if strings.TrimSpace(l.code) == "}" {
				inextern--
			}
			l.doSkip()
		}
		out = append(out, l)
	}
	return out
}

// processStructDefinition rewrites the structs.
func processStructDefinition(lines []Line) []Line {
	var out []Line
	for _, l := range lines {
		if m := reStructDefinition.FindStringSubmatch(l.code); m != nil {
			l.code = "struct " + m[1] + " {"
		}
		out = append(out, l)
	}
	return out
}

// mergeParenthesis makes all () on one line to make function declaration
// easier to parse.
func mergeParenthesis(lines []Line) []Line {
	out := []Line{}
	acc := Line{}
	count := 0
	for _, l := range lines {
		if count == 0 {
			acc.indent = l.indent
			acc.skip = l.skip
		}
		count += strings.Count(l.code, "(")
		count -= strings.Count(l.code, ")")
		if count < 0 {
			fmt.Fprintf(os.Stderr, "ERROR: mergeParenthesis %d\n", count)
		} else {
			// Merging matching ) to the previous line containing the (.
			acc.original = append(acc.original, l.original...)
			acc.code += l.code
			acc.comment += l.comment
			if count == 0 {
				// Output the line.
				out = append(out, acc)
				acc = Line{indent: l.indent, skip: l.skip}
			} else {
				acc.code += " "
			}
		}
	}
	return out
}

func cleanCond(cond string) string {
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
	return cond
}

// fixCondition does two things:
//  - Fix one liners.
//  - Remove the extra parenthesis.
func fixCondition(lines []Line) []Line {
	var out []Line
	var insertClosingBracket []string
	for i, l := range lines {
		shouldClose := len(insertClosingBracket) != 0
		// Start of a condition.
		if strings.HasPrefix(l.code, "if (") || strings.HasPrefix(l.code, "} else if (") {
			if !strings.HasSuffix(l.code, "{") {
				// One liner.
				l.code += " {"
				insertClosingBracket = append(insertClosingBracket, l.indent)
			}
			// Trim the very first and very last parenthesis. There can be inside due
			// to function calls.
			j := strings.LastIndex(l.code, ")")
			c := strings.Index(l.code, "if (")
			l.code = l.code[:c] + "if " + cleanCond(l.code[4:j]) + l.code[j+1:]
		} else if l.code == "} else" {
			// One liner else.
			l.code += " {"
			insertClosingBracket = append(insertClosingBracket, l.indent)
		}
		out = append(out, l)
		if shouldClose {
			// Check if the next line has a "else", if so then add the bracket there.
			if i < len(lines)-1 && strings.HasPrefix(lines[i+1].code, "else") {
				lines[i+1].code = "} " + lines[i+1].code
			} else {
				out = append(out, Line{indent: insertClosingBracket[len(insertClosingBracket)-1], code: "}"})
			}
			insertClosingBracket = insertClosingBracket[:len(insertClosingBracket)-1]
		}
	}

	return out
}

// processFunctionDeclaration comments out forward declarations, grabbing
// docstring along the way.
//
// Go doesn't need forward declaration.
func processFunctionDeclaration(lines []Line, doc map[string][]Line) []Line {
	var out []Line
	for _, l := range lines {
		if m := reConstructorDeclaration.FindStringSubmatch(l.code); m != nil {
			// TODO(maruel): Gets confused with simple function calls. Needs to not
			// process when inside a function.
			//l.doSkip()
		} else if m := reFuncDeclaration.FindStringSubmatch(l.code); m != nil {
			if m[1] == "return" {
				// It's super annoying that "return Foo();" triggers but there's
				// nothing to do but explicitly ignore.
				continue
			}
			l.doSkip()
			// Associate the function description if available.
			for i := len(out) - 1; i >= 0 && out[i].code == "" && out[i].comment != ""; i-- {
				doc[m[2]] = append([]Line{out[i]}, doc[m[2]]...)
				out[i].skip = true
			}
		}
		out = append(out, l)
	}
	return out
}

// processArgs process the arguments in a function declaration.
func processArgs(l string) string {
	if l == "" {
		return l
	}

	// Handling templates requires walking manually to count the angle brackets.
	var args []string
	ab := 0
	acc := ""
	for _, r := range l {
		switch r {
		case '<':
			ab++
			acc += string(r)
		case '>':
			ab--
			acc += string(r)
		case ',':
			if ab == 0 {
				args = append(args, acc)
				acc = ""
			} else {
				acc += string(r)
			}
		case ' ':
			if len(acc) != 0 {
				acc += string(r)
			}
		default:
			acc += string(r)
		}
	}
	if acc != "" {
		args = append(args, acc)
	}

	// Now that they are properly split, process the types.
	for i, a := range args {
		args[i] = processArg(a)
	}
	return strings.Join(args, ", ")
}

// processArg process one argument string in a function declaration. It's one
// of the most tedious thing to do when converting code manually.
func processArg(a string) string {
	c := strings.LastIndex(a, " ")
	if c == -1 {
		panic(a)
	}
	t := a[:c]
	n := a[c+1:]
	for strings.HasSuffix(t, "*") {
		t = "*" + t[:len(t)-1]
	}
	for strings.HasPrefix(n, "*") {
		n = n[1:]
		t = "*" + t
	}
	if strings.HasSuffix(n, "[]") {
		n = n[:len(n)-2]
		t = "[]" + t
	}
	if strings.HasPrefix(t, "const ") {
		t = t[len("const "):]
	}
	if strings.HasSuffix(t, "&") {
		t = "*" + strings.TrimSpace(t[:len(t)-1])
	}

	/* TODO(maruel): Skip for now because it doesn't behave well with pointers.
	for found := true; found; found = false {
		// Convert "vector<foo>" to "[]foo".
		if c := strings.Index(t, "vector<"); c != -1 {
			t = t[:c] + "[]" + t[c+len("vector<"):len(t)-1]
			found = true
		}
		// TODO(maruel): Convert "set<foo>" to "map[foo]struct{}"
	}
	*/
	return n + " " + t
}

// processFunctionImplementation handles function implementations.
//
// It leverages doc that was populated in processFunctionDeclaration().
func processFunctionImplementation(lines []Line, doc map[string][]Line) []Line {
	var out []Line
	cur := ""
	brackets := 0
	for _, l := range lines {
		brackets += strings.Count(l.code, "{")
		brackets -= strings.Count(l.code, "}")
		if m := reFuncImplementation.FindStringSubmatch(l.code); m != nil {
			// 1 is return type, 2 is name, 3 is args, 4 is brackets
			if m[1] == "if" {
				// It's annoying that this triggers, just shrug it.
				out = append(out, l)
				continue
			}
			if m[1] == "return" {
				panic(l.code)
			}
			if cur != "" {
				panic(l.code)
			}
			if m[1] == "void" {
				m[1] = ""
			}
			//log.Printf("%s", l.code)
			args := processArgs(m[3])
			if m[1] == "" {
				l.code = "func " + m[2] + "(" + args + ") " + m[4]
			} else {
				l.code = "func " + m[2] + "(" + args + ") " + m[1] + " " + m[4]
			}
			name := strings.ReplaceAll(m[2], "::", ".")
			if d := doc[name]; len(d) != 0 {
				// Insert the doc there.
				out = append(out, d...)
			}
		}
		out = append(out, l)
	}
	return out
}

// fixStatements handles statements inside a function.
func fixStatements(lines []Line) []Line {
	// TODO(maruel): Ensure the code cannot be inside nested functions. The
	// code base we process do not use this.

	// TODO(maruel): Constructor, destructor, method.
	// Convert method
	// Remove const
	// Process argument type
	//addThisPointer(out)
	insideBlock := 0
	var out []Line
	for i, l := range lines {
		was := insideBlock
		insideBlock += strings.Count(l.code, "{")
		insideBlock -= strings.Count(l.code, "}")
		if insideBlock < 0 {
			fmt.Fprintf(os.Stderr, "ERROR fixStatements: %d: %s\n", i, l.String())
		}
		if was > 0 {
			// Process a statement.
			if strings.HasSuffix(l.code, ".clear();") {
				l.code = l.code[:len(l.code)-len(".clear();")] + " = nil"
			} else if m := reAssignment.FindStringSubmatch(l.code); m != nil {
				//Convert "foo bar = baz();" to "bar := baz();"
				l.code = m[1] + m[2] + ":=" + m[3]
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
func load(name string, keepSkip bool, doc map[string][]Line) (string, string) {
	raw, err := os.ReadFile(name)
	if err != nil {
		return "", ""
	}
	log.Printf("load(%s)", name)

	// Do a first pass to trim obvious stuff.
	var lines []Line
	hdr := ""
	inHdr := true
	for _, l := range strings.Split(string(raw), "\n") {
		if inHdr {
			if strings.HasPrefix(l, "//") {
				hdr += l + "\n"
				continue
			}
			inHdr = false
		}
		lines = append(lines, processLine(l))
	}

	lines = commentDefines(lines)
	lines = mergeParenthesis(lines)
	lines = fixCondition(lines)
	lines = commentExtern(lines)
	lines = processStructDefinition(lines)
	lines = processFunctionDeclaration(lines, doc)
	lines = processFunctionImplementation(lines, doc)
	lines = fixStatements(lines)

	//Comment assert()
	//Convert enum
	//Comment out namespace

	// At the very end, remove the trailing ";".
	// Skip consecutive empty lines.
	out := ""
	wasEmpty := false
	for _, l := range lines {
		if !keepSkip && l.skip {
			continue
		}
		l.code = strings.TrimSuffix(l.code, ";")
		s := l.String()
		if !keepSkip && wasEmpty && s == "" {
			continue
		}
		wasEmpty = s == ""
		out += s + "\n"
	}
	return hdr, out
}

// process processes a pair of .h/.cc files.
func process(outDir, inDir, root string, keepSkip bool) error {
	f := filepath.Join(outDir, root+".go")
	if content, _ := ioutil.ReadFile(f); len(content) != 0 {
		if !bytes.Contains(content, []byte("//go:build nobuild")) {
			// It is legitimate, do not overwrite.
			return nil
		}
	}
	doc := map[string][]Line{}
	hdr1, c1 := load(filepath.Join(inDir, root+".h"), keepSkip, doc)
	hdr2, c2 := load(filepath.Join(inDir, root+".cc"), keepSkip, doc)
	if len(doc) != 0 {
		//log.Printf("%v", doc)
	}
	out := hdr1
	if hdr1 != hdr2 {
		out += hdr2
	}
	out += "\n//go:build nobuild\n\npackage ginga\n\n" + c1 + c2
	return os.WriteFile(f, []byte(out), 0o644)
}

// gingaContent adds glue code to make transition a tad easier.
const gingaContent = `package ginga

import (
  "fmt"
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

func printf(f string, v...interface{}) {
  fmt.Printf(f, v...)
}

func fprintf(w io.Writer, f string, v...interface{}) {
  fmt.Fprintf(w, f, v...)
}
`

func mainImpl() error {
	v := flag.Bool("v", false, "verbose")
	inDir := flag.String("i", "src", "input directory")
	outDir := flag.String("o", ".", "output directory")
	keepSkip := flag.Bool("s", false, "keep skipped lines")
	flag.Parse()
	log.SetFlags(0)
	if !*v {
		log.SetOutput(ioutil.Discard)
	}

	files := flag.Args()
	if len(files) == 0 {
		// Get the list of source files.
		entries, err := os.ReadDir(*inDir)
		if err != nil {
			return err
		}
		roots := map[string]struct{}{}
		for _, e := range entries {
			n := e.Name()
			ext := filepath.Ext(n)
			if ext != ".h" && ext != ".cc" {
				continue
			}
			n = n[:len(n)-len(ext)]
			roots[n] = struct{}{}
		}
		if _, err := os.Stat(*outDir); os.IsNotExist(err) {
			if err = os.Mkdir(*outDir, 0o755); err != nil {
				return err
			}
		} else if err != nil {
			return err
		}

		// Process all the source files in order.
		files = make([]string, 0, len(roots))
		for root := range roots {
			files = append(files, root)
		}
		sort.Strings(files)
	}
	for _, root := range files {
		if err := process(*outDir, *inDir, root, *keepSkip); err != nil {
			return err
		}
	}

	// Inject a file with helpers.
	if err := ioutil.WriteFile(filepath.Join(*outDir, "ginja.go"), []byte(gingaContent), 0o644); err != nil {
		return err
	}

	// Insert a go.mod if missign.
	if _, err := os.Stat(filepath.Join(*outDir, "go.mod")); os.IsNotExist(err) {
		cmd := exec.Command("go", "mod", "init", "ginga")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		cmd.Dir = *outDir
		if err = cmd.Run(); err != nil {
			return err
		}
	}
	return nil
}

func main() {
	if err := mainImpl(); err != nil {
		fmt.Fprintf(os.Stderr, "cc2go: %s\n", err)
		os.Exit(1)
	}
}
