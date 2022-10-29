// Copyright 2021 Marc-Antoine Ruel. All rights reserved.
// Use of this source code is governed under the Apache License, Version 2.0
// that can be found in the LICENSE file.

package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"unicode"
)

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

// doSkip comments out the line.
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

func joinLines(prefix string, lines []Line) string {
	out := ""
	for _, l := range lines {
		out += prefix + l.String() + "\n"
	}
	return out
}

// Phase 1

const (
	// symbolSimple is a symbol that cannot be a template "std::set<int>" or
	// subtype "Foo:BAR".
	symbolSimple = `[A-Za-z][A-Za-z0-9_]*`
	// complexType is a type that can be a template "std::map<int, std::string>"
	// or subtype "Foo:BAR".
	complexType = `[A-Za-z][A-Za-z0-9_:<>, *&[\]]*[A-Za-z0-9_>*&\]]+`
)

var (
	// Used in phase 1

	// e.g. "struct Foo;"
	reForwardStruct = regexp.MustCompile(`^struct ` + symbolSimple + `;$`)
	// e.g. "protected:"
	reStructAccess = regexp.MustCompile(`^(public|protected|private):$`)
	// A string.
	reConstChar   = regexp.MustCompile(`const char\s?\*`)
	reStringPiece = regexp.MustCompile(`\bStringPiece\b`)
	// Handle reserved Go keyword.
	reLen = regexp.MustCompile(`\blen\b`)
	reVar = regexp.MustCompile(`\bvar\b`)

	asciiSpace = [256]uint8{'\t': 1, '\n': 1, '\v': 1, '\f': 1, '\r': 1, ' ': 1}
)

func countSpaces(l string) int {
	i := 0
	for ; i < len(l); i++ {
		if asciiSpace[l[i]] == 0 {
			break
		}
	}
	return i
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
	// Walk the line to find comments. Special handling of quote since "//" isn't a C++ comment.
	out.code = l
	inquote := false
	for i := 0; i < len(l)-1; i++ {
		if l[i] == '"' {
			inquote = !inquote
		} else if !inquote && l[i:i+2] == "//" {
			if b := strings.LastIndexFunc(l[:i], func(r rune) bool { return !unicode.IsSpace(r) }); b != -1 {
				i = b + 1
			}
			out.code = l[:i]
			out.comment = l[i:]
			break
		}
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
	out.code = replaceQuotesAware(out.code, "std::", "")
	out.code = replaceQuotesAware(out.code, "const string&", "string")
	out.code = replaceQuotesAware(out.code, ".c_str()", "")
	out.code = replaceQuotesAware(out.code, "->", ".")
	out.code = replaceQuotesAware(out.code, "NULL", "nil")
	out.code = reConstChar.ReplaceAllString(out.code, "string")
	out.code = reStringPiece.ReplaceAllString(out.code, "string")
	// Handle reserved Go keywords.
	out.code = reLen.ReplaceAllString(out.code, "len2")
	out.code = reVar.ReplaceAllString(out.code, "var2")
	return out
}

// replaceQuotesAware does a strings.ReplaceAll() while skipping content of
// double-quotes.
func replaceQuotesAware(l, base, repl string) string {
	inquote := false
	b := len(base)
	out := ""
	for i := 0; i < len(l); i++ {
		c := l[i]
		if c == '"' {
			inquote = !inquote
			out += string(c)
		} else if !inquote && strings.HasPrefix(l[i:], base) {
			out += repl
			i += b - 1
		} else {
			out += string(c)
		}
	}
	return out
}

// Rest, in order.

// commentDefines comments out #include, #define and #if, including multi-lines
// defines.
//
// It comments out all the #else / #endif part because one has to pick, and
// keep both #if and #else parts leads to confusing broken code.
func commentDefines(lines []Line) []Line {
	var out []Line
	cont := false
	elseBlock := false
	for _, l := range lines {
		if elseBlock {
			elseBlock = !strings.HasPrefix(l.code, "#endif")
			l.doSkip()
		} else if cont {
			cont = strings.HasSuffix(l.code, "\\")
			l.doSkip()
		} else if strings.HasPrefix(l.code, "#else") {
			elseBlock = true
			l.doSkip()
		} else if strings.HasPrefix(l.code, "#") {
			cont = strings.HasSuffix(l.code, "\\")
			l.doSkip()
		} else {
			cont = false
		}
		out = append(out, l)
	}
	return out
}

//

// e.g. "extern "C" {"
var reExtern = regexp.MustCompile(`^(\s*)extern (.*)`)

// commentExtern comments out extern and extern "C" {.
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

//

// countBrackets counts { (+1) and } (-1) and returns the delta. It ignores
// anything within parenthesis.
func countBrackets(l string) int {
	parenthesis := 0
	count := 0
	for _, r := range l {
		if parenthesis <= 0 {
			switch r {
			case '{':
				count++
			case '}':
				count--
			}
		}
		switch r {
		case '(':
			parenthesis++
		case ')':
			parenthesis--
		}
	}
	return count
}

// findClosingBracket returns the index of the line where the corresponding
// closing bracket is located.
//
// It works even if the opening bracket isn't on the first line.
func findClosingBracket(lines []Line) int {
	b := 0
	found := false
	for i, l := range lines {
		b += countBrackets(l.code)
		found = found || strings.Contains(l.code, "{")
		if found && b == 0 {
			return i
		}
	}
	return -1
}

// commentNamespace comment out "namespace foo {".
//
// Running it before processFunctionImplementation makes the function easier to
// implement.
func commentNamespace(lines []Line) []Line {
	var out []Line
	brackets := 0
	inNamespace := false
	for _, l := range lines {
		brackets += countBrackets(l.code)
		if brackets == 1 && !inNamespace {
			if strings.HasPrefix(l.code, "namespace") && strings.HasSuffix(l.code, "{") {
				l.doSkip()
				inNamespace = true
			}
		}
		if brackets == 0 && l.code == "}" && inNamespace {
			l.doSkip()
			inNamespace = false
		}
		out = append(out, l)
	}
	return out
}

//

func mergeStrings(lines []Line) []Line {
	var out []Line
	was := false
	for _, l := range lines {
		if was && strings.HasPrefix(l.code, "\"") {
			// Merge with the prefix line.
			i := len(out) - 1
			out[i].original = append(out[i].original, l.original...)
			out[i].code = out[i].code[:len(out[i].code)-1] + l.code[1:]
			if l.comment != "" {
				if out[i].comment != "" {
					panic(l.String())
				}
				out[i].comment = l.comment
			}
		} else {
			out = append(out, l)
		}
		was = strings.HasSuffix(l.code, "\"")
	}
	return out
}

//

var reStructDefinition = regexp.MustCompile(`^struct (` + symbolSimple + `)\s*(.+)$`)

// processStructDefinition rewrites the structs.
func processStructDefinition(lines []Line) []Line {
	var out []Line
	for i := 0; i < len(lines); i++ {
		l := lines[i]
		if m := reStructDefinition.FindStringSubmatch(l.code); m != nil {
			// Sometimes it's a variable declaration.
			suffix := m[2]
			if !strings.HasSuffix(suffix, ";") {
				for strings.HasSuffix(suffix, ",") {
					// It's a multi-lines definition. We need to skip the next line.
					i++
					l.original = append(l.original, lines[i].original...)
					suffix = lines[i].code
				}
				if !strings.HasSuffix(suffix, "{") {
					panic(l.code)
				}
				l.code = "type " + m[1] + " struct {"
			}
		}
		out = append(out, l)
	}
	return out
}

//

// mergeParenthesis makes all () on one line to make function declaration
// easier to parse.
//
// Merges parenthesis both for function declaration, call, conditions and
// loops.
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
				// Output the line and reset the accumulator.
				out = append(out, acc)
				acc = Line{}
			} else {
				// Join the lines with a space.
				acc.code += " "
			}
		}
	}
	if len(acc.code) != 0 {
		panic(acc.code)
	}
	return out
}

//

const arguments = `[a-zA-Z0-9\-\. *,=<>_:&\[\]"]*`

var (
	reGoStruct               = regexp.MustCompile(`^type (` + symbolSimple + `) struct {$`)
	reConstructorDeclaration = regexp.MustCompile(`^(` + symbolSimple + `)\s?\([a-zA-Z *,=<>_:&\[\]]*\);$`)
	// e.g. "void bar();" or "virtual void bar() const = 0;"
	// 1 is return type, 2 is name.
	reFuncDeclaration = regexp.MustCompile(`^(?:virtual |static |)(` + complexType + `)\s+(` + symbolSimple + `)\s?\((` + arguments + `)\)(?: const|)\s*(?:= 0|)\s*;$`)
)

// processFunctionDeclaration comments out forward declarations, grabbing
// docstring along the way.
//
// Go doesn't need forward declaration so comment them all.
func processFunctionDeclaration(lines []Line, doc map[string][]Line) []Line {
	var out []Line
	structName := ""
	brackets := 0
	for _, l := range lines {
		brackets += countBrackets(l.code)
		if brackets == 1 {
			if m := reGoStruct.FindStringSubmatch(l.code); m != nil {
				structName = m[1]
			}
		}
		if brackets == 0 {
			structName = ""
		}
		if structName != "" && brackets == 1 {
			if m := reConstructorDeclaration.FindStringSubmatch(l.code); m != nil {
				l.doSkip()
				// Associate the function description if available.
				for i := len(out) - 1; i >= 0 && out[i].code == "" && out[i].comment != ""; i-- {
					n := getID(structName, m[1])
					doc[n] = append([]Line{out[i]}, doc[n]...)
					out[i].skip = true
				}
			}
		}
		// It can both be a function if brackets == 0 or a method if brackets > 1.
		if m := reFuncDeclaration.FindStringSubmatch(l.code); m != nil {
			// It's super annoying that "return Foo();" triggers but there's
			// nothing to do but explicitly ignore.
			if m[1] != "return" {
				// The main difference between a function declaration and an
				// implementation is that each argument has a type, so look for spaces.
				// Templates are annoying,
				spaces := true
				if m[3] != "" {
					spaces = false
					for _, a := range strings.Split(m[3], ", ") {
						if strings.Contains(a, " ") {
							spaces = true
							break
						}
					}
				}
				if spaces {
					l.doSkip()
					// Associate the function description if available.
					for i := len(out) - 1; i >= 0 && out[i].code == "" && out[i].comment != ""; i-- {
						n := getID(structName, m[2])
						doc[n] = append([]Line{out[i]}, doc[n]...)
						out[i].skip = true
					}
				}
			}
		}
		out = append(out, l)
	}
	return out
}

// getID returns the key for use in the doc map.
func getID(structName, funcName string) string {
	if structName == "" {
		return funcName
	}
	return structName + "." + funcName
}

//

const implementation = `{(?:|\s*}|\s*.+\s*})`

var (
	// 1 is return type, 2 is name, 3 is args, 4 is brackets
	reFuncImplementation = regexp.MustCompile(`^(?:virtual |static |)(` + complexType + `)\s+(` + symbolSimple + `)\s?\((` + arguments + `)\)(?:\s*const|)\s*(` + implementation + `)$`)
	// 1 is return type, 2 is class, 3 is name, 4 is args, 5 is brackets
	reMethodImplementation = regexp.MustCompile(`^(?:virtual |static |)(` + complexType + `)\s+(` + symbolSimple + `)::(` + symbolSimple + `)\s?\((` + arguments + `)\)(?:\s*const|)\s*(` + implementation + `)$`)
	// e.g. "TEST_F(DepfileParserTest, Continuation) {"
	reGoogleTestfunc = regexp.MustCompile(`^TEST(?:|_F)\((` + symbolSimple + `), (` + symbolSimple + `)\) {$`)
)

// processFunctionImplementation handles function implementations.
//
// It leverages doc that was populated in processFunctionDeclaration().
func processFunctionImplementation(lines []Line, doc map[string][]Line) []Line {
	var out []Line
	cur := ""
	brackets := 0
	funcName := ""
	structName := ""
	for _, l := range lines {
		brackets += countBrackets(l.code)
		if brackets == 1 {
			if m := reGoStruct.FindStringSubmatch(l.code); m != nil {
				structName = m[1]
			}
		}
		if brackets == 0 {
			structName = ""
		}

		// Easy to replace google test function into a proper Go function right away.
		if m := reGoogleTestfunc.FindStringSubmatch(l.code); m != nil {
			l.code = "func Test" + m[1] + "_" + m[2] + "(t *testing.T) {"
		} else if m := reFuncImplementation.FindStringSubmatch(l.code); m != nil {
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
			ret := m[1]
			funcName = m[2]
			args, err := processArgs(m[3])
			rest := m[4]
			if err != nil {
				panic(fmt.Sprintf("%s: %s", l.code, err))
			}
			var extra []Line
			if strings.HasPrefix(rest, "{") && countBrackets(rest) == 0 {
				// One liner, expand.
				if !strings.HasSuffix(rest, "}") {
					panic(rest)
				}
				if middle := strings.TrimSpace(rest[1 : len(rest)-1]); middle != "" {
					extra = append(extra, Line{code: middle, indent: "\t"})
					extra = append(extra, Line{code: "}"})
					rest = "{"
				}
			}
			// Extracting a function method defined inlined will be handled in
			// extractEmbedded later.
			l.code = rewriteFunc(structName, funcName, args, ret, rest)
			if d := doc[getID(structName, funcName)]; len(d) != 0 {
				// Insert the doc there, updating the indentation.
				for _, x := range d {
					x.indent = l.indent
					out = append(out, x)
				}
			}
			//panic(joinLines("* ", extra))
			out = append(out, l)
			out = append(out, extra...)
			continue
		} else if m := reMethodImplementation.FindStringSubmatch(l.code); m != nil {
			// 1 is return type, 2 is class, 3 is name, 4 is args, 5 is brackets
			if m[1] == "if" {
				// It's annoying that this triggers, just shrug it.
				out = append(out, l)
				continue
			}
			if m[1] == "void" {
				m[1] = ""
			}
			ret := m[1]
			structName = m[2]
			funcName = m[3]
			args, err := processArgs(m[4])
			rest := m[5]
			if err != nil {
				panic(fmt.Sprintf("%s: %s", l.code, err))
			}
			var extra []Line
			if strings.HasPrefix(rest, "{") && countBrackets(rest) == 0 {
				// One liner, expand.
				if !strings.HasSuffix(rest, "}") {
					panic(rest)
				}
				if middle := strings.TrimSpace(rest[1 : len(rest)-1]); middle != "" {
					extra = append(extra, Line{code: middle, indent: "\t"})
					extra = append(extra, Line{code: "}"})
					rest = "{"
				}
			}
			l.code = rewriteFunc(structName, funcName, args, ret, rest)
			if d := doc[getID(structName, funcName)]; len(d) != 0 {
				// Insert the doc there, updating the indentation.
				for _, x := range d {
					x.indent = l.indent
					out = append(out, x)
				}
			}
			out = append(out, l)
			out = append(out, extra...)
			continue
		}
		out = append(out, l)
	}
	return out
}

func rewriteFunc(structName, funcName, args, ret, rest string) string {
	if structName == "" {
		if ret == "" {
			return "func " + funcName + "(" + args + ") " + rest
		}
		return "func " + funcName + "(" + args + ") " + reduceComplexType(ret) + " " + rest
	}
	receiver := strings.ToLower(structName[:1])
	if ret == "" {
		return "func (" + receiver + " *" + structName + ") " + funcName + "(" + args + ") " + rest
	}
	return "func (" + receiver + " *" + structName + ") " + funcName + "(" + args + ") " + reduceComplexType(ret) + " " + rest
}

// processArgs process the arguments in a function declaration.
//
// Called by processFunctionImplementation for both functions and methods.
func processArgs(l string) (string, error) {
	if l == "" {
		return l, nil
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
	var err error
	for i, a := range args {
		var err1 error
		if args[i], err1 = processArg(a); err == nil {
			err = err1
		}
	}
	return strings.Join(args, ", "), err
}

// processArg process one argument string in a function declaration. It's one
// of the most tedious thing to do when converting code manually.
func processArg(a string) (string, error) {
	c := strings.LastIndex(a, " ")
	if c == -1 {
		// There's a type with no name. Return it as-is
		return a, nil
		//return "", fmt.Errorf("failed to process argument %q", a)
	}
	t := reduceComplexType(a[:c])
	n := a[c+1:]
	for strings.HasPrefix(n, "*") {
		n = n[1:]
		t = "*" + t
	}
	if strings.HasSuffix(n, "[]") {
		n = n[:len(n)-2]
		t = "[]" + t
	}
	return n + " " + t, nil
}

//

// extractEmbedded extracts embedded structs, enum and methods from within a
// struct.
//
// It is different than fixInsideStructs below that here it extract content
// from the struct.
func extractEmbedded(lines []Line) []Line {
	var out []Line
	for i := 0; i < len(lines); i++ {
		if m := reGoStruct.FindStringSubmatch(lines[i].code); m != nil {
			// Find the end, and process this part only.
			end := findClosingBracket(lines[i:])
			if end == -1 {
				panic(joinLines("x ", lines))
			}
			end += i
			out = append(out, lines[i])
			// If there's content inside.
			if i < end+1 {
				inner, outer := huntForEmbedded(lines[i+1:end], m[1])
				out = append(out, inner...)
				// And the trailing line, if applicable.
				out = append(out, lines[end])
				// Then the stuff.
				out = append(out, outer...)
				i = end
			} else if i == end+1 {
				// And the trailing line, if applicable.
				out = append(out, lines[end])
				i = end
			}
		} else {
			out = append(out, lines[i])
		}
	}
	return out
}

func huntForEmbedded(lines []Line, structName string) ([]Line, []Line) {
	var inner, outer []Line
	for i := 0; i < len(lines); i++ {
		if m := reGoStruct.FindStringSubmatch(lines[i].code); m != nil {
			// Grab the documentation too and pop it from the output.
			start := walkbackDoc(lines[:i])
			inner = inner[:len(inner)-(i-start)]
			// Find the end, and process this part only.
			end := findClosingBracket(lines[i:]) + i
			indent := lines[i].indent
			// Append the type declaration and its documentation.
			outer = append(outer, dedentLines(indent, lines[start:i+1])...)
			if i+1 < end {
				// Recurse!
				inner2, outer2 := huntForEmbedded(lines[i+1:end], m[1])
				outer = append(outer, dedentLines(indent, inner2)...)
				// And the trailing line, if applicable.
				outer = append(outer, lines[end])
				outer = append(outer, dedentLines(indent, outer2)...)
				i = end
			} else if i+1 == end {
				if !strings.HasSuffix(lines[end].code, "}") && !strings.HasSuffix(lines[end].code, "};") {
					panic(lines[end].String())
				}
				outer = append(outer, dedentLine(indent, lines[end]))
				i = end
			} else if i == end {
				panic(end)
			}
		} else if strings.HasPrefix(lines[i].code, "func ") || strings.HasPrefix(lines[i].code, "enum ") {
			// Extract functions and enums the same way. Both will be
			// processed/converted into Go types later.
			// At this point, the functions were already converted to be Go method
			// style. The only exception is constructor and destructor.

			// Grab the documentation too and pop it from the output.
			start := walkbackDoc(lines[:i])
			inner = inner[:len(inner)-(i-start)]
			// Find the end, and process this part only.
			end := findClosingBracket(lines[i:]) + i
			// Append the function implementation and its documentation.
			outer = append(outer, dedentLines(lines[i].indent, lines[start:i+1])...)
			if i+1 < end {
				outer = append(outer, dedentLines(lines[i].indent, lines[i+1:end+1])...)
				i = end
			} else if i+1 == end {
				//panic(joinLines("- ", lines))
				i++
			}
		} else if isConstructor(lines[i].code, structName) {
			if strings.HasSuffix(lines[i].code, ";") && !strings.Contains(lines[i].code, "{") {
				// Zap declaration without implementation, since it's of no use.
				// Grab the documentation too and pop it from the output.
				start := walkbackDoc(lines[:i])
				inner = inner[:len(inner)-(i-start)]
				l := lines[i]
				l.doSkip()
				inner = append(inner, l)
			} else {
				// We want to move it outside. Finding brackets can be tricky because
				// often code looks like this:
				//   Foo()
				//    : bar(0),
				//      baz(1) {}
				// First is to find all the lines, then extract.
				// Grab the documentation too and pop it from the output.
				start := walkbackDoc(lines[:i])
				inner = inner[:len(inner)-(i-start)]
				// Find the end, and process this part only.
				end := findClosingBracket(lines[i:]) + i
				outer = append(outer, rewriteConstructor(dedentLines(lines[i].indent, lines[i:end+1]), structName)...)
				i = end
			}
		} else if isDestructor(lines[i].code, structName) {
			// Grab the documentation too and pop it from the output.
			start := walkbackDoc(lines[:i])
			inner = inner[:len(inner)-(i-start)]
			if strings.HasSuffix(lines[i].code, "{}") || (strings.HasSuffix(lines[i].code, ";") && !strings.Contains(lines[i].code, "{")) {
				// One liner virtual destructor, just zap it.
				// Also zap declaration without implementation, since it's of no use.
				l := lines[i]
				l.doSkip()
				inner = append(inner, l)
			} else {
				end := findClosingBracket(lines[i:]) + i
				// Append the function implementation and its documentation.
				outer = append(outer, dedentLines(lines[i].indent, lines[start:i+1])...)
				if i+1 < end {
					outer = append(outer, dedentLines(lines[i].indent, lines[i+1:end+1])...)
					i = end
				} else if i+1 == end {
					//panic(joinLines("- ", lines))
					i++
				}
			}
		} else if strings.HasPrefix(lines[i].code, "typedef ") {
			// Move typedef outside of the struct since Go does not support embedded
			// type but do not precess it yet, it will be mass processed later.
			l := lines[i]
			l.indent = ""
			outer = append(outer, l)
		} else {
			inner = append(inner, lines[i])
		}
	}
	return inner, outer
}

// walkbackDoc starts at the end and find the start of the documentation.
func walkbackDoc(lines []Line) int {
	start := len(lines)
	for ; start > 0 && lines[start-1].code == "" && strings.HasPrefix(lines[start-1].comment, "//"); start-- {
	}
	return start
}

func dedentLine(indent string, l Line) Line {
	l.indent = strings.TrimPrefix(l.indent, indent)
	return l
}

func dedentLines(indent string, lines []Line) []Line {
	out := make([]Line, len(lines))
	copy(out, lines)
	for i, l := range out {
		out[i].indent = strings.TrimPrefix(l.indent, indent)
	}
	return out
}

func isConstructor(code, structName string) bool {
	return strings.HasPrefix(strings.TrimPrefix(code, "explicit "), structName+"(")
}

func isDestructor(code, structName string) bool {
	return strings.HasPrefix(strings.TrimPrefix(code, "virtual "), "~"+structName+"(")
}

func rewriteConstructor(lines []Line, structName string) []Line {
	var originals []string
	for _, l := range lines {
		originals = append(originals, l.original...)
	}
	code := strings.TrimPrefix(lines[0].code, "explicit ")
	if !strings.HasPrefix(code, structName+"(") {
		panic(lines[0].String())
	}
	// Find the matching closing parenthesis.
	args, rest := findMatchingParenthesis(strings.TrimPrefix(code, structName+"("))
	var err error
	if args, err = processArgs(args); err != nil {
		panic(err)
	}
	out := []Line{
		{original: originals, code: "func New" + structName + "(" + args + ") " + structName + " {"},
		{indent: "\t", code: "return " + structName + "{"},
	}

	rest = strings.TrimSpace(rest)
	for _, l := range lines[1:] {
		rest += " " + strings.TrimSpace(l.code)
	}
	rest = strings.TrimSpace(rest)
	rest = strings.TrimPrefix(rest, ":")
	rest = strings.TrimSuffix(rest, "{}")
	rest = strings.TrimSpace(rest)
	impl := ""
	if i := strings.Index(rest, "{"); i != -1 {
		impl = rest[i:]
		rest = rest[:i]
	}
	// Split manually with a state machine, since a regexp can't do it.
	acc := ""
	for i := 0; i < len(rest); i++ {
		c := rest[i]
		switch c {
		case '(':
			init, r := findMatchingParenthesis(rest[i+1:])
			// Go memory is guaranteed to be zero initialized so trim these.
			if init != "0" && init != "false" {
				out = append(out, Line{indent: "\t\t", code: acc + ": " + init + ","})
			}
			i = -1
			rest = strings.TrimPrefix(r, ",")
			rest = strings.TrimSpace(rest)
			acc = ""
		default:
			acc += string(c)
		}
	}
	out = append(out, Line{indent: "\t", code: "}"})
	if impl != "" {
		out = append(out, Line{indent: "\t", code: impl})
	}
	out = append(out, Line{code: "}"})
	return out
}

//

var (
	reGoFunc   = regexp.MustCompile(`^func (` + symbolSimple + `)\(.+$`)
	reGoMethod = regexp.MustCompile(`^func \(([a-z]) \*(` + symbolSimple + `)\) (` + symbolSimple + `)\(.+$`)
)

// fixInsideFuncs calls within with the block inside a function.
func fixInsideFuncs(lines []Line, within func(lines []Line, receiver, structName, funcName string) []Line) []Line {
	var out []Line
	for i := 0; i < len(lines); i++ {
		l := lines[i]
		// Look for functions. It is easy now that the functions start with
		// "func " and that they are on one line.
		receiver := ""
		structName := ""
		funcName := ""
		if m := reGoFunc.FindStringSubmatch(l.code); m != nil {
			funcName = m[1]
		} else if m := reGoMethod.FindStringSubmatch(l.code); m != nil {
			receiver = m[1]
			structName = m[2]
			funcName = m[3]
		} else if strings.HasPrefix(l.code, "func ") {
			panic(l.code)
		}
		if funcName != "" {
			// Find the end, and process this part only.
			end := findClosingBracket(lines[i:]) + i
			// Append the function.
			out = append(out, l)
			if i+1 < end {
				//log.Printf("Within func:\n%s", joinLines("- ", lines[i+1:end]))
				out = append(out, within(lines[i+1:end], receiver, structName, funcName)...)
				// And the trailing line, if applicable.
				out = append(out, lines[end])
				i = end
			} else if i+1 == end {
				// And the trailing line, if applicable.
				out = append(out, lines[end])
				i = end
			}
		} else {
			out = append(out, l)
		}
	}
	return out
}

//

// fixIf does two things:
//   - Fix one liners.
//   - Remove the extra parenthesis.
func fixIf(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for i := 0; i < len(lines); {
		// Process "one line" at a time. The line can read multiple lines if it's a
		// oneliner condition.
		j, more := fixIfOneline(lines[i:])
		i += j
		out = append(out, more...)
	}
	return out
}

var (
	reSimpleWord    = regexp.MustCompile(`^[a-z]+$`)
	reNotSimpleWord = regexp.MustCompile(`^![a-z]+$`)
)

// cleanCond takes the condition and returns the cleaned version and the rest.
func cleanCond(cond string) (string, string) {
	cond, rest := findMatchingParenthesis(cond)
	rest = strings.TrimSpace(rest)
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
	return cond, rest
}

// findMatchingParenthesis returns the closing parenthesis
func findMatchingParenthesis(l string) (string, string) {
	i := 0
	count := 1
	for ; count != 0 && i < len(l); i++ {
		if l[i] == '(' {
			count++
		} else if l[i] == ')' {
			count--
		}
	}
	return l[:i-1], l[i:]
}

// fixIfOneline handles one line or more lines if a one liner is found.
//
// It returns the number of lines consumed and the corresponding output.
func fixIfOneline(lines []Line) (int, []Line) {
	l := lines[0]
	if strings.HasPrefix(l.code, "if (") {
		// Trim the very first and very last parenthesis. There can be inside due
		// to function calls.
		cond, rest := cleanCond(l.code[len("if ("):])
		l.code = "if " + cond + " {"

		// One liner?
		if rest != "{" {
			j := 0
			var more []Line
			if rest == "" {
				j, more = fixIfOneliner(l.indent, lines[1:], false)
				j++
			} else {
				// Inject rest as a statement.
				j, more = fixIfOneliner(l.indent, append([]Line{{indent: l.indent + "\t", code: rest}}, lines[1:]...), false)
			}
			return j, append([]Line{l}, more...)
		}

	} else if strings.HasPrefix(l.code, "} else if (") {
		// Trim the very first and very last parenthesis. There can be inside due
		// to function calls.
		cond, rest := cleanCond(l.code[len("} else if ("):])
		l.code = "} else if " + cond + " {"

		// One liner?
		if rest != "{" {
			j := 0
			var more []Line
			if rest == "" {
				j, more = fixIfOneliner(l.indent, lines[1:], false)
				j++
			} else {
				// Inject rest as a statement.
				j, more = fixIfOneliner(l.indent, append([]Line{{indent: l.indent + "\t", code: rest}}, lines[1:]...), false)
			}
			return j, append([]Line{l}, more...)
		}

	} else if strings.HasPrefix(l.code, "} else") {
		// One liner?
		if !strings.HasSuffix(l.code, "{") {
			l.code += " {"
			j, more := fixIfOneliner(l.indent, lines[1:], true)
			return 1 + j, append([]Line{l}, more...)
		}
	}

	return 1, []Line{l}
}

// fixIfOneliner fixes a one liner.
//
// One liners can be embedded into each other, which makes it a bit tricky to
// process.
func fixIfOneliner(indent string, lines []Line, isElse bool) (int, []Line) {
	i, out := fixIfOneline(lines)

	// Special case else, but only if it was a if.
	if !isElse && i < len(lines) && strings.HasPrefix(lines[i].code, "else") {
		l := lines[i]
		l.code = "} " + l.code
		j, more := fixIfOneline(append([]Line{l}, lines[i+1:]...))
		return i + j, append(out, more...)
	}
	return i, append(out, Line{indent: indent, code: "}"})
}

//

// fixWhile does two things:
//   - Fix one liners.
//   - Remove the extra parenthesis.
func fixWhile(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for i := 0; i < len(lines); {
		// Process "one line" at a time. The line can read multiple lines if it's a
		// oneliner condition.
		j, more := fixWhileOneline(lines[i:])
		i += j
		out = append(out, more...)
	}
	return out
}

// fixWhileOneline handles one line or more lines if a one liner is found.
//
// It returns the number of lines consumed and the corresponding output.
func fixWhileOneline(lines []Line) (int, []Line) {
	l := lines[0]
	if strings.HasPrefix(l.code, "while (") {
		// Trim the very first and very last parenthesis. There can be inside due
		// to function calls.
		cond, rest := cleanCond(l.code[len("while ("):])
		l.code = "for " + cond + " {"

		// One liner?
		if rest != "{" {
			j := 0
			var more []Line
			if rest == "" {
				j, more = fixWhileOneliner(l.indent, lines[1:])
				j++
			} else {
				// Inject rest as a statement.
				j, more = fixWhileOneliner(l.indent, append([]Line{{indent: l.indent + "\t", code: rest}}, lines[1:]...))
			}
			return j, append([]Line{l}, more...)
		}
	}

	return 1, []Line{l}
}

// fixWhileOneliner fixes a one liner.
//
// One liners can be embedded into each other, which makes it a bit tricky to
// process.
func fixWhileOneliner(indent string, lines []Line) (int, []Line) {
	i, out := fixWhileOneline(lines)
	return i, append(out, Line{indent: indent, code: "}"})
}

//

// fixFor does two things:
//   - Fix one liners.
//   - Remove the extra parenthesis.
func fixFor(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for i := 0; i < len(lines); {
		// Process "one line" at a time. The line can read multiple lines if it's a
		// oneliner condition.
		j, more := fixForOneline(lines[i:])
		i += j
		out = append(out, more...)
	}
	return out
}

// fixForOneline handles one line or more lines if a one liner is found.
//
// It returns the number of lines consumed and the corresponding output.
func fixForOneline(lines []Line) (int, []Line) {
	l := lines[0]
	if strings.HasPrefix(l.code, "for (") {
		// Trim the very first and very last parenthesis. There can be inside due
		// to function calls.
		cond, rest := cleanForParams(l.code[len("for ("):])
		l.code = "for " + cond + " {"

		// One liner?
		if rest != "{" {
			j := 0
			var more []Line
			if rest == "" {
				j, more = fixForOneliner(l.indent, lines[1:])
				j++
			} else {
				// Inject rest as a statement.
				j, more = fixForOneliner(l.indent, append([]Line{{indent: l.indent + "\t", code: rest}}, lines[1:]...))
			}
			return j, append([]Line{l}, more...)
		}
	}

	return 1, []Line{l}
}

var (
	// e.g. "vector<Node*>::iterator out_node = (*e).outputs_.begin()"
	reAssignment = regexp.MustCompile(`^(` + complexType + `) (\*?` + symbolSimple + `) = ([a-zA-Z0-9 \*,<>\._:&\[\]()]+);$`)
)

// cleanForParams takes the for parameters and returns the cleaned version and
// the rest.
func cleanForParams(cond string) (string, string) {
	i := 0
	count := 1
	for ; count != 0 && i < len(cond); i++ {
		if cond[i] == '(' {
			count++
		} else if cond[i] == ')' {
			count--
		}
	}
	rest := strings.TrimSpace(cond[i:])
	cond = cond[:i-1]
	// We never care about const denomination.
	cond = strings.TrimPrefix(cond, "const ")

	if strings.HasPrefix(cond, "auto") {
		// It's a new style iteration.
		cond = strings.TrimPrefix(cond, "auto")
		cond = strings.TrimLeft(cond, " ")
		cond = strings.TrimLeft(cond, "&")
		cond = strings.TrimLeft(cond, " ")
		// What remains is "foo : bar".
	}
	parts := strings.SplitN(cond, ";", 3)
	if len(parts) != 3 {
		panic(cond)
	}
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}

	if m := reAssignment.FindStringSubmatch(parts[0] + ";"); m != nil {
		// Ignore type.
		parts[0] = m[2] + " := " + m[3]
	}

	if strings.HasPrefix(parts[2], "++") {
		parts[2] = parts[2][2:] + "++"
	} else if strings.HasPrefix(parts[2], "--") {
		parts[2] = parts[2][2:] + "--"
	}

	return strings.Join(parts, "; "), rest
}

// fixForOneliner fixes a one liner.
//
// One liners can be embedded into each other, which makes it a bit tricky to
// process.
func fixForOneliner(indent string, lines []Line) (int, []Line) {
	i, out := fixForOneline(lines)
	return i, append(out, Line{indent: indent, code: "}"})
}

//

// fixAssignments converts all assignments in a block.
func fixAssignments(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for _, l := range lines {
		if m := reAssignment.FindStringSubmatch(l.code); m != nil {
			// Ignore type.
			// Pointer type can be touching the variable name.
			l.code = strings.TrimPrefix(m[2], "*") + " := " + m[3]
		}
		out = append(out, l)
	}
	return out
}

//

func fixPreIncrement(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for _, l := range lines {
		if strings.HasPrefix(l.code, "++") {
			l.code = strings.TrimRight(l.code[2:], ";") + "++"
		}
		if strings.HasPrefix(l.code, "--") {
			l.code = strings.TrimRight(l.code[2:], ";") + "--"
		}
		out = append(out, l)
	}
	return out
}

//

func fixClear(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for _, l := range lines {
		if strings.HasSuffix(l.code, ".clear();") {
			l.code = l.code[:len(l.code)-len(".clear();")] + " = nil"
		}
		out = append(out, l)
	}
	return out
}

//

var (
	reAssert          = regexp.MustCompile(`^assert\((.+)\);$`)
	reGoogleTestEQ    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_EQ\(([^,]+), (.+)\);$`)
	reGoogleTestNE    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_NE\(([^,]+), (.+)\);$`)
	reGoogleTestGT    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_GT\(([^,]+), (.+)\);$`)
	reGoogleTestGE    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_GE\(([^,]+), (.+)\);$`)
	reGoogleTestLT    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_LT\(([^,]+), (.+)\);$`)
	reGoogleTestLE    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_LE\(([^,]+), (.+)\);$`)
	reGoogleTestTrue  = regexp.MustCompile(`^(?:ASSERT|EXPECT)_TRUE\((.+)\);$`)
	reGoogleTestFalse = regexp.MustCompile(`^(?:ASSERT|EXPECT)_FALSE\((.+)\);$`)
)

// fixAsserts handles asserts inside a function.
func fixAsserts(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for _, l := range lines {
		if m := reAssert.FindStringSubmatch(l.code); m != nil {
			l.code = "if !" + m[1] + " { panic(\"oops\") }"
		} else if m := reGoogleTestEQ.FindStringSubmatch(l.code); m != nil {
			l.code = "if " + m[1] + " != " + m[2] + " { t.Fatal(\"expected equal\") }"
		} else if m := reGoogleTestNE.FindStringSubmatch(l.code); m != nil {
			l.code = "if " + m[1] + " == " + m[2] + " { t.Fatal(\"expected different\") }"
		} else if m := reGoogleTestGT.FindStringSubmatch(l.code); m != nil {
			l.code = "if " + m[1] + " <= " + m[2] + " { t.Fatal(\"expected greater\") }"
		} else if m := reGoogleTestGE.FindStringSubmatch(l.code); m != nil {
			l.code = "if " + m[1] + " < " + m[2] + " { t.Fatal(\"expected greater or equal\") }"
		} else if m := reGoogleTestLT.FindStringSubmatch(l.code); m != nil {
			l.code = "if " + m[1] + " >= " + m[2] + " { t.Fatal(\"expected less or equal\") }"
		} else if m := reGoogleTestLE.FindStringSubmatch(l.code); m != nil {
			l.code = "if " + m[1] + " > " + m[2] + " { t.Fatal(\"expected less\") }"
		} else if m := reGoogleTestTrue.FindStringSubmatch(l.code); m != nil {
			l.code = "if !" + m[1] + " { t.Fatal(\"expected true\") }"
		} else if m := reGoogleTestFalse.FindStringSubmatch(l.code); m != nil {
			l.code = "if " + m[1] + " { t.Fatal(\"expected false\") }"
		}
		out = append(out, l)
	}
	return out
}

//

var reVariable = regexp.MustCompile(`^(?:const |struct |)(` + complexType + `) (\*?` + symbolSimple + `);$`)

func fixVariableDeclaractions(lines []Line, receiver, structName, funcName string) []Line {
	var out []Line
	for _, l := range lines {
		if m := reVariable.FindStringSubmatch(l.code); m != nil && m[1] != "return" {
			if m[1] != "typedef" {
				switch t := reduceComplexType(m[1]); t {
				case "string":
					l.code = m[2] + " := \"\""
				case "int":
					l.code = m[2] + " := 0"
				case "float64":
					l.code = m[2] + " := 0."
				default:
					l.code = "var " + m[2] + " " + t
				}
			} else {
				panic(l.code)
			}
		}
		out = append(out, l)
	}
	return out
}

var (
	reVector = regexp.MustCompile(`^vector<(` + complexType + `)>$`)
	reQueue  = regexp.MustCompile(`^queue<(` + complexType + `)>$`)
	reSet    = regexp.MustCompile(`^set<(` + complexType + `)>$`)
	reMap    = regexp.MustCompile(`^map<(` + complexType + `), (` + complexType + `)>$`)
)

// reduceComplexType reduces a complex type, e.g. "vector<uint64_t>" to
// "[]uint64".
func reduceComplexType(t string) string {
	t = strings.TrimPrefix(t, "const ")
	t = strings.TrimSuffix(t, " const")
	prefix := ""
	if strings.HasSuffix(t, "*") || strings.HasSuffix(t, "&") {
		prefix = "*"
		t = t[:len(t)-1]
	} else if t[0] == '*' {
		prefix = "*"
		t = t[1:]
	}
	// Handle common stdlib containers.
	if m := reVector.FindStringSubmatch(t); m != nil {
		t = "[]" + reduceComplexType(m[1])
	} else if m := reQueue.FindStringSubmatch(t); m != nil {
		t = "[...]" + reduceComplexType(m[1])
	} else if m := reSet.FindStringSubmatch(t); m != nil {
		t = "map[" + reduceComplexType(m[1]) + "]struct{}"
	} else if m := reMap.FindStringSubmatch(t); m != nil {
		t = "map[" + reduceComplexType(m[1]) + "]" + reduceComplexType(m[2])
	} else {
		switch t {
		case "return":
			panic(t)
		case "int32_t":
			t = "int32"
		case "int64_t":
			t = "int64"
		case "uint32_t":
			t = "uint32"
		case "uint64_t":
			t = "uint64"
		case "size_t":
			t = "uint"
		case "long":
			t = "int32"
		case "unsigned":
			t = "uint32"
		case "unsigned int":
			t = "uint"
		case "float":
			t = "float32"
		case "double":
			t = "float64"
		// Kind of a hack.
		case "filesystem::path":
			t = "string"
		}
	}
	return prefix + t
}

//

var (
	// This one cannot start a the line, as it has to look back that the previous
	// character isn't a ".".
	reMember1 = regexp.MustCompile(`([^a-z\._])([a-z_]+_)\b`)
	// This one only triggers at the start of the line for assignments.
	reMember2 = regexp.MustCompile(`^([a-z_]+_)\b`)
)

// fixMemberAccess replaces all "foo_" inside a method to "p.foo_".
func fixMemberAccess(lines []Line, receiver, structName, funcName string) []Line {
	if receiver == "" {
		return lines
	}
	var out []Line
	for _, l := range lines {
		l.code = reMember1.ReplaceAllString(l.code, "${1}"+receiver+".${2}")
		l.code = reMember2.ReplaceAllString(l.code, receiver+".${1}")
		out = append(out, l)
	}
	return out
}

//

// fixInsideStructs calls within with the block inside a struct.
func fixInsideStructs(lines []Line, within func(lines []Line, structName string) []Line) []Line {
	var out []Line
	for i := 0; i < len(lines); i++ {
		l := lines[i]
		if m := reGoStruct.FindStringSubmatch(l.code); m != nil {
			// Find the end, and process this part only.
			//findClosingBracket()
			b := countBrackets(l.code)
			end := i + 1
			for ; b > 0 && end < len(lines); end++ {
				b += countBrackets(lines[end].code)
			}
			end--
			// Append the type declaration.
			out = append(out, l)
			if i+1 < end {
				out = append(out, within(lines[i+1:end], m[1])...)
				// And the trailing line, if applicable.
				out = append(out, lines[end])
				i = end
			} else if i+1 == end {
				// And the trailing line, if applicable.
				if !strings.HasSuffix(lines[end].code, "}") && !strings.HasSuffix(lines[end].code, "};") {
					panic(lines[end].String())
				}
				out = append(out, lines[end])
				i = end
			} else {
				panic(end)
			}
		} else {
			out = append(out, l)
		}
	}
	return out
}

//

// fixMembers fix struct members. Because it also runs on functions inside
// structs, it has to be run after fixVariableDeclaractions.
func fixMembers(lines []Line, structName string) []Line {
	var out []Line
	for _, l := range lines {
		if m := reVariable.FindStringSubmatch(l.code); m != nil && m[1] != "return" && !strings.HasPrefix(m[1], "typedef ") {
			l.code = m[2] + " " + reduceComplexType(m[1])
		}
		out = append(out, l)
	}
	return out
}

//

var (
	reEnumDefinition = regexp.MustCompile(`^enum (` + symbolSimple + `)\s*(.*)$`)
	reEnumItem       = regexp.MustCompile(`^(` + symbolSimple + `),?$`)
)

// processEnumDefinition rewrites the enums.
func processEnumDefinition(lines []Line) []Line {
	var out []Line
	for i := 0; i < len(lines); i++ {
		if m := reEnumDefinition.FindStringSubmatch(lines[i].code); m != nil {
			// There's 3 styles:
			// - one liner
			// - normal
			// - normal but with the opening bracket on a separate line
			var items []Line
			orig := lines[i]
			if strings.Contains(m[2], "}") {
				// One liner.
				s := strings.TrimSpace(m[2])
				if s[0] != '{' || !strings.HasSuffix(s, "};") {
					panic(s)
				}
				s = s[1 : len(s)-2]
				for _, item := range strings.Split(strings.TrimSpace(s), ",") {
					items = append(items, Line{code: item, indent: "\t"})
				}
			} else {
				if !strings.Contains(m[2], "{") {
					// Move the bracket up.
					if lines[i+1].code != "{" {
						panic(lines[i+1])
					}
					i++
				}
				i++
				for ; reEnumItem.MatchString(lines[i].code) || (lines[i].code == "" && lines[i].comment != ""); i++ {
					items = append(items, lines[i])
				}
				if lines[i].code != "};" {
					panic(lines[i].String())
				}
			}
			// Generate.
			orig.code = "type " + m[1] + " int"
			out = append(out, orig)
			out = append(out, Line{code: "const ("})
			first := false
			for _, item := range items {
				// Cleanup then add.
				item.code = strings.TrimRight(strings.TrimSpace(item.code), ",")
				if !first && item.code != "" {
					item.code += " " + m[1] + " = iota"
					first = true
				}
				out = append(out, item)
			}
			out = append(out, Line{code: ")"})
		} else {
			out = append(out, lines[i])
		}
	}
	return out
}

//

var reTypedef = regexp.MustCompile(`^typedef (` + complexType + `)\s+(` + symbolSimple + `);$`)

// processTypedef rewrites typedef.
func processTypedef(lines []Line) []Line {
	var out []Line
	for _, l := range lines {
		if l.indent == "" {
			if m := reTypedef.FindStringSubmatch(l.code); m != nil {
				l.code = "type " + m[2] + " " + reduceComplexType(m[1])
			}
		}
		out = append(out, l)
	}
	return out
}

// End of fixups.

// load loads a single .h/.cc and processes it to make it closer to Go.
//
// The resulting file is not syntactically valid Go but it will make manual fix
// ups easier.
func load(raw []byte, keepSkip bool, doc map[string][]Line) (string, string) {
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

	// Starts with zapping out C++ macros, extern and namespaces.
	// - #define and #include
	// - extern "C" {
	// - namespace foo {
	lines = commentDefines(lines)
	lines = commentExtern(lines)
	lines = commentNamespace(lines)
	lines = mergeStrings(lines)

	// Do in order:
	// - type Foo struct {
	// - enum Foo {
	// - () on one line.
	// - Save comments on func/method declaration.
	// - Rewrite function implementation.
	// - Fix conditions, loops
	// - Fix assignments
	// - Convert ++i to i++
	// - Fix .clear()
	// - Fix assert()
	// - Fix variable declaration
	// - Fix member declaration
	lines = processStructDefinition(lines)
	lines = mergeParenthesis(lines)
	lines = processFunctionDeclaration(lines, doc)
	lines = processFunctionImplementation(lines, doc)
	//log.Printf("snapshot:\n%s", joinLines("x ", lines))
	lines = extractEmbedded(lines)
	lines = fixInsideFuncs(lines, fixIf)
	lines = fixInsideFuncs(lines, fixFor)
	lines = fixInsideFuncs(lines, fixWhile)
	lines = fixInsideFuncs(lines, fixAssignments)
	lines = fixInsideFuncs(lines, fixPreIncrement)
	lines = fixInsideFuncs(lines, fixClear)
	lines = fixInsideFuncs(lines, fixAsserts)
	lines = fixInsideFuncs(lines, fixVariableDeclaractions)
	lines = fixInsideFuncs(lines, fixMemberAccess)
	lines = fixInsideStructs(lines, fixMembers)
	lines = processEnumDefinition(lines)
	lines = processTypedef(lines)

	// At the very end, remove the trailing ";" and skip consecutive empty lines.
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

func loadFile(name string, keepSkip bool, doc map[string][]Line) (string, string) {
	log.Printf("loadFile(%s)", name)
	// #nosec G304
	raw, err := os.ReadFile(name)
	if err != nil {
		return "", ""
	}
	return load(raw, keepSkip, doc)
}

// process processes a pair of .h/.cc files.
func process(pkg, outDir, inDir, root string, keepSkip bool) error {
	f := filepath.Join(outDir, root+".go")
	// #nosec G304
	if content, _ := os.ReadFile(f); len(content) != 0 {
		if !bytes.Contains(content, []byte("//go:build nobuild")) {
			// It is legitimate, do not overwrite.
			log.Printf("skipping %s", root+".go")
			return nil
		}
	}
	doc := map[string][]Line{}
	hdr1, c1 := loadFile(filepath.Join(inDir, root+".h"), keepSkip, doc)
	hdr2, c2 := loadFile(filepath.Join(inDir, root+".cc"), keepSkip, doc)
	out := hdr1
	if hdr1 != hdr2 {
		out += hdr2
	}
	out += "\n//go:build nobuild\n\npackage " + pkg + "\n\n" + c1 + c2
	// #nosec G306
	return os.WriteFile(f, []byte(out), 0o644)
}

// glueContent adds glue code to make transition a tad easier.
const glueContent = `
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
	if !b {
		panic(b)
	}
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
	inDir := flag.String("i", ".", "input directory")
	outDir := flag.String("o", ".", "output directory")
	keepSkip := flag.Bool("s", false, "keep skipped lines")
	pkg := flag.String("p", "", "package name to use")
	flag.Parse()
	log.SetFlags(0)
	if !*v {
		log.SetOutput(io.Discard)
	}

	if !filepath.IsAbs(*inDir) {
		var err error
		*inDir, err = filepath.Abs(*inDir)
		if err != nil {
			return err
		}
	}
	if *pkg == "" {
		*pkg = filepath.Base(*inDir)
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
			// #nosec G301
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
		if err := process(*pkg, *outDir, *inDir, root, *keepSkip); err != nil {
			return err
		}
	}

	// Inject a file with helpers.
	// #nosec G306
	if err := os.WriteFile(filepath.Join(*outDir, "cc2go.go"), []byte("package "+*pkg+"\n"+glueContent), 0o644); err != nil {
		return err
	}

	// Insert a go.mod if missing.
	if _, err := os.Stat(filepath.Join(*outDir, "go.mod")); os.IsNotExist(err) {
		// #nosec G204
		cmd := exec.Command("go", "mod", "init", *pkg)
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
