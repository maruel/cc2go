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

// Phase 1

var (
	// Used in phase 1

	// e.g. "struct Foo;"
	reForwardStruct = regexp.MustCompile(`^struct [A-Za-z]+;$`)
	// e.g. "protected:"
	reStructAccess = regexp.MustCompile(`^(public|protected|private):$`)
	// A string.
	reConstChar = regexp.MustCompile(`const char\s?\*`)
	// e.g. "void bar() const {"
	reConstMethod = regexp.MustCompile(`^(.+)\) const {$`)

	// e.g. "TEST_F(DepfileParserTest, Continuation) {"
	reGoogleTestfunc  = regexp.MustCompile(`^TEST_F\(([a-zA-Z]+), ([a-zA-Z]+)\) {$`)
	reGoogleTestEQ    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_EQ\(([^,]+), (.+)\);$`)
	reGoogleTestNE    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_NE\(([^,()]+), (.+)\);$`)
	reGoogleTestGT    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_GT\(([^,()]+), (.+)\);$`)
	reGoogleTestGE    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_GE\(([^,()]+), (.+)\);$`)
	reGoogleTestLT    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_LT\(([^,()]+), (.+)\);$`)
	reGoogleTestLE    = regexp.MustCompile(`^(?:ASSERT|EXPECT)_LE\(([^,()]+), (.+)\);$`)
	reGoogleTestTrue  = regexp.MustCompile(`^(?:ASSERT|EXPECT)_TRUE\((.+)\);$`)
	reGoogleTestFalse = regexp.MustCompile(`^(?:ASSERT|EXPECT)_FALSE\((.+)\);$`)

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
	// Easy to replace google test function into a proper Go function right away.
	if m := reGoogleTestfunc.FindStringSubmatch(out.code); m != nil {
		out.code = "func Test" + m[1] + "_" + m[2] + "(t *testing.T) {"
	} else if m := reGoogleTestEQ.FindStringSubmatch(out.code); m != nil {
		out.code = "if " + m[1] + " != " + m[2] + " { t.FailNow() }"
	} else if m := reGoogleTestNE.FindStringSubmatch(out.code); m != nil {
		out.code = "if " + m[1] + " == " + m[2] + " { t.FailNow() }"
	} else if m := reGoogleTestGT.FindStringSubmatch(out.code); m != nil {
		out.code = "if " + m[1] + " <= " + m[2] + " { t.FailNow() }"
	} else if m := reGoogleTestGE.FindStringSubmatch(out.code); m != nil {
		out.code = "if " + m[1] + " < " + m[2] + " { t.FailNow() }"
	} else if m := reGoogleTestLT.FindStringSubmatch(out.code); m != nil {
		out.code = "if " + m[1] + " >= " + m[2] + " { t.FailNow() }"
	} else if m := reGoogleTestLE.FindStringSubmatch(out.code); m != nil {
		out.code = "if " + m[1] + " > " + m[2] + " { t.FailNow() }"
	} else if m := reGoogleTestTrue.FindStringSubmatch(out.code); m != nil {
		out.code = "if " + m[1] + " { t.FailNow() }"
	} else if m := reGoogleTestFalse.FindStringSubmatch(out.code); m != nil {
		out.code = "if !" + m[1] + " { t.FailNow() }"
	}
	return out
}

// Rest, in order.

// commentDefines comments out #include and #define, including multi-lines
// defines.
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

var reStructDefinition = regexp.MustCompile(`^struct ([A-Za-z]+)(.+)$`)

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

//

var (
	reGoStruct               = regexp.MustCompile(`^type ([a-zA-Z]+) struct {$`)
	reConstructorDeclaration = regexp.MustCompile(`^([A-Za-z_0-9]+)\s?\([a-zA-Z *,=<>_:&\[\]]*\);$`)
	// e.g. "void bar();" or "virtual void bar() const = 0;"
	// 1 is return type, 2 is name.
	reFuncDeclaration = regexp.MustCompile(`^(?:virtual |)([a-zA-Z<>*]+)\s+([A-Za-z_0-9]+)\s?\([a-zA-Z *,=<>_:&\[\]]*\)(?: const|)\s*(?:= 0|)\s*;$`)
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
			if m[1] == "return" {
				// It's super annoying that "return Foo();" triggers but there's
				// nothing to do but explicitly ignore.
				continue
			}
			l.doSkip()
			// Associate the function description if available.
			for i := len(out) - 1; i >= 0 && out[i].code == "" && out[i].comment != ""; i-- {
				n := getID(structName, m[2])
				doc[n] = append([]Line{out[i]}, doc[n]...)
				out[i].skip = true
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

var (
	// 1 is return type, 2 is name, 3 is args, 4 is brackets
	reFuncImplementation = regexp.MustCompile(`^(?:virtual |)([a-zA-Z<>*]+)\s+([A-Za-z_0-9]+)\s?\(([a-zA-Z *,=<>_:&\[\]]*)\)(?: const|)\s*({(?:|\s*}))$`)
	// 1 is return type, 2 is class, 3 is name, 4 is args, 5 is brackets
	reMethodImplementation = regexp.MustCompile(`^(?:virtual |)([a-zA-Z<>*]+)\s+([A-Za-z_0-9]+)::([A-Za-z_0-9]+)\s?\(([a-zA-Z *,=<>_:&\[\]]*)\)(?: const|)\s*({(?:|\s*}))$`)
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
			funcName = m[2]
			args, err := processArgs(m[3])
			if err != nil {
				panic(fmt.Sprintf("%s: %s", l.code, err))
			}
			// TODO(maruel): if structName != "", then move the function outside of
			// the struct.
			l.code = rewriteFunc(structName, funcName, args, m[1], m[4])
			if d := doc[getID(structName, funcName)]; len(d) != 0 {
				// Insert the doc there, updating the indentation.
				for _, x := range d {
					x.indent = l.indent
					out = append(out, x)
				}
			}
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
			structName = m[2]
			funcName = m[3]
			args, err := processArgs(m[4])
			if err != nil {
				panic(fmt.Sprintf("%s: %s", l.code, err))
			}
			l.code = rewriteFunc(structName, funcName, args, m[1], m[5])
			if d := doc[getID(structName, funcName)]; len(d) != 0 {
				// Insert the doc there, updating the indentation.
				for _, x := range d {
					x.indent = l.indent
					out = append(out, x)
				}
			}
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
		return "func " + funcName + "(" + args + ") " + ret + " " + rest
	}
	receiver := strings.ToLower(structName[:1])
	if ret == "" {
		return "func (" + receiver + " *" + structName + ") " + funcName + "(" + args + ") " + rest
	}
	return "func (" + receiver + " *" + structName + ") " + funcName + "(" + args + ") " + ret + " " + rest
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
	t = strings.TrimPrefix(t, "const ")
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
	return n + " " + t, nil
}

//

// fixCondition does two things:
//  - Fix one liners.
//  - Remove the extra parenthesis.
func fixCondition(lines []Line) []Line {
	var out []Line
	for i := 0; i < len(lines); i++ {
		l := lines[i]
		// Look for functions. It is easy now that the functions start with
		// "func " and that they are on one line.
		// Include hack for TEST_F() googletest.
		if strings.HasPrefix(l.code, "func ") || strings.HasPrefix(l.code, "TEST_F(") {
			// Find the end, and process this part only.
			b := countBrackets(l.code)
			end := i
			for ; b > 0 && end < len(lines); end++ {
				b += countBrackets(lines[end].code)
			}
			end -= 2
			// Append the function.
			out = append(out, l)
			if i+1 < end {
				/*
					// Everything within.
					for x := i + 1; x < end; x++ {
						log.Printf("FUNC IS: %s\n", lines[x].String())
					}
				*/
				more := fixConditionFunc(lines[i+1 : end])
				/*
					for x := range more {
						log.Printf("RESULT : %s\n", more[x].String())
					}
				*/
				out = append(out, more...)
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

// fixConditionFunc handles one function.
func fixConditionFunc(lines []Line) []Line {
	//log.Printf("fixConditionFunc(%s)", lines[0].String())
	var out []Line
	for i := 0; i < len(lines); {
		// Process "one line" at a time. The line can read multiple lines if it's a
		// oneliner condition.
		j, more := fixConditionOneline(lines[i:])
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
	//log.Printf("cleanCond() = %q, %q", cond, rest)
	return cond, rest
}

// fixConditionOneline handles one line or more lines if a one liner is found.
//
// It returns the number of lines consumed and the corresponding output.
func fixConditionOneline(lines []Line) (int, []Line) {
	//log.Printf("fixConditionOneline(%s)", lines[0].String())
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
				j, more = fixConditionOneliner(l.indent, lines[1:], false)
				j++
			} else {
				// Inject rest as a statement.
				j, more = fixConditionOneliner(l.indent, append([]Line{{indent: l.indent + "\t", code: rest}}, lines[1:]...), false)
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
				j, more = fixConditionOneliner(l.indent, lines[1:], false)
				j++
			} else {
				// Inject rest as a statement.
				j, more = fixConditionOneliner(l.indent, append([]Line{{indent: l.indent + "\t", code: rest}}, lines[1:]...), false)
			}
			return j, append([]Line{l}, more...)
		}

	} else if strings.HasPrefix(l.code, "} else") {
		// One liner?
		if !strings.HasSuffix(l.code, "{") {
			l.code += " {"
			j, more := fixConditionOneliner(l.indent, lines[1:], true)
			return 1 + j, append([]Line{l}, more...)
		}
	}

	return 1, []Line{l}
}

// fixConditionOneliner fixes a one liner.
//
// One liners can be embedded into each other, which makes it a bit tricky to
// process.
func fixConditionOneliner(indent string, lines []Line, isElse bool) (int, []Line) {
	//log.Printf("fixConditionOneliner(%s)", lines[0].String())
	i, out := fixConditionOneline(lines)

	// Special case else, but only if it was a if.
	if !isElse && i < len(lines) && strings.HasPrefix(lines[i].code, "else") {
		l := lines[i]
		l.code = "} " + l.code
		j, more := fixConditionOneline(append([]Line{l}, lines[i+1:]...))
		return i + j, append(out, more...)
	}
	return i, append(out, Line{indent: indent, code: "}"})
}

//

func fixLoops(lines []Line) []Line {
	// TODO(maruel): fixLoops to fix for and convert while to for.
	return lines
}

//

// e.g. "foo bar = baz();"
var reAssignment = regexp.MustCompile(`^(\s*)[a-zA-Z<>\*:&]+ ([a-zA-Z_]+ )=( .+;)$`)

// fixStatements handles statements inside a function.
func fixStatements(lines []Line) []Line {
	// TODO(maruel): Ensure the code cannot be inside nested functions. The
	// code base we process do not use this.

	// TODO(maruel): Constructor, destructor.
	// Remove const
	// Change ++p to p++
	//addThisPointer(out)
	insideBlock := 0
	var out []Line
	for i, l := range lines {
		was := insideBlock
		insideBlock += countBrackets(l.code)
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

// End of fixups.

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

	// Starts with zapping out C++ macros, extern and namespaces.
	// - #define and #include
	// - extern "C" {
	// - namespace foo {
	lines = commentDefines(lines)
	lines = commentExtern(lines)
	lines = commentNamespace(lines)

	// Do in order:
	// - type Foo struct {
	// - () on one line.
	// - Save comments on func/method declaration.
	// - Rewrite function implementation.
	// - Fix conditions
	// - Fix loops
	// - Fix statements.
	lines = processStructDefinition(lines)
	lines = mergeParenthesis(lines)
	lines = processFunctionDeclaration(lines, doc)
	lines = processFunctionImplementation(lines, doc)
	lines = fixCondition(lines)
	lines = fixLoops(lines)
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
func process(pkg, outDir, inDir, root string, keepSkip bool) error {
	f := filepath.Join(outDir, root+".go")
	if content, _ := ioutil.ReadFile(f); len(content) != 0 {
		if !bytes.Contains(content, []byte("//go:build nobuild")) {
			// It is legitimate, do not overwrite.
			log.Printf("skipping %s", root+".go")
			return nil
		}
	}
	doc := map[string][]Line{}
	hdr1, c1 := load(filepath.Join(inDir, root+".h"), keepSkip, doc)
	hdr2, c2 := load(filepath.Join(inDir, root+".cc"), keepSkip, doc)
	out := hdr1
	if hdr1 != hdr2 {
		out += hdr2
	}
	out += "\n//go:build nobuild\n\npackage " + pkg + "\n\n" + c1 + c2
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
	pkg := flag.String("p", "ginja", "package name to use")
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
		if err := process(*pkg, *outDir, *inDir, root, *keepSkip); err != nil {
			return err
		}
	}

	// Inject a file with helpers.
	if err := ioutil.WriteFile(filepath.Join(*outDir, *pkg+".go"), []byte("package "+*pkg+"\n"+glueContent), 0o644); err != nil {
		return err
	}

	// Insert a go.mod if missing.
	if _, err := os.Stat(filepath.Join(*outDir, "go.mod")); os.IsNotExist(err) {
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
