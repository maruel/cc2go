// Copyright 2021 Marc-Antoine Ruel. All rights reserved.
// Use of this source code is governed under the Apache License, Version 2.0
// that can be found in the LICENSE file.

package main

import (
	"log"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

var (
	reForwardStruct  = regexp.MustCompile(`^struct [A-Za-z]+;$`)
	reForwardFunc    = regexp.MustCompile(`^(\s*)([a-zA-Z]+)\s+[A-Za-z_]+\s?\([a-zA-Z *,=<>_:&\[\]]*\)(?: const |);$`)
	reTrippleComment = regexp.MustCompile(`^(\s*)///(.*)`)
	reDoubleComment  = regexp.MustCompile(`^(\s*)//(.*)`)
	reStructAccess   = regexp.MustCompile(`^(\s*)(public|protected|private):$`)
	reExtern         = regexp.MustCompile(`^(\s*)extern (.*)`)
	reConstMethod    = regexp.MustCompile(`^(.+)\) const {$`)
	reStd            = regexp.MustCompile(`std::`)
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

// processLine processes the low hanging fruits first.
func processLine(l string) string {
	// Just ignore.
	if strings.HasPrefix(l, "#") {
		return "// " + l
	}
	if strings.HasPrefix(l, "using namespace") {
		return "// " + l
	}
	if reForwardStruct.MatchString(l) {
		return "// " + l
	}
	if reExtern.MatchString(l) {
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
	l = reStd.ReplaceAllString(l, "")
	if reConstMethod.MatchString(l) {
		return reConstMethod.ReplaceAllString(l, "$1) {")
	}
	return l
}

// mergeParenthesis makes all () on one line to make function declaration
// easier to parse.
func mergeParenthesis(lines []string) string {
	count := 0
	out := ""
	for _, l := range lines {
		if count > 0 {
			// Merging.
			l = " " + l[countSpaces(l):]
		}
		out += l
		if strings.HasPrefix(l[countSpaces(l):], "//") {
			// Ignore comments within parenthesis.
			out += "\n"
			continue
		}
		count += strings.Count(l, "(")
		count -= strings.Count(l, ")")
		if count < 0 {
			panic(count)
		}
		if count == 0 {
			out += "\n"
		}
	}
	return out
}

func handleForwardFunc(l string) string {
	if m := reForwardFunc.FindStringSubmatch(l); m != nil {
		if m[2] == "return" {
			return l
		}
		// White spaces in front.
		c := len(m[1])
		return l[:c] + "//" + l[c:]
	}
	return l
}

// processFunctionDeclaration rewrite a function declaration to be closer to Go
// style.
func processFunctionDeclaration(l string) string {
	// Convert method
	// Remove const
	// Rewrite const string& to string
	// Process argument type
	return ""
}

func load(name string) string {
	raw, err := os.ReadFile(name)
	if err != nil {
		return ""
	}

	// Do a first pass to trim obvious stuff.
	lines := strings.Split(string(raw), "\n")
	for i, l := range lines {
		lines[i] = processLine(l)
	}

	// Make a second context aware pass.
	lines = strings.Split(mergeParenthesis(lines), "\n")
	for i, l := range lines {
		if reDoubleComment.MatchString(l) {
			continue
		}
		lines[i] = handleForwardFunc(l)
		//out = processFunctionDeclaration(out)
		//fixOnelinerCondition(out)
		//fixPointerReference(out)
		//addThisPointer(out)
		//Remove ".c_str()"
		//Convert ".clear()" to " = nil"
		//Convert ".empty()" to " == nil"
		//Comment assert()
		//Convert "foo bar = baz()" to "bar = baz()"
		//Convert "set<foo>" to "map[foo]struct{}"
		//Convert "vector<foo>" to "[]foo"
		//Convert enum
	}
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

func process(outDir, root string) error {
	out := "package ginga\n\n"
	out += load(root + ".h")
	out += load(root + ".cc")
	return os.WriteFile(filepath.Join(outDir, root+".go"), []byte(out), 0o644)
}

func main() {
	outDir := "go2"
	entries, err := os.ReadDir(".")
	if err != nil {
		log.Fatal(err)
	}
	roots := map[string]struct{}{}
	for _, e := range entries {
		n := e.Name()
		ext := filepath.Ext(n)
		n = n[:len(n)-len(ext)]
		if strings.HasSuffix(n, "_test") || strings.HasSuffix(n, "_perftest") {
			continue
		}
		roots[n] = struct{}{}
	}
	if err := os.Mkdir(outDir, 0o755); err != nil {
		log.Fatal(err)
	}
	files := make([]string, 0, len(roots))
	for root := range roots {
		files = append(files, root)
	}
	sort.Strings(files)
	for _, root := range files {
		if err := process(outDir, root); err != nil {
			log.Fatal(err)
		}
	}
}
