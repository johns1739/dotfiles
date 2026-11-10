(use judge)
(import ./speak_parser :as parser)

(test (parser/plain-text "# Heading\n## Sub heading\n") "Heading\nSub heading\n")

(test (parser/plain-text "- first\n* second\n+ third\n") "first\nsecond\nthird\n")

(test (parser/plain-text "before\n```fish\necho hi\n```\n~~~\nskipped\n~~~\nafter\n")
  "before\nafter\n")

(test (parser/plain-text "| Name | Age |\n|:-----|----:|\n| Ann  | 30  |\n")
  "Name, Age\nAnn, 30\n")

(test (parser/plain-text "above\n---\n* * *\n___\nbelow\n") "above\nbelow\n")

(test (parser/plain-text "See [docs](https://example.com) and ![logo](logo.png)\n")
  "See docs and logo\n")

(test (parser/plain-text "Run `make test` now\n") "Run make test now\n")

(test (parser/plain-text "**bold** __strong__ *italic* _emphasis_\n")
  "bold strong italic emphasis\n")

(test (parser/plain-text "**bold with *italic* inside**\n") "bold with italic inside\n")

(test (parser/plain-text "*outer **inner** outer*\n") "outer inner outer\n")

(test (parser/plain-text "snake_case_name and 2 * 3 * 4 and a*b*c\n")
  "snake_case_name and 2 * 3 * 4 and a*b*c\n")

(test (parser/plain-text "Edit ~/a/b.txt ./c/d /e/f g/h/i j/k.md and/or\n")
  "Edit file b TXT d f i file k MD and/or\n")

(test (parser/plain-text "Open (~/a/b) or https://example.com/a/b.html\n")
  "Open (b) or https://example.com/a/b.html\n")

(test (parser/plain-text "- See `scripts/parser.janet`\n") "See file parser JANET\n")

(test (parser/plain-text "two  spaces\n\nblank above\n") "two  spaces\n\nblank above\n")

(test (parser/plain-text "abc.txt") "file abc TXT\n")
(test (parser/plain-text "a/b/c/abc.txt") "file abc TXT\n")
(test (parser/plain-text "`a/b/c/abc.txt`") "file abc TXT\n")
(test (parser/plain-text "`a/b/c/abc.txt:123`") "file abc TXT\n")
(test (parser/plain-text "`a/b/c/abc.txt:123:4`") "file abc TXT\n")
