(use judge)
(import ./speak_to_plain_text :as parser)

(test (parser/plain-text "# Heading\n## Sub heading\n") "Heading [pause:0.5s]\nSub heading\n")

(test (parser/plain-text "- first\n* second\n+ third\n") "first [pause:0.5s]\nsecond [pause:0.5s]\nthird\n")

(test (parser/plain-text "before\n```fish\necho hi\n```\n~~~\nskipped\n~~~\nafter\n")
  "before [pause:0.5s]\nCode Block [pause:0.5s]\nCode Block [pause:0.5s]\nafter\n")

(test (parser/plain-text "| Name | Age |\n|:-----|----:|\n| Ann  | 30  |\n")
  "Table Block\n")

(test (parser/plain-text "above\n---\n* * *\n___\nbelow\n") "above [pause:0.5s]\nbelow\n")

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
  "Open or URL example.com\n")

(test (parser/plain-text "- See `scripts/parser.janet`\n") "See file parser JANET\n")

(test (parser/plain-text "two  spaces\n\nblank above\n") "two  spaces [pause:1s]\n\nblank above\n")

(test (parser/plain-text "abc.txt") "file abc TXT\n")
(test (parser/plain-text "a/b/c/abc.txt") "file abc TXT\n")
(test (parser/plain-text "`a/b/c/abc.txt`") "file abc TXT\n")
(test (parser/plain-text "`a/b/c/abc.txt:123`") "file abc TXT\n")
(test (parser/plain-text "`a/b/c/abc.txt:123:4`") "file abc TXT\n")

(test (parser/plain-text "") "")
(test (parser/plain-text "  - nested bullet\n") "nested bullet\n")
(test (parser/plain-text "before\n```\nunclosed fence\n") "before [pause:0.5s]\nCode Block\n")
(test (parser/plain-text "Read [the guide](docs/guide.md)\n") "Read the guide\n")
(test (parser/plain-text "e.g. i.e. 2.5 v1.2 and/or\n") "e.g. i.e. 2.5 v1.2 and/or\n")
(test (parser/plain-text "https://example.com/a/b.txt\n") "URL example.com\n")
(test (parser/plain-text "~/.config/fish/config.fish\n") "file config FISH\n")
(test (parser/plain-text "a.b.txt abc.tar.gz\n") "file a.b TXT file abc.tar GZ\n")
(test (parser/plain-text "README.MD snake_case.rb:10\n") "file README MD file snake_case RB\n")
(test (parser/plain-text "abc.txt:12:3:\n") "file abc TXT:\n")
(test (parser/plain-text "see foo.txt.\n") "see file foo TXT.\n")
(test (parser/plain-text "foo.txt, bar.md\n") "file foo TXT, file bar MD\n")
(test (parser/plain-text "\"a/b.md\"\n") "\"file b MD\"\n")
(test (parser/plain-text "> quoted line\n") "quoted line\n")
(test (parser/plain-text "see (a/b/abc.txt) now\n") "see now\n")
(test (parser/plain-text "run (see a/b.txt and c.md) now\n") "run now\n")
(test (parser/plain-text "(aside) start\n") "start\n")
(test (parser/plain-text "end (aside)\n") "end\n")
(test (parser/plain-text "outer (a (nested) b) done\n") "outer done\n")
(test (parser/plain-text "**bold (aside)** text\n") "bold text\n")
(test (parser/plain-text "unclosed (paren here\n") "unclosed (paren here\n")
(test (parser/plain-text "See [docs](https://example.com) now\n") "See docs now\n")
(test (parser/plain-text "  ```janet\n(print 1)\n  ```\n") "Code Block\n")
(test (parser/plain-text "```\n```\n") "Code Block\n")
(test (parser/plain-text "in foo.txt: and 'a/b.md'!\n") "in file foo TXT: and 'file b MD'!\n")
(test (parser/plain-text "main.c main.c:10\n") "file main C file main C\n")
(test (parser/plain-text "example.com, google.ai and www.example.org.\n") "example.com, google.ai and www.example.org.\n")
(test (parser/plain-text "a/b.io\n") "file b IO\n")
(test (parser/plain-text "> > - nested quote\n") "nested quote\n")
(test (parser/plain-text "abc\r\ndef.txt\r\n") "abc [pause:0.5s]\nfile def TXT\n")
(test (parser/plain-text "|A|B|C|\n") "Table Block\n")
(test (parser/plain-text "before\n| A | B |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |\nafter\n") "before [pause:0.5s]\nTable Block [pause:0.5s]\nafter\n")
(test (parser/plain-text "| A |\n\n| B |\n") "Table Block [pause:1s]\n\nTable Block\n")
(test (parser/plain-text "  | indented | table |  \n") "Table Block\n")
(test (parser/plain-text "a | b and |pipe\n") "a | b and |pipe\n")

(test (parser/plain-text "`.select(:user_id).map(:&some)`") "Inline Reference\n")
(test (parser/plain-text "use `foo()` or `a == b` here\n") "use Inline Reference or Inline Reference here\n")
(test (parser/plain-text "password `Xk9mP2q` and `git@github.com`\n") "password Inline Reference and Inline Reference\n")
(test (parser/plain-text "run `ls -la` in `~/a/b`\n") "run ls -la in b\n")
(test (parser/plain-text "plain Xk9mP2q and a==b\n") "plain Xk9mP2q and a==b\n")

(test (parser/plain-text "open `https://example.com/a?b=1`\n") "open URL example.com\n")
(test (parser/plain-text "open `https://www.example.com/a?b=1`\n") "open URL example.com\n")
(test (parser/plain-text "see http://sub.example.org:8080/x, \"https://a.io\".\n") "see URL sub.example.org, \"URL a.io\".\n")
(test (parser/plain-text "open https://example.com/a?b=1\n") "open URL example.com\n")
(test (parser/plain-text "open https://www.example.com/a?b=1\n") "open URL example.com\n")
(test (parser/plain-text "a\nb\n\nc\n") "a [pause:0.5s]\nb [pause:1s]\n\nc\n")
(test (parser/plain-text "a\n\n\n\nb") "a [pause:1s]\n\n\n\nb\n")
(test (parser/plain-text "single line\n\n\n") "single line\n\n\n")
