#!/usr/bin/env janet

(def fence-grammar
  (peg/compile
    ~(* (any :s) (+ "```" "~~~"))))

(def table-row-grammar
  (peg/compile
    ~(* (any :s) "|" (to (* "|" (any :s) -1)))))

(def skipped-line-grammar
  (peg/compile
    ~{:table-separator (* (any :s)
                          (? "|")
                          (some (* (any :s) (? ":") (some "-") (? ":") (any :s) "|"))
                          (any :s) (? ":") (any "-") (? ":") (any :s)
                          -1)
      :horizontal-rule (* (any :s) (at-least 3 (* (set "-*_") (any :s))) -1)
      :main (+ :table-separator :horizontal-rule)}))

(def block-grammar
  (peg/compile
    ~{:blockquote (some (* (any :s) ">" (any :s)))
      :heading (* (any :s) (between 1 6 "#") (some :s))
      :bullet (* (any :s) (set "-*+") (some :s))
      :main (* (? :blockquote)
               (? :heading)
               (? :bullet)
               (<- (any 1)))}))

(def top-level-domains
  {"ai" true "app" true "biz" true "co" true "com" true "dev" true "edu" true
   "gov" true "info" true "io" true "me" true "net" true "org" true})

(defn spoken-filename
  [opener directories basename extension closers]
  (unless (and (empty? directories)
               (top-level-domains (string/ascii-lower extension)))
    (string opener "file " basename " " (string/ascii-upper extension) closers)))

(def path-grammar
  (peg/compile
    ~{:opener (set "(\"'[")
      :segment (some (if-not (+ "/" :s) 1))
      :directory (* :segment "/")
      :closer (set "\"'.,;:!?)]")
      :label (some (+ :w "-"))
      :host (* :label (any (* "." :label)))
      :url (/ (* (<- (any :opener))
                 (some (+ :w (set "+.-")))
                 "://"
                 (? "www.")
                 (<- :host)
                 (any (if-not (* (any :closer) -1) 1))
                 (<- (any :closer))
                 -1)
              ,(fn [opener host closers] (string opener "URL " host closers)))
      :unparsed-url (if (to "://") (<- (any 1)))
      :line-number (* ":" (some :d))
      :short-extension (* (<- :a) (any :line-number) (constant "") -1)
      :long-extension (* (<- (* :a (some :w))) (any :line-number) (<- (any :closer)) -1)
      :file-ending (* "." (+ :short-extension :long-extension))
      :filename (* (<- (some (if-not (+ "/" :s :file-ending) 1))) :file-ending)
      :file (cmt (* (<- (any :opener)) (<- (* (? "/") (any :directory))) :filename)
                 ,spoken-filename)
      :rooted-path (* (<- (any :opener))
                      (? (+ "~" ".." "."))
                      "/"
                      (some :directory)
                      (<- :segment)
                      -1)
      :nested-path (* (<- (any :opener)) (at-least 2 :directory) (<- :segment) -1)
      :file-path (* (<- (any :opener)) :directory (<- (if (to ".") :segment)) -1)
      :main (+ :url
               :unparsed-url
               :file
               (% (+ :rooted-path :nested-path :file-path))
               (<- (any 1)))}))

(defn path-basename
  [word]
  (first (peg/match path-grammar word)))

(def reference-word-grammar
  (peg/compile
    ~(+ (to (set "()[]{}:&=<>$#@;\\|^%*+!?"))
        (* (to :a) (to :d))
        (* (to :d) (to :a)))))

(defn reference-word?
  [word]
  (and (= word (path-basename word))
       (peg/match reference-word-grammar word)))

(defn inline-reference
  [code]
  (when (some reference-word? (string/split " " code))
    "Inline Reference"))

(def inline-grammar
  (peg/compile
    ~{:word (+ :w "_")
      :link (* (? "!") "[" (sub (any (if-not "]" 1)) :inline) "]"
               "(" (any (if-not ")" 1)) ")")
      :code-content (any (if-not "`" 1))
      :code (* "`"
               (+ (cmt (<- :code-content) ,inline-reference)
                  (sub :code-content :inline))
               "`")
      :bold-star (* "**" (sub (some (if-not "**" 1)) :inline) "**")
      :bold-underscore (* "__" (sub (some (if-not "__" 1)) :inline) "__")
      :parens (* "(" (any (+ :parens (if-not ")" 1))) ")")
      :aside (+ (* (not (look -1 1)) :parens (any :s))
                (* (any :s) :parens))
      :nested (+ :link :aside :code :bold-star :bold-underscore)
      :italic-star-close (* (not (look -1 :s)) "*" (not (+ :word "*")))
      :italic-star (* (not (look -1 (+ :word "*")))
                      "*"
                      (not :s)
                      (sub (drop (some (if-not :italic-star-close (+ :nested 1)))) :inline)
                      :italic-star-close)
      :italic-underscore-close (* (not (look -1 :s)) "_" (not :word))
      :italic-underscore (* (not (look -1 :word))
                            "_"
                            (not :s)
                            (sub (drop (some (if-not :italic-underscore-close (+ :nested 1)))) :inline)
                            :italic-underscore-close)
      :token (+ :nested :italic-star :italic-underscore (<- 1))
      :inline (any :token)
      :main (% :inline)}))

(defn plain-line
  [line]
  (->> line
       (peg/match block-grammar)
       (first)
       (peg/match inline-grammar)
       (first)
       (string/split " ")
       (map path-basename)
       (|(string/join $ " "))))

(defn input-lines
  [text]
  (def lines (string/split "\n" text))
  (def lines-without-carriage-returns (map |(string/trimr $ "\r") lines))
  (if (= "" (last lines-without-carriage-returns))
    (array/slice lines-without-carriage-returns 0 -2)
    lines-without-carriage-returns))

(def pause-grammar
  (peg/compile
    ~{:trailing-newlines (<- (* (some "\n") -1))
      :paragraph-break (/ (<- (at-least 2 "\n")) ,|(string " [pause:1s]" $))
      :line-break (/ (<- "\n") " [pause:0.5s]\n")
      :main (% (any (+ :trailing-newlines :paragraph-break :line-break (<- 1))))}))

(defn plain-text
  [text]
  (var in-fence false)
  (var in-table false)
  (def output @[])
  (each line (input-lines text)
    (def table-row (and (not in-fence) (peg/match table-row-grammar line)))
    (when (and table-row (not in-table))
      (array/push output "Table Block\n"))
    (set in-table table-row)
    (cond
      table-row nil
      (peg/match fence-grammar line) (do
                                       (unless in-fence
                                         (array/push output "Code Block\n"))
                                       (set in-fence (not in-fence)))
      in-fence nil
      (peg/match skipped-line-grammar line) nil
      (array/push output (string (plain-line line) "\n"))))
  (first (peg/match pause-grammar (string/join output))))

(defn main
  [&]
  (prin (plain-text (file/read stdin :all))))
