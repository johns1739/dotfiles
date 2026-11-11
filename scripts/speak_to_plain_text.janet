#!/usr/bin/env janet

(def top-level-domains
  (tabseq [domain :in ["ai" "app" "biz" "co" "com" "dev" "edu"
                       "gov" "info" "io" "me" "net" "org"]]
    domain true))

(defn join-words
  [words]
  (string/join words " "))

(defn non-empty
  [text]
  (unless (empty? text)
    text))

(defn spoken-filename
  [opener directories basename extension closers]
  (unless (and (empty? directories)
               (top-level-domains (string/ascii-lower extension)))
    (string opener
            (join-words (filter non-empty ["file" basename (string/ascii-upper extension)]))
            closers)))

(defn spoken-segment
  [segment]
  (if (= "id" (string/ascii-lower segment))
    "ID"
    segment))

(defn backticked
  [words]
  (string "`" (join-words words) "`"))

(def plain-text-grammar
  (peg/compile
    ~{:line-content (any (if-not (* (any "\r") (+ "\n" -1)) 1))
      :line-end (* (any "\r") (+ "\n" -1))
      :line (* (not -1) :line-content :line-end)

      :fence-line (sub :line-content (* (any :s) (+ "```" "~~~")))
      :fence-block (* :fence-line :line-end
                      (any (* (not :fence-line) :line))
                      (? (* :fence-line :line-end))
                      (constant "Code Block"))

      :table-row (* (sub :line-content (* (any :s) "|" (to (* "|" (any :s) -1))))
                    :line-end)
      :table-block (* (some :table-row) (constant "Table Block"))

      :table-separator (* (any :s)
                          (? "|")
                          (some (* (any :s) (? ":") (some "-") (? ":") (any :s) "|"))
                          (any :s) (? ":") (any "-") (? ":") (any :s)
                          -1)
      :horizontal-rule (* (any :s) (at-least 3 (* (set "-*_") (any :s))) -1)
      :skipped-line (* (not :table-row)
                       (sub :line-content (+ :table-separator :horizontal-rule))
                       :line-end)

      :blockquote (some (* (any :s) ">" (any :s)))
      :heading (* (any :s) (between 1 6 "#") (some :s))
      :bullet (* (any :s) (set "-*+") (some :s))
      :leading-aside (* :parens (any :s))
      :rendered-line (% (sub :line-content
                             (* (? :blockquote) (? :heading) (? :bullet) (? :leading-aside)
                                :inline)))
      :text-line (* (not -1) (not :table-row) (not :fence-line) (not :skipped-line)
                    :rendered-line
                    :line-end)
      :spoken-line (cmt :text-line ,non-empty)
      :silent-line (drop (cmt :text-line ,|(empty? $)))

      :spoken-block (+ :table-block :fence-block :spoken-line)
      :blank-run (* (any :skipped-line) :silent-line (any (+ :skipped-line :silent-line)))
      :pause (? (* (if (drop (* :blank-run :spoken-block)) 0)
                   (constant " [pause:1s]")))
      :leading-pause (? (* (if (drop (* (any :skipped-line) :silent-line :blank-run :spoken-block)) 0)
                           (constant " [pause:1s]")))
      :output-line (+ :skipped-line
                      (* :spoken-block :pause (constant "\n"))
                      (* :silent-line (constant "\n")))

      :word-character (+ :w "_")
      :space-word (some (if-not " " 1))
      :link (* (? "!") "[" (sub (any (if-not "]" 1)) :inline) "]"
               "(" (any (if-not ")" 1)) ")")
      :code (* "`" (sub (any (if-not "`" 1)) :spoken-code) "`")
      :bold-star (* "**" (sub (some (if-not "**" 1)) :inline) "**")
      :bold-underscore (* "__" (sub (some (if-not "__" 1)) :inline) "__")
      :parens (* "(" (any (+ :parens (if-not ")" 1))) ")")
      :aside (* (any :s) :parens)
      :nested (+ :link :aside :code :bold-star :bold-underscore)
      :italic-star-close (* (not (look -1 :s)) "*" (not (+ :word-character "*")))
      :italic-star (* (not (look -1 (+ :word-character "*")))
                      "*"
                      (not :s)
                      (sub (drop (some (if-not :italic-star-close (+ :nested 1)))) :inline)
                      :italic-star-close)
      :italic-underscore-close (* (not (look -1 :s)) "_" (not :word-character))
      :italic-underscore (* (not (look -1 :word-character))
                            "_"
                            (not :s)
                            (sub (drop (some (if-not :italic-underscore-close (+ :nested 1)))) :inline)
                            :italic-underscore-close)
      :path-word (* (not (look -1 (if-not (+ :s (set "*_`[]()\"'")) 1)))
                    (sub :space-word
                         (* (if (* (any :opener) (not (to (set "`[(")))) 0)
                            :path-transform)))
      :token (+ :nested :italic-star :italic-underscore :path-word (<- 1))
      :inline (any :token)

      :opener (set "(\"'[")
      :closer (set "\"'.,;:!?)]")
      :segment (some (if-not (+ "/" :s) 1))
      :directory (* :segment "/")
      :label (some (+ :w "-"))
      :host (* :label (any (* "." :label)))
      :url (* (<- (any :opener))
              (some (+ :w (set "+.-")))
              "://"
              (? "www.")
              (constant "URL ")
              (<- :host)
              (any (if-not (* (any :closer) -1) 1))
              (<- (any :closer))
              -1)
      :line-number (* ":" (some :d))
      :short-extension (* (<- :a) (any :line-number) (constant "") -1)
      :long-extension (* (<- (* :a (some :w))) (any :line-number) (<- (any :closer)) -1)
      :file-ending (* "." (+ :short-extension :long-extension))
      :basename-separator (set "_-")
      :basename-word (some (if-not :basename-separator 1))
      :large-number (* (any "0") (at-least 5 :d) (not :basename-word))
      :spoken-basename (/ (group (any (+ :large-number (<- :basename-word) :basename-separator)))
                          ,join-words)
      :filename (* (sub (some (if-not (+ "/" :s :file-ending) 1)) :spoken-basename)
                   :file-ending)
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
      :path-transform (+ :url :file :rooted-path :nested-path :file-path)

      :upper (range "AZ")
      :lower (range "az")
      :identifier-segment (/ (<- (+ (* :upper (some :lower))
                                    (some (* :upper (not :lower)))
                                    (some :lower)
                                    (some :d)))
                             ,spoken-segment)
      :identifier (/ (group (* (any (+ :identifier-segment "_")) -1)) ,join-words)
      :folder (if (to "/")
                (* (constant "FOLDER ")
                   (thru (* (<- (some (if-not "/" 1))) (any "/") -1))))
      :code-word (+ :url
                    (if (to "://") (<- (any 1)))
                    :file
                    :folder
                    :identifier
                    (<- (any 1)))
      :plain-code (any (+ (<- " ") (sub :space-word :code-word)))

      :symbol-character (set "()[]{}:&=<>$#@;\\|^%*+!?")
      :symbol-word (* (not :path-transform)
                      (+ (to :symbol-character)
                         (* (to :a) (to :d))
                         (* (to :d) (to :a))))
      :has-symbol-word (* (any (* (any " ") (not (sub :space-word :symbol-word)) :space-word))
                          (any " ")
                          (sub :space-word :symbol-word))
      :equals (+ "===" "==")
      :symbol-delimiter (+ :equals (set "()[]{}:&; "))
      :symbol-piece (+ (if (to :d) (<- (any 1)))
                       :identifier
                       (<- (any 1)))
      :symbol-chunk (+ (if (to "@") (<- (any 1)))
                       (any (+ (some ".") (sub (some (if-not "." 1)) :symbol-piece))))
      :symbol-code (/ (group (any (+ (* :equals (constant "EQUALS"))
                                     (set "()[]{}:&; ")
                                     (sub (some (if-not :symbol-delimiter 1)) :symbol-chunk))))
                      ,backticked)
      :spoken-code (+ (if (drop :has-symbol-word) :symbol-code) :plain-code)

      :main (% (* :leading-pause (any :output-line)))}))

(defn plain-text
  [text]
  (first (peg/match plain-text-grammar text)))

(defn input-text
  [arguments]
  (if (empty? arguments)
    (file/read stdin :all)
    (string/join arguments " ")))

(defn main
  [_ & arguments]
  (prin (plain-text (input-text arguments))))
