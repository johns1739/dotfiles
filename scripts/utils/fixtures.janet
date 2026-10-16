(use sh)

(def root (string (os/realpath "/tmp") "/judge-scripts"))

(defn git*
  "Run git in repo-path with a fixed identity and no signing."
  [repo-path & arguments]
  ($ git -C ,repo-path -c user.name=test -c user.email=test@example.com -c commit.gpgsign=false ,;arguments
     > [stdout :null] > [stderr :null]))

(defn commit
  "Create an empty commit."
  [repo-path message]
  (git* repo-path "commit" "-q" "--allow-empty" "-m" message))

(defn git-repo
  "Fresh clone at root/name/clone, tracking a bare origin with one commit on main."
  [name]
  (def directory (string root "/" name))
  (def clone (string directory "/clone"))
  (def origin (string directory "/origin.git"))
  ($ rm -rf ,directory)
  ($ mkdir -p ,clone)
  ($ git init -q --bare -b main ,origin)
  (git* clone "init" "-q" "-b" "main")
  (commit clone "init")
  (git* clone "remote" "add" "origin" origin)
  (git* clone "push" "-q" "-u" "origin" "main")
  (git* clone "remote" "set-head" "origin" "main")
  clone)
