;; -*- lexical-binding: t; -*-
((nil . ((eglot-workspace-configuration
          . ( :pylsp ( :plugins ( :pycodestyle (:enabled :json-false)
                                  :mccabe (:enabled :json-false)
                                  :pyflakes (:enabled :json-false)
                                  :flake8 (:enabled t))
                       :configurationSources ["flake8"]))))))
