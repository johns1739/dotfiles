#!/usr/bin/env janet

(import ./lib/gh-report)

(defn main
  [_script]
  (gh-report/collect-and-table-print-prs)
  (print)
  (gh-report/collect-and-table-print-issues))
