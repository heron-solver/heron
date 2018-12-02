;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; tesl-mode.el -- TESL specifications for Emacs

(require 'generic-x)

;; (set-face-foreground 'font-lock-string-face "purple")
;; (set-face-foreground 'font-lock-comment-face "dark green")

(define-generic-mode 
    'tesl-mode                         ;; name of the mode to create
  '("//")                           ;; comments start with '!!'
  '(
    "unit-clock"
    "U-clock"
    "int-clock"
    "Z-clock"
    "int-quantity"
    "Z-quantity"
    "decimal-clock"
    "D-clock"
    "rational-clock"
    "Q-clock"
    "rational-quantity"
    "Q-quantity"
    "let"
    "int"
    "decimal"
    "rational"
    "float"
    "sporadic"
    "implies"
    "time relation"
    "tag relation"
    "trel"
    "by"
    "time"
    "delayed"
    "on"
    "filtered"
    "from"
    "sustained"
    "immediately"
    "weakly"
    "strictly"
    "to"
    "await"
    "when"
    "not"
    "every"
    "at"
    "starting"
    "periodic"
    "offset"
    "now"
    "next"
    "with"
    "reset"
    "precedes"
    "excludes"
    "kills"
    "der"
    "until"
    "->"
    "="
    "+"
    "*"
    "/"
    "()"
    "("
    ")"
    "<"
    ">"
    ","
    "@"
    "maxstep"
    "minstep"
    "policy"
    "dumpres"
    "trict"
    "scenario"
    "exit"
    "run"
    "step"
    "stutter"
    "print"
    "help"
    "tagref"
    "output"
    "driving-clock"
    "event-concretize"
    "event-solve"
    "vcd"
    "tikz"
    "tex"
    "pdf"
    "svg"
    "select"
    )                     ;; some keywords
  '(;;("=" . 'font-lock-operator)     ;; '=' is an operator
    ;;("/" . 'font-lock-builtin))     ;; ';' is a built-in 
    nil)
  '("\\.tesl$")                      ;; files for which to activate this mode 
  nil                              ;; other functions to call
  "Major mode for TESL specifications (Heron)."            ;; doc string for this mode
  )

(provide 'tesl-mode)
