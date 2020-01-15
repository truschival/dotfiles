;;==============================================================================
;; https://www.emacswiki.org/emacs/SortWords
;;==============================================================================

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))


(defun quote-symbols (reverse beg end)
  "Put symbols in double-quotes."
  (query-replace-regexp "\\(\\sw\\|\\s_\\)+" "\1"  beg end))

(provide 'custom-lisp)
