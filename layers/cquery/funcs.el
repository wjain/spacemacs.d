(require 'cl-lib)

(defun cquery//enable ()
  (when
      (and buffer-file-name
           (or (locate-dominating-file default-directory "compile_commands.json")
               (locate-dominating-file default-directory ".cquery")))
    (lsp-cquery-enable)))
