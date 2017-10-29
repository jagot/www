(require 'widget)
(require 'dash-functional)
(require 's)

(eval-when-compile
  (require 'wid-edit))

(defun get-bibtex-alist (bibkey)
  (let* ((data
          (with-current-buffer
              (find-file-noselect bibliography-file)
            (save-excursion
              (bibtex-search-crossref bibkey)
              (bibtex-parse-entry)))))
    (mapcar*
     (lambda (d) (cons (s-downcase (car d)) (cdr d)))
     data)))

(defun get-bibtex-keys (search-term)
  (let* ((candidates (bibtex-completion-candidates)))
    (mapcar* (lambda (a)
               (cdr (assoc "=key=" (cdr a))))
             (-filter
              (lambda (c) (s-matches? search-term (first c)))
              candidates))))

(defvar reference-face 
  '(:family
    "Futura"))

(defvar reference-heading-face
  '(:family
    "Futura"
    :foreground
    "red"
    :weight
    :bold))

;; org-ref-nonascii-latex-replacements

(defun curly-strip (v)
  (s-chop-prefix
    "{" (s-chop-suffix
         "}" v)))

(defun ascii-replace (val)
  (s-replace
   " " " "
   (let* ((v val))
     (dolist (e org-ref-nonascii-latex-replacements v)
       (message "%s\n" (cdr e))
       (setq v (s-replace
                (cdr e)
                (car e) v))       
       (setq v (s-replace
                (s-replace "\\\\" "\\" (cdr e))
                (car e) v))
       (setq v (s-replace
                (curly-strip (s-replace "\\\\" "\\" (cdr e)))
                (car e) v))
       (setq v (s-replace
                (s-replace "}" ""
                           (s-replace "{" ""
                                      (curly-strip (s-replace "\\\\" "\\" (cdr e)))))
                (car e) v))))))

(defun ref-strip (key data)
  (s-replace "}" ""
             (s-replace "{" ""
                        (ascii-replace
                         (curly-strip (cdr (assoc key data)))))))

(defun reference-form (bibkey)
  (let* ((data (get-bibtex-alist bibkey)))
    (widget-insert
     (propertize bibkey
                 'face reference-heading-face)
     (propertize
      (concat       
       ": "
       (ref-strip "title" data)
       "\n"
       (ref-strip "author" data)
       nil
       "\n\n")
      'face reference-face))                   
    (widget-create 'radio-button-choice
                   :value "Normal"
                   '(item "Normal")
                   '(item "Selected")
                   '(item "Ignored"))
    (widget-insert (format "%s\n\n"
                           (mapcar* 'first data))
                   "\n\n\n")))

(defun reference-gui (search-term &optional N)
  "A simple gui for selecting references"
  (interactive)
  (switch-to-buffer "*References*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Orpheus Orm's Reference Selector.\n\n")
  
  (let* ((keys (subseq (get-bibtex-keys search-term) 1 (if (numberp N) (+ 1 N) nil))))
    (mapcar* 'reference-form keys))
  
  (use-local-map
   (let* ((map (make-sparse-keymap)))
     (set-keymap-parent map widget-keymap)
     map))
  (local-set-key "q" 'bury-buffer)
  (widget-setup))

(reference-gui "Carlström")
