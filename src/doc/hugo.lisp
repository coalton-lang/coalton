(in-package #:coalton-impl/doc)

;;
;; Hugo docs generator for coalton-lang.github.io
;;

(defvar *coalton-docs-hugo-page-identifier* "Reference")

(defmethod write-documentation ((backend (eql ':hugo)) stream (object documentation-package-entries))
  (with-slots (packages asdf-system documentation-by-package) object

    ;; Write out title of page
    (format stream "---~%identifier: ~A~%---~%" *coalton-docs-hugo-page-identifier*)

    ;; Write out header for sidebar
    (format stream "
<style>
@media only screen and (max-width: 1250px) {
  .sidebar {
    display: none;
  }
}
</style>

<div class=\"sidebar\" style=\"height: 0; position: sticky; top: 10px\">
<div style=\"position: relative; right: 40%; width: 40%;\">

### Reference
~{~A~}

</div>
</div>
<div>~%~%"
            (mapcar
             (lambda (package)
               (let* ((package-entry (gethash package documentation-by-package))
                      ;; Only print out files that are used _and_ appear in the valid files list
                      (valid-files
                        (intersection
                         (reverse (documentation-package-entry-valid-files package-entry))
                         (alexandria:hash-table-keys
                          (documentation-package-entry-documentation-entries-by-file package-entry))
                         :test #'string=)))
                 (format nil
                         "- <a href=\"#~(~A-package~)\"><code>~:*~(~A~)</code></a>~%~{~A~}"
                         package
                         (mapcar
                          (lambda (file)
                            (format nil
                                    "  - <a href=\"#~(~A-~A-file~)\"><code>~A</code></a>~%"
                                    package
                                    (cl-ppcre:regex-replace-all "[^a-zA-Z\d\s:]" file "-")
                                    file))
                          valid-files))))
             packages))

    ;; For the main content, just render as markdown
    (dolist (package packages)
      (let ((docs-for-package (gethash package documentation-by-package)))
        (when docs-for-package
          (write-documentation ':markdown stream
                               docs-for-package))))

    (format stream "</div>
    </div>
    </div>")))
