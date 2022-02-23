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
               (format nil
                       "- <a href=\"#~(~A-package~)\"><code>~:*~(~A~)</code></a>~%"
                       package))
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
