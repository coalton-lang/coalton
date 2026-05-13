(in-package #:mine-tests)

(defun check-current-release-tag-prefixes-bare-version ()
  (let ((mine/version:*mine-version* "0.1.6"))
    (%check (string= "mine-v0.1.6"
                     (mine/app/check-update::current-release-tag))
            "Expected bare mine version to become mine-v0.1.6, got ~S"
            (mine/app/check-update::current-release-tag))))

(defun check-current-release-tag-keeps-prefixed-version ()
  (let ((mine/version:*mine-version* "mine-v0.1.6"))
    (%check (string= "mine-v0.1.6"
                     (mine/app/check-update::current-release-tag))
            "Expected prefixed mine version to stay mine-v0.1.6, got ~S"
            (mine/app/check-update::current-release-tag))))

(defun check-current-release-tag-accepts-v-prefixed-version ()
  (let ((mine/version:*mine-version* "v0.1.6"))
    (%check (string= "mine-v0.1.6"
                     (mine/app/check-update::current-release-tag))
            "Expected v-prefixed mine version to become mine-v0.1.6, got ~S"
            (mine/app/check-update::current-release-tag))))
