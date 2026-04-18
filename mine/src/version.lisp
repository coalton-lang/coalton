;;;; version.lisp -- Build version string for mine.

(defpackage #:mine/version
  (:use #:cl)
  (:export #:*mine-version*))

(in-package #:mine/version)

(defvar *mine-version* "[unknown-build]"
  "Version string baked in at build time.
Release builds set MINE_VERSION (e.g. \"v0.0.0-alpha1m\"); dev builds
fall back to the git short hash; otherwise \"[unknown-build]\".")
