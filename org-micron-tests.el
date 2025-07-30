;; -*- lexical-binding: t; -*-

(require 'org)
(require 'org-micron)

(defun org-micron-test (a b)
  (should (equal
	   (org-test-with-temp-text a (org-export-as 'micron))
	   b)))

(ert-deftest test-org-micron/bold ()
  "Ensure that org-micron-bold properly returns a Micron bold"
  (org-micron-test "*bold*" "`!bold`!\n"))

(ert-deftest test-org-micron/italic ()
  "Ensure that org-micron-italic returns a Micron italic"
  (org-micron-test "/italic/" "`*italic`*\n"))

(ert-deftest test-org-micron/underline ()
  "Ensure that org-micron-underline returns a Micron underline"
  (org-micron-test "_underline_" "`_underline`_\n"))

(ert-deftest test-org-micron/link-file ()
  "Ensure that org-micron-link converts an org file link to a micron link"
  (org-micron-test "[[file:///fake/test.org]]" "`[/fake/test.mu]\n"))

(ert-deftest test-org-micron/link-rns ()
  "Ensure that org-micron-link creates valid RNS links"
  (org-micron-test "[[3b5bc6888356193f1ac1bfb716c1beef:/page/index.mu]]" "`[3b5bc6888356193f1ac1bfb716c1beef:/page/index.mu]\n"))

(ert-deftest test-org-micron/link-rns-descr ()
  "Ensure that org-micron-link creates valid RNS links with a description"
  (org-micron-test "[[3b5bc6888356193f1ac1bfb716c1beef:/page/index.mu][my link]]" "`[my link`3b5bc6888356193f1ac1bfb716c1beef:/page/index.mu]\n"))
