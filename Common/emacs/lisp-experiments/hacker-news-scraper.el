;;; hacker-news-scraper.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(require 'dom)
(require 'url)
(require 'cl-lib)

(defun my/demo-scrape (url)
    "Download URL and return the Hacker News headlines as a list of conses.
Each element is (TITLE . HREF)."
    (with-current-buffer (url-retrieve-synchronously url t t 30)
        (goto-char (point-min))
        ;; Skip HTTP headers until the first blank line.
        (re-search-forward "\r?\n\r?\n" nil t)
        (let* ((dom (libxml-parse-html-region (point) (point-max)))
                  ;; On HN each headline is <span class="titleline"><a>...</a>.
                  (titles (dom-by-class dom "titleline")))
            (mapcar (lambda (node)
                        (let ((a (dom-child-by-tag node 'a)))
                            (cons (string-trim (dom-texts a))   ; link text
                                (dom-attr a 'href))))          ; destination
                titles))))

(defun my/demo-scrape-hn ()
    "Download the Hacker News front page and show the headlines in a buffer."
    (interactive)
    (let ((items (my/demo-scrape "https://news.ycombinator.com/")))
        (with-output-to-temp-buffer "*HN headlines*"
            (princ (format "Headlines found: %d\n\n" (length items)))
            (cl-loop for (title . href) in items
                for i from 1
                do (princ (format "%2d. %s\n    %s\n\n" i title href))))))

;; Evaluating the buffer (M-x eval-buffer) runs it directly:
(my/demo-scrape-hn)

(provide 'hacker-news-scraper)
;;; hacker-news-scraper.el ends here
