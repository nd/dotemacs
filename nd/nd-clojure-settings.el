;;;; clojure settings

;; clojure-mode
(add-to-list 'load-path "~/distr/clojure-mode")
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path "~/distr/swank-clojure")
(setq swank-clojure-jar-path "~/.clojure/clojure.jar")
(setq swank-clojure-extra-classpaths (list "~/.clojure/clojure-contrib.jar;~/projects/sicp/ch01;~/projects/sicp/ch02"))
(require 'swank-clojure-autoload)

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/distr/slime")
(require 'slime)
(slime-setup)


(provide 'nd-clojure-settings)