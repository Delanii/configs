;;; cli.el -*- lexical-binding: t; -*-
(setq org-confirm-babel-evaluate nil)

;; Má umožnit spouštění kódu v org-mode bez problémů a potvrzování.
;; Odkaz: https://github.com/tecosaur/emacs-config/blob/master/config.org

(defun doom-shut-up-a (orig-fn &rest args)
  (quiet! (apply orig-fn args)))

(advice-add 'org-babel-execute-src-block :around #'doom-shut-up-a)
