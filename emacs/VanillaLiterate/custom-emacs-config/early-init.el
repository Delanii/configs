;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
;; NOTE: early-init.el is now generated from config.org.  Please edit that file instead

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
