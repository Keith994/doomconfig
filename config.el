;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ============================================================================
;; Configuration Loading Order
;; ============================================================================
;; Load order is important for dependencies:
;; 1. Base settings (user info, core settings)
;; 2. UI settings (theme, fonts, appearance)
;; 3. Custom functions
;; 4. Package configurations
;; 5. LSP configurations
;; 6. Key bindings (depends on all above)

(load! "+base.el")      ;; Core settings, user info, performance
(load! "+ui.el")        ;; Theme, fonts, appearance
(load! "+func.el")      ;; Custom functions
(load! "+use-package.el") ;; Package configurations
(load! "+lsp.el")       ;; LSP and programming configurations
(load! "+keys.el")      ;; Key bindings (should be last)
