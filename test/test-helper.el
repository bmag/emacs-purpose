(set-frame-width nil 80)
(set-frame-height nil 24)

(message "setting undercover")
(require 'undercover)
(undercover "window-purpose.el"
            "window-purpose-configuration.el"
            "window-purpose-core.el"
            "window-purpose-layout.el"
            "window-purpose-prefix-overload.el"
            "window-purpose-switch.el"
            "window-purpose-utils.el"
            "window-purpose-fixes.el"
            "window-purpose-x.el"
            )

(message "loading purpose")

(require 'window-purpose)
(require 'window-purpose-x)
