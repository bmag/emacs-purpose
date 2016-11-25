@echo off
type NUL > tests\user-input.txt
cask exec emacs -batch -L . -L tests -l buttercup -f buttercup-run-discover < tests\user-input.txt
