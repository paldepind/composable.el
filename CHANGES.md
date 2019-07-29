# Changes

- master
  - Fixed issues with comment commands,
  - Added which-key integration in `which-key-mode`.
  - Mark commands marks from beginning to end when used inside
    composable-mark-mode. For instance `C-SPC w` marks entire word
  - Properly detect prefix arguments in Emacs >= 25.1
  - Fix breakage of mark popping with C-u C-SPC
  - Fix error when object mode entered trough composable-mark-mode for
    the first time.
  - Highlight region saved with M-w.
  - A region can be selected by repeating the same composable command
    twice.
  - Support setting custom faces in object mode.
- 0.0.1
  - Initial stable release
