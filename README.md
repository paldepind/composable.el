# composable.el

> Let there be composable editing!

Composable text editing is vim's greatest feature. composable.el is
composable text editing for Emacs. It drastically improves the basic
editing power of Emacs by making commands combineable.

## Introduction

### Why

Every programmer knows that to maximize power and reuseability one has
to create abstractions. One does not write a specific function to
print "Hello", and a second function to print "Good evening", and a
third function to print "Foobar", and so on. Instead one creates a
function that takes any string as a parameter and prints it. However
the editing command in Emacs suffer from excactly this inflexible
specialization. There is `kill-region`, `kill-word`, `kill-paragraph`
and so on.

There is a quite simple abstraction that improves the situration
drasticaly: Make it possible to combine any action (like `kill`,
`mark`, `comment`, etc.) with any boundary to operate on (like `word`,
`sentence`, `paragraph`, etc.). This abstraction is known and
appreciated by all experienced users of vim.

If you in composable.el have `n` actions and `m` objects then you can
combine them in `n * m` ways. Without composable.el you'd need `n * m`
commands in total. Furthermore after an action an object is always
expected. Thus you can activate an additional layer of keybindings
after executing an action.

Finally you excape from many inconsistencies and missing pieces in
Emacs. For instance you can by default go to the end of line with
`move-end-of-line` and you can kill to the end of the line with
`kill-line`. But even though you can go to the beginning of the line
with `move-begnning-of-line` there is no way to kill to the beginning
of the line. With composable.el you can _always_ apply any actions
on any unit of operation.

### How

The key insight in composable.el is that Emacs already provides all
the primitives to implement composable editing. What vim call's an
_operator_ is simply a command that operates on the current region
(like `kill-region`), a _motion command_ is just a command that moves
point and a _text object_ is like a command that marks a region.

Thus composable.el reuses existing Emacs infrastructure. It is a small
focused package that only brings together pieces existing pieces in a
new way.

## Overview

* A _operator_ is any plain Emacs command that works on the region. An
  example is `kill-region`.
* A _boundary_ is a any commmand that moves point and obtionally sets
  the mark as well. Examples are `next-line` and `mark-paragraph`.
* A _composable command_ is a command that after being called waits
  for a boundary and then executes an operator. A composable can be
  created by the function `composable-create-composable`.
* When a composable command has been called a special keymap layer is
  activated. This layer contains boundary commands.

## Installation

Place `composable.el` in your load path.

```
(use-package composable
  :config
  (composable-mode))
```

# Repeat
