# composable.el

Composable text editing for Emacs.

Porting vim's greates feature to Emacs.

## Introduction

### What

Every programmer knows that to maximize power and reuseability one has
to create abstractions. One does not write a specific function to
print "Hello", and a function to print "Good evening", and a function
to print "Foobar", and so on. Instead one creates a function that
takes any string as a parameter and prints it. This function can now
be used with any string. However the editing command in Emacs suffer
from excactly this inflexible specialization. There is `kill-region`,
`kill-word`, `kill-paragraph` and so on. There is a quite simple
abstraction that improves the situration drasticaly: each action (like
`kill`, `mark`, `comment`, etc.) can be combined with any selection to
operate on (like `word`, `sentence`, `paragraph`, etc.)

### How

The key insight in composable.el is that Emacs already provides all
the primitives to implement composable editing like in vim. What vim
call's an _operator_ is simply a command that operates on the current
region (like `kill-region`), a _motion command_ is just a command that
moves point and a _text object_ is like a command that marks a region.

### Why

If you in composable have `n` actions and `m` objects then you can
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

## Installation

Place `composable.el` in your load path.

```
Something.
```

tfitneftft
