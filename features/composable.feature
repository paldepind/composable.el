Feature: composable
  In order to efficiently edit text
  As an Emacs user
  I want composable editing

  Background:
    Given the buffer is empty
    And there is no region selected

  Scenario: Kill to end of line
    When I insert "foo bar"
    And I place the cursor after "foo"
    And I press "C-w e"
    Then I should not see "bar"
    Then I should see pattern "^foo$"

  Scenario: Delete to end of line
    When I insert "foo bar"
    And I place the cursor after "foo"
    And I press "C-k e"
    Then I should not see "bar"
    Then I should see pattern "^foo$"

  Scenario: Kill to beginning of line
    When I insert "foo bar"
    And I place the cursor after "foo "
    And I press "C-w a"
    Then I should not see "foo"
    Then I should see pattern "^bar$"

  Scenario: Delete to beginning of line
    When I insert "foo bar"
    And I place the cursor after "foo "
    And I press "C-k a"
    Then I should not see "foo"
    Then I should see pattern "^bar$"

  Scenario: C-w with active region
    When I insert "foo bar baz"
    And I select " bar"
    And I press "C-w"
    Then I should see "foo baz"

  Scenario: C-k with active region
    When I insert "foo bar baz"
    And I select " bar"
    And I press "C-k"
    Then I should see "foo baz"

  Scenario: Pass prefix argument along to action
    # Fixme

  Scenario: Kill word with forward word
    When I insert "first second third fourth"
    And I place the cursor after "first"
    And I press "C-w f"
    Then I should see "first third fourth"

  Scenario: Delete word with forward word
    When I insert "first second third fourth"
    And I place the cursor after "first"
    And I press "C-k f"
    Then I should see "first third fourth"

  Scenario: Kill paragraph
    When I insert:
    """
    Foo

    Paragraph

    Bar
    """
    And I place the cursor after "aragr"
    And I press "C-w h"
    Then I should see:
    """
    Foo

    Bar
    """

  Scenario: Passing numbers to motion
    When I insert "first second third fourth"
    And I place the cursor after "first"
    And I start an action chain
    And I press "C-w"
    And I press "2"
    And I press "f"
    And I execute the action chain
    Then I should see "first fourth"

  Scenario: C-w to kill line
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "2."
    And I press "C-w l"
    Then I should see:
    """
      1. line
      3. line
    """

  Scenario: C-k to delete line
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "2."
    And I press "C-k l"
    Then I should see:
    """
      1. line
      3. line
    """

  Scenario: Kill line by repeating action
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "2."
    And I press "C-w C-w"
    Then I should see:
    """
      1. line
      3. line
    """

  Scenario: Delete line by repeating action
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "2."
    And I press "C-k C-k"
    Then I should see:
    """
      1. line
      3. line
    """

  Scenario: Kill several lines by repeating action
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "2."
    And I press "C-w C-w C-w"
    Then I should not see "3. line"

  Scenario: Delete several lines by repeating action
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "2."
    And I press "C-k C-k C-k"
    Then I should not see "3. line"

  Scenario: Kill several lines
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "1."
    And I press "C-w 2 l"
    Then I should see:
    """
      3. line
    """

  Scenario: Delete several lines
    When I insert:
    """
      1. line
      2. line
      3. line
    """
    And I place the cursor after "1."
    And I press "C-k 2 l"
    Then I should see:
    """
      3. line
    """

  Scenario: Kill line backwards
    When I insert:
    """
    1. line
    2. line
    3. line
    """
    And I place the cursor after "2."
    And I start an action chain
    And I press "C-w"
    And I press "-"
    And I press "l"
    And I execute the action chain
    Then I should see:
    """
    2. line
    3. line
    """

  Scenario: Delete line backwards
    When I insert:
    """
    1. line
    2. line
    3. line
    """
    And I place the cursor after "2."
    And I start an action chain
    And I press "C-k"
    And I press "-"
    And I press "l"
    And I execute the action chain
    Then I should see:
    """
    2. line
    3. line
    """

  Scenario: Mark line
    When I insert:
    """
    1
    a line with text
    3
    """
    And I place the cursor after "with"
    And I press "C-SPC l"
    Then the region should be:
    """
    a line with text

    """

  Scenario: Mark word
    When I insert "first second third"
    And I place the cursor after "sec"
    And I press "C-SPC w"
    Then the region should be "second"

  Scenario: Mark three words forward
    When I insert "first second third fourth fifth"
    And I place the cursor before "second"
    And I press "C-SPC 3 f"
    Then the region should be "second third fourth"

  Scenario: Disabling composable mark mode
    When I insert "foo"
    And I start an action chain
    And I press "M-x"
    And I type "composable-mark-mode"
    And I execute the action chain
    And I press "C-SPC f"
    Then I should see "foof"

  Scenario: Cancel with C-g
    When I insert "first second third"
    And I place the cursor before "second"
    And I press "C-w"
    And I press "C-g"
    And I press "a"
    Then I should see "first asecond third"

  Scenario: Cancel with g
    When I insert "first second third"
    And I place the cursor before "second"
    And I press "C-w"
    And I press "g"
    And I press "a"
    Then I should see "first asecond third"

  Scenario: Use beginning of region with prefix
    When I insert "first second third"
    And I place the cursor before "second"
    And I start an action chain
    And I press "C-w"
    And I press ","
    And I press "l"
    And I execute the action chain
    Then I should see pattern "^second third$"

  Scenario: Use end of region with prefix
    When I insert "first second third"
    And I place the cursor before " second"
    And I start an action chain
    And I press "C-w"
    And I press "."
    And I press "l"
    And I execute the action chain
    Then I should see pattern "^first$"

  Scenario: Killing word forward with symmetric command
    When I insert "first second third"
    And I place the cursor before "cond"
    And I start an action chain
    And I press "C-w"
    And I press ","
    And I press "f"
    And I execute the action chain
    Then I should see "first  third"

  Scenario: Kill word backward with symmetric command
    When I insert "first second third"
    And I start an action chain
    And I place the cursor before "cond"
    And I press "C-w"
    And I press "."
    And I press "b"
    And I execute the action chain
    Then I should see "first  third"

  Scenario: Killing thing at indention with symmetric command
    When I insert "  foo"
    And I place the cursor before "oo"
    And I start an action chain
    And I press "C-w"
    And I press ","
    And I press "e"
    And I execute the action chain
    And I insert "bar"
    Then I should see "  bar"

  Scenario: Mark word with symmetric command
    When I insert "first second third"
    And I place the cursor before "cond"
    And I start an action chain
    And I press "C-SPC"
    And I press "."
    And I press "b"
    And I execute the action chain
    Then the region should be "second"

  Scenario: Repeat killing of word
    When I insert "first second third fourth fifth"
    And I place the cursor before " second"
    And I start an action chain
    And I press "C-w"
    And I press "f"
    And I press "f"
    And I press "f"
    And I execute the action chain
    Then I should see "first fifth"

  Scenario: Repeat uppercasing backward word
    When I insert "first second third fourth fifth"
    And I place the cursor before "fifth"
    And I start an action chain
    And I press "C-x C-u"
    And I press "b"
    And I press "b"
    And I press "b"
    And I execute the action chain
    Then I should see "first SECOND THIRD FOURTH fifth"

  Scenario: Repeat uppercasing paragraph
    When I insert:
    """
    Foo

    Paragraph

    Bar

    Baz
    """
    And I place the cursor after "Fo"
    And I start an action chain
    And I press "C-x C-u"
    And I press "h"
    And I press "h"
    And I press "h"
    And I execute the action chain
    Then I should see:
    """
    FOO

    PARAGRAPH

    BAR

    Baz
    """

  Scenario: Selecting lines with repeat
    When I insert:
    """
    1. line
    2. line
    3. line
    4. line
    """
    And I place the cursor after "2."
    And I press "C-SPC l l"
    Then the region should be:
    """
    2. line
    3. line

    """

  Scenario: Joining line with line below
    When I insert:
    """
    foo
      bar
    """
    And I place the cursor after "foo"
    And I press "C-w j"
    Then I should see "foobar"

  Scenario: Joining line with line above
    When I insert:
    """
    foo
      bar
    """
    And I place the cursor after "bar"
    And I press "C-w - j"
    Then I should see "foobar"

  Scenario: Executing action that does not move point
    When I insert "foo bar"
    And I place the cursor after "bar"
    And I press "C-w e"
    And I press "a"
    Then I should see "foo bara"

  Scenario: Kill a word
    When I insert "first second third fourth"
    And I place the cursor after "sec"
    And I press "C-w w"
    Then I should see "first  third fourth"

  Scenario: Delete a word
    When I insert "first second third fourth"
    And I place the cursor after "sec"
    And I press "C-k w"
    Then I should see "first  third fourth"

  Scenario: Kill several words with numeric and repeat
    When I insert "first second third fourth"
    And I place the cursor after "fi"
    And I press "C-w 2 w w"
    Then I should see pattern "^ fourth$"

  Scenario: Delete several words with numeric and repeat
    When I insert "first second third fourth"
    And I place the cursor after "fi"
    And I press "C-k 2 w w"
    Then I should see pattern "^ fourth$"

  Scenario: Kill several words with repeat backwards
    When I insert "first second third fourth fifth"
    And I place the cursor after "four"
    And I press "C-w - w w w"
    Then I should see pattern "^first  fifth$"

  Scenario: Kill a symbol
    When I insert "(first-symbol second third_symbol fourth)"
    And I place the cursor after "thir"
    And I press "C-w y"
    Then I should see "(first-symbol second  fourth)"

  Scenario: Kill symbol at beginning
    When I insert "(first-symbol second third_symbol fourth)"
    And I place the cursor before "third"
    And I press "C-w y"
    Then I should see "(first-symbol second  fourth)"

  Scenario: Kill several symbols with numeric and repeat
    When I insert "(first-symbol second third_symbol fourth)"
    And I place the cursor after "fi"
    And I press "C-w 2 y y"
    Then I should see "( fourth)"

  Scenario: Kill several symbols with zero start numeric and repeat
    When I insert "(first-symbol second third_symbol fourth)"
    And I place the cursor after "fi"
    And I press "C-w 02 y y"
    Then I should see "( fourth)"

  Scenario: Kill several symbols with repeat backwards
    When I insert "(first second third_symbol &fourth fifth)"
    And I place the cursor after "four"
    And I press "C-w - y y y"
    Then I should see "(first  fifth)"

  Scenario: Not breaking C-u C-SPC
    When I insert "a couple of words"
    And I place the cursor after "a"
    And I press "C-SPC C-SPC"
    And I place the cursor after "words"
    And I press "C-u C-SPC"
    Then the cursor should be between "a" and " couple"

  Scenario: Remove list from inside
    When I insert "(deep (nested (list arg1 arg2)))"
    And I place the cursor after "1"
    And I press "C-SPC o C-w"
    Then I should see "(deep (nested ))"

  Scenario: Remove list from deep inside
    When I insert "(deep (nested (list arg1 arg2)))"
    And I place the cursor after "1"
    And I press "C-SPC oo C-w"
    Then I should see "(deep )"

  Scenario: Kill list from inside
    When I insert "(deep (nested (list arg1 arg2)))"
    And I place the cursor after "1"
    And I press "C-w o"
    Then I should see "(deep (nested ))"

  Scenario: Kill list from deep inside
    When I insert "(deep (nested (list arg1 arg2)))"
    And I place the cursor after "1"
    And I press "C-w oo"
    Then I should see "(deep )"

  Scenario: Kill quoted string from inside
    When I insert:
      """
      (deep (nested (somefun "test1 test2")))
      """
    And I place the cursor after "1"
    And I press "C-w o"
    Then I should see:
      """
      (deep (nested (somefun )))
      """

  Scenario: Kill upper list from inside quoted string
    When I insert:
      """
      (deep (nested (somefun "test1 test2")))
      """
    And I place the cursor after "1"
    And I press "C-w oo"
    Then I should see:
      """
      (deep (nested ))
      """

  Scenario: Commenting lines with repeat
    When I insert:
    """
    1. line
    2. line
    3. line
    4. line
    """
    And I place the cursor after "2."
    And I press "M-; l l"
    Then I should see:
    """
    1. line
    ;; 2. line
    ;; 3. line
    4. line
    """

  Scenario: Kill with goto-char
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "C-w c t"
    Then I should see "first hird forth fifth"

  Scenario: Kill with goto-char repeating
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "C-w c t c c"
    Then I should see "first h"

  Scenario: Kill with goto-char repeating
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "C-w c t t t"
    Then I should see "first h"

  Scenario: Copy with goto-char repeating and C-e
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "M-w c t t"
    And I press "C-e"
    And I press "C-y"
    Then I should see "first second third forth fifthsecond third fort"

  Scenario: Copy with goto-char repeating inplace
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "M-w c t t"
    And I press "C-y"
    Then I should see "first second third fortsecond third forth fifth"

  Scenario: Copy with goto-char with numeric repeating inplace
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "M-w 2 c t"
    And I press "C-y"
    Then I should see "first second third fortsecond third forth fifth"

  Scenario: Kill with goto-char backward
    When I insert "first second third forth fifth"
    And I place the cursor after "forth"
    And I press "C-w - c r"
    Then I should see "first second third fo fifth"

  Scenario: Kill with goto-char with prefix
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "C-w 3 c t"
    Then I should see "first h"

  Scenario: Kill with composable-begin-argument
    When I insert "first second third forth fifth"
    And I place the cursor before "third"
    And I press "C-w , l"
    Then I should see "third forth fifth"

  Scenario: Kill with composable-end-argument
    When I insert "first second third forth fifth"
    And I place the cursor before "third"
    And I press "C-w . l"
    Then I should see "first second "

  Scenario: Composable-mark mode word
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "C-SPC w w"
    And I press "C-w"
    Then I should see "first  forth fifth"

  Scenario: Composable-mark mode end line
    When I insert "first second third forth fifth"
    And I place the cursor before "second"
    And I press "C-SPC e"
    And I press "C-w"
    Then I should see "first "
