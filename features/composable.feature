Feature: composable
  In order to efficiently edit text
  As an Emacs user
  I want composable editing

  Background:
    Given the buffer is empty
    And there is no region selected

  Scenario: Delete to end of line
    When I insert "foo bar"
    And I place the cursor after "foo"
    And I press "C-w e"
    Then I should not see "bar"
    Then I should see pattern "^foo$"

  Scenario: Delete to beginning of line
    When I insert "foo bar"
    And I place the cursor after "foo "
    And I press "C-w a"
    Then I should not see "foo"
    Then I should see pattern "^bar$"

  Scenario: C-w with active region
    When I insert "foo bar baz"
    And I select " bar"
    And I press "C-w"
    Then I should see "foo baz"

  Scenario: Deleting word
    When I insert "first second third fourth"
    And I place the cursor after "first"
    And I press "C-w f"
    Then I should see "first third fourth"

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
