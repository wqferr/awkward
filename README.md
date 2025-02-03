# Awkward
A more ergonomic AWK

## What is this?
This is meant as an attempt to make AWK more CLI friendly. For one, variables are now accessed with @ instead of $, so you're free to use any quotation type for the program without fear of forgetting to escape one of those little buggers.

It also supports most of AWK's core features, like regex matching lines, replacing and/or inserting fields, creating new records and more, along with new ones, like an "else" condition for lines that didn't match the previous rule, and a header flag that lets you index fields by their name.

## Usage
Please note that this is still very much in experimental territory, so any of the following examples are subject to change (and maybe forgetfulness to update the docs). If any of the following examples dont work as expected, please let me know in an issue and I'll fix it right away.

### Syntax
An Awkward (awd) program is structured in the following way:
A "rule" is of the form `condition -> action1, action2, action3;`. If `condition ->` is omitted, the rule runs for every record. There are 3 special keywords that can be used for `condition`: `start`, `end`, and `else`.

- `start` makes the rule run before any record is processed (or with the header record, if the flag is set).
- `end` makes the rule run after the last record is processed.
- `else` makes the rule run only on records which failed to meet the condition specified immediately before it.

An `action` can be any valid `expression`, which are:
- Variable assignment (`varname = expr`).
- \*Read a variable (`varname`).
- Field assignment (`@1 = expr`).
- \*Read a field (`@1`).
- \*Read a numeric field (`n@1`).
- \*Read a boolean field (`b@1`).
- Drop statement (`drop @1, @2`).
- Keep statement (`keep @1, @2`).
- \*A string literal (`"a string"`).
- \*A number literal (`0.5`).
- \*A boolean literal (`true`).
- A function call (`print("something", @1)`).
- \*Any arithmetic expression involving addition, subtraction, multiplication, division, and modulo.
- \*Simple concatenate (`"hello" . "," . "world!"`).
- \*Field-separator-included concatenate (`"hello" .. "world"`).
- \*Any equality or inequality expression (`var < 3`).
- \*Any boolean algebra expression involving `and`, `or`, and `not` (`var1 and b@3`).
- \*Regex match (`/^\d+$/`).

`expressions` marked with \* are no-ops when used as actions.

## Why use this over AWK?

While Awkward is not as powerful as AWK (it doesn't support loops, for example), it makes some quality of life changes.
For example, variables are accessed with `@` instead of `$`. This means you can use either type of quote from your shell
when writing inline programs, without fear of having to escape variable accesses when meshing shell variables and Awkward variables.

Awkward also provides no type coersions. If you want a number, you have to request it. That makes it easier to pin down where
a conversion is going wrong.
