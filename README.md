# AdventOfCode

Advent of Code puzzle solutions in Elixir. It's work in progress and starting year 2015 to 2019, random days are attempted.

All solution modules reside in `lib/<year>/day_<day>.ex` and associated tests have the same structure in `test/` folder. Input files are in `lib/data/inputs/<year>_<day>.txt`.

To make the experience boilerplate free and give all solutions uniform (more or less) structure, type `mix solve --year <year> --day <day>` to have the structure and stub generated. For example, `mix solve --year 2015 --day 2` will generate `lib/2015/day_2.ex`, `lib/data/inputs/2015_2.txt`, and `test/2015/day_2_test.exs` and contain the common structure (i.e. module name, file reader function, test cases etc).

run `mix test` to ensure all solutions are working okay.
