# AdventOfCode

Advent of Code puzzle solutions in Elixir. It's work in progress and starting year 2015 to 2022, random days are attempted.

All solution modules reside in `lib/<year>/day_<day>.ex` and associated tests have the same structure in `test/` folder. Input files are in `lib/data/inputs/<year>_<day>.txt`.

To make the experience boilerplate-free and give all solutions uniform (more or less) structure, type `mix gen --year <year> --day <day>` or `mix gen <year> <day>` to have the structure and stub generated. Optionally, if you have `COOKIE` environment variable storing your cookie then it will download the input file and add the title on the moduledocs for you. Type `mix help gen` for more.

To solve the problem for a particular year/day, type in `mix solve --year <year> --day <day>` or `mix solve <year> <day>` and you will see the result of the mentioned `year/day` if it is valid and has been solved. Type `mix help solve` for more.

run `mix test` to ensure all solutions are working okay. To run test for a particular year and day, type `mix test --only y<last-two-digits-of-year><zero-padded-day>`. For example: `mix test --only y1501` tests for `2015` as year and `1` as day.

run `mix gen_stats` to update the relevant `README.md` so that the latest stats are always displayed.

run `mix gen_readme` to update the main `README.md` to reflect all the stars.
