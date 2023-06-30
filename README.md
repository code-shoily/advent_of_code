# AdventOfCode

Advent of Code puzzle solutions in Elixir. It's work in progress and starting year 2015 to 2022, random days are attempted.

All solution modules reside in `lib/<year>/day_<day>.ex` and associated tests have the same structure in `test/` folder. Input files are in `lib/data/inputs/<year>_<day>.txt`.

To make the experience boilerplate-free and give all solutions uniform (more or less) structure, type `mix gen --year <year> --day <day>` or `mix gen <year> <day>` to have the structure and stub generated. Optionally, if you have `COOKIE` environment variable storing your cookie then it will download the input file and add the title on the moduledocs for you. Type `mix help gen` for more.

To solve the problem for a particular year/day, type in `mix solve --year <year> --day <day>` or `mix solve <year> <day>` and you will see the result of the mentioned `year/day` if it is valid and has been solved. Type `mix help solve` for more.

run `mix test` to ensure all solutions are working okay. To run test for a particular year and day, type `mix test --only y<last-two-digits-of-year><zero-padded-day>`. For example: `mix test --only y1501` tests for `2015` as year and `1` as day.

## :star2: 236/400

| Year | Progress | :star2: |
| :-----: | :------: | :-:|
| [2015](/lib/2015) | :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :new_moon: :full_moon: :full_moon: :full_moon: |48|
| [2016](/lib/2016) |:full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :new_moon: :full_moon: :full_moon: :new_moon: :full_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: |20|
| [2017](/lib/2017) |:full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :last_quarter_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :last_quarter_moon: :full_moon: :full_moon: :full_moon: :last_quarter_moon: :new_moon: :new_moon: :new_moon: :new_moon: :last_quarter_moon: :new_moon: :new_moon: |34|
| [2018](/lib/2018) | :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :new_moon: :last_quarter_moon: :full_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: |15|
| [2019](/lib/2019) |:full_moon: :full_moon: :full_moon: :full_moon: :last_quarter_moon: :full_moon: :new_moon: :full_moon: :new_moon: :full_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: |16|
| [2020](/lib/2020) | :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :last_quarter_moon: :new_moon: :full_moon: :full_moon: :last_quarter_moon: :full_moon: :full_moon: :last_quarter_moon: :last_quarter_moon: :full_moon: |34|
| [2021](/lib/2021) | :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :new_moon: :full_moon: :new_moon: :new_moon: :new_moon: :new_moon: :new_moon: :full_moon: :full_moon: :new_moon: :new_moon: :new_moon: :new_moon: :full_moon: :new_moon: :new_moon: :new_moon: :full_moon: |24|
| [2022](/lib/2022) | :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :full_moon: :new_moon: :new_moon: :full_moon: :full_moon: :full_moon: :full_moon: :last_quarter_moon: :full_moon: :full_moon: :full_moon: |45|
