# AdventOfCode

Advent of Code puzzle solutions in Elixir. It's work in progress and starting year 2015 to 2022, random days are attempted.

All solution modules reside in `lib/<year>/day_<day>.ex` and associated tests have the same structure in `test/` folder. Input files are in `lib/data/inputs/<year>_<day>.txt`.

To make the experience boilerplate-free and give all solutions uniform (more or less) structure, type `mix gen --year <year> --day <day>` or `mix gen <year> <day>` to have the structure and stub generated. Optionally, if you have `COOKIE` environment variable storing your cookie then it will download the input file and add the title on the moduledocs for you. Type `mix help gen` for more.

To solve the problem for a particular year/day, type in `mix solve --year <year> --day <day>` or `mix solve <year> <day>` and you will see the result of the mentioned `year/day` if it is valid and has been solved. Type `mix help solve` for more.

run `mix test` to ensure all solutions are working okay. To run test for a particular year and day, type `mix test --only y<last-two-digits-of-year><zero-padded-day>`. For example: `mix test --only y1501` tests for `2015` as year and `1` as day.

run `mix gen_stats` to update the relevant `README.md` so that the latest stats are always displayed.

## :trophy: 247/400

| Day | [2015](/lib/2015) | [2016](/lib/2016) | [2017](/lib/2017) | [2018](/lib/2018) | [2019](/lib/2019) | [2020](/lib/2020) | [2021](lib/2021) | [2022](lib/2022) |
|:---:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| :star2: | 48 | 22 | 34 | 15 | 15 | 44 | 24 | 45 |
| 1 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 2 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 3 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 4 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 5 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :2nd_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 6 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 7 | :1st_place_medal: | :1st_place_medal: | :2nd_place_medal: |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 8 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :2nd_place_medal: | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: |
| 9 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 10 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: |
| 11 | :1st_place_medal: |   | :1st_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: |
| 12 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: |
| 13 | :1st_place_medal: |   | :1st_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: |
| 14 | :1st_place_medal: |   | :2nd_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: |
| 15 | :1st_place_medal: |   | :1st_place_medal: |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 16 | :1st_place_medal: |   | :1st_place_medal: |   |   | :2nd_place_medal: | :1st_place_medal: |   |
| 17 | :1st_place_medal: |   | :1st_place_medal: |   |   |   |   |   |
| 18 | :1st_place_medal: |   | :2nd_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: |
| 19 | :1st_place_medal: |   |   |   |   | :1st_place_medal: |   | :1st_place_medal: |
| 20 | :1st_place_medal: |   |   |   |   | :2nd_place_medal: |   | :1st_place_medal: |
| 21 | :1st_place_medal: |   |   |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 22 |   |   |   |   |   | :1st_place_medal: |   | :2nd_place_medal: |
| 23 | :1st_place_medal: |   | :2nd_place_medal: |   |   | :2nd_place_medal: |   | :1st_place_medal: |
| 24 | :1st_place_medal: |   |   |   |   | :2nd_place_medal: |   | :1st_place_medal: |
| 25 | :1st_place_medal: |   |   |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |


