<!--
* AUTOGENERATED -- DO NOT EDIT
* Edit static content in `priv/partials/readme_content.md`
* Run `mix gen_readme` to recreate
-->
# AdventOfCode

Advent of Code puzzle solutions in Elixir. It's work in progress and starting year 2015 to 2022, random days are attempted.

## Code Organization

All solution modules reside in `lib/<year>/day_<day>.ex` and associated tests have the same structure in `test/` folder. Input files are in `lib/data/inputs/<year>_<day>.txt`.

## Stubs

To make the experience boilerplate-free and give all solutions uniform (more or less) structure, type `mix gen --year <year> --day <day>` or `mix gen <year> <day>` to have the structure and stub generated. Optionally, if you have `COOKIE` environment variable storing your cookie then it will download the input file and add the title on the moduledocs for you. Type `mix help gen` for more.

To solve the problem for a particular year/day, type in `mix solve --year <year> --day <day>` or `mix solve <year> <day>` and you will see the result of the mentioned `year/day` if it is valid and has been solved. Type `mix help solve` for more.

## Introducing a new year

During December 1 of a new year (if Advent of Code is happening that year), the following steps should be taken (this should be more streamlined in future):

* Create a new folder `XXXX` (in year format) under `/lib` or `/test`
* Update the `@latest_year` attribute on `advent_of_code.ex`
* Double check your session cookie (in case you're using it)

All commands should work from then on.

## Testing

run `mix test` to ensure all solutions are working okay. To run test for a particular year and day, type `mix test --only y<last-two-digits-of-year><zero-padded-day>`. For example: `mix test --only y1501` tests for `2015` as year and `1` as day.

## Updating README and Stats

If you want to update the `README.md` (i.e. this file) please do not update `README.me` directly, instead update the static content (not stats related) on `priv/partials/readme_content.md` and then run `mix readme` to regenerate this file.

The stats, both in `<year>/README.md` and the bottom part of `README.md` are autogenerated by pulling from the live code state. To keep the stats updated, run `mix update_stats` command. This does the following two things:

* Runs `mix gen_stats` to update the relevant `README.md` so that the latest stats are always displayed.
* Runs `mix gen_readme` to update the main `README.md` to reflect all the stars.
* Runs `mix gen_tags` to update `tags.md` with latest tag summary
* Runs `mix gen_difficulties` to update `difficulties.md` with latest difficulty summary

Note that you can call those commands independently and also set up pre-commit hooks to run it for you.

## Pages

* [tags.md](/tags.md) contains information about tags, a loose attempt to have an idea of nature (i.e. algorithms, output type, input size etc) of puzzles
* [difficulties.md](/difficulties.md) contains information about relative difficulties, marked with :snowflake: (higher number of icons represent higher difficulty)
* Each year has its own `README.md` to have summarized information of that year. Those pages are:
    - [2015/README.md](/lib/2015/README.md)
    - [2016/README.md](/lib/2016/README.md)
    - [2017/README.md](/lib/2017/README.md)
    - [2018/README.md](/lib/2018/README.md)
    - [2019/README.md](/lib/2019/README.md)
    - [2020/README.md](/lib/2020/README.md)
    - [2021/README.md](/lib/2021/README.md)
    - [2022/README.md](/lib/2022/README.md)
    - [2023/README.md](/lib/2023/README.md)

Note: All files mentioned above are autogenerated and are created by running `mix update_stats` task.

## :trophy: 287/450

| Day | [2015](/lib/2015) | [2016](/lib/2016) | [2017](/lib/2017) | [2018](/lib/2018) | [2019](/lib/2019) | [2020](/lib/2020) | [2021](/lib/2021) | [2022](/lib/2022) | [2023](/lib/2023) |
|:---:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| :star2: | 48 | 22 | 34 | 15 | 15 | 44 | 24 | 45 | 40 |
| 1 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 2 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 3 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 4 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 5 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :2nd_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 6 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 7 | :1st_place_medal: | :1st_place_medal: | :2nd_place_medal: |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 8 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :2nd_place_medal: | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 9 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 10 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 11 | :1st_place_medal: |   | :1st_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 12 | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 13 | :1st_place_medal: |   | :1st_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 14 | :1st_place_medal: |   | :2nd_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 15 | :1st_place_medal: |   | :1st_place_medal: |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |
| 16 | :1st_place_medal: |   | :1st_place_medal: |   |   | :2nd_place_medal: | :1st_place_medal: |   | :1st_place_medal: |
| 17 | :1st_place_medal: |   | :1st_place_medal: |   |   |   |   |   |   |
| 18 | :1st_place_medal: |   | :2nd_place_medal: |   |   | :1st_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 19 | :1st_place_medal: |   |   |   |   | :1st_place_medal: |   | :1st_place_medal: |   |
| 20 | :1st_place_medal: |   |   |   |   | :2nd_place_medal: |   | :1st_place_medal: |   |
| 21 | :1st_place_medal: |   |   |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |   |
| 22 |   |   |   |   |   | :1st_place_medal: |   | :2nd_place_medal: |   |
| 23 | :1st_place_medal: |   | :2nd_place_medal: |   |   | :2nd_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 24 | :1st_place_medal: |   |   |   |   | :2nd_place_medal: |   | :1st_place_medal: | :1st_place_medal: |
| 25 | :1st_place_medal: |   |   |   |   | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: | :1st_place_medal: |


