defmodule Mix.Tasks.Solve do
  @moduledoc """
  Generates boilerplate code for solving a problem for a `year` and `day`.

  Type `mix solve --year <year> --day <day>` to create the boilerplate files for
  solving the problem of the year <year> and day <day>.

  For example, `mix solve --year 2015 --day 2` will generate `lib/2015/day_2.ex`,
  `lib/data/inputs/2015_2.txt`, and `test/2015/day_2_test.exs` and contain the common
  structure (i.e. module name, file reader function, test cases etc)
  """

  alias AdventOfCode.Helpers.{FileWriter, InputParser}

  use Mix.Task

  @usage "mix solve --year <year> --day <day>"

  @shortdoc """
  Creates a boilerplate for solving a year/day's problem.
  """
  @impl Mix.Task
  def run(args) do
    case InputParser.parse(args) do
      {year, day} ->
        {to_string(year), to_string(day)}
        |> FileWriter.write()
        |> Mix.shell().info()

      _ ->
        Mix.shell().error("[Usage] #{@usage}")
    end
  end
end
