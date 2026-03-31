defmodule Mix.Tasks.Gen do
  @moduledoc """
  Generates boilerplate code for solving a problem for a `year` and `day`.

  Type `mix gen --year <year> --day <day>` to create the boilerplate files for
  solving the problem of the year <year> and day <day>.

  For example, `mix gen --year 2015 --day 2` will generate `lib/2015/day_02.ex`,
  `priv/input_files/2015_2.txt`, and `test/2015/day_02_test.exs` and contain the common
  structure (i.e. module name, file reader function, test cases etc)
  """

  alias AdventOfCode.Helpers.{Generator, InputParser}

  use Mix.Task

  @usage "mix gen --year <year> --day <day> OR mix gen <year> <day>"

  @shortdoc """
  Creates a boilerplate for solving a year/day's problem.
  """
  @impl Mix.Task
  def run(args) do
    case InputParser.parse(args) do
      {year, day} ->
        {to_string(year), to_string(day)}
        |> Generator.run()
        |> Enum.each(fn
          {:ok, path} ->
            Mix.shell().info([:green, "* created ", :reset, path])

          {:exists, path} ->
            Mix.shell().info([:yellow, "* skipped ", :reset, path, " (already exists)"])

          {:error, path, reason} ->
            Mix.shell().error([:red, "* error   ", :reset, path, " (reason: #{inspect(reason)})"])
        end)

      _ ->
        Mix.shell().error("[Usage] #{@usage}")
    end
  end
end
