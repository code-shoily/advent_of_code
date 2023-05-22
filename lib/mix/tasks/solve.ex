defmodule Mix.Tasks.Solve do
  @moduledoc """
  Solves a problem given `year` and `day`.

  Type `mix solve --year <year> --day <day>` to see result for <year> and day <day>.
  """
  alias AdventOfCode.Helpers.InputParser

  use Mix.Task

  @usage "mix solve --year <year> --day <day>"

  @shortdoc """
  Provides solution for <year> and <day>.
  """
  @impl Mix.Task
  def run(args) do
    case InputParser.parse(args) do
      {year, day} ->
        {part_1, part_2} = AdventOfCode.solve(year, day)

        Mix.shell().info("Part 1: #{part_1}\tPart 2: #{part_2}")

      _ ->
        Mix.shell().error("[Usage] #{@usage}")
    end
  end
end
