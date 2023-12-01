defmodule Mix.Tasks.Solve do
  @moduledoc """
  Solves a problem given `year` and `day`.

  Type `mix solve --year <year> --day <day>` to see result for <year> and day <day>.
  """
  alias AdventOfCode.Helpers.InputParser

  use Mix.Task

  @usage "mix solve --year <year> --day <day> OR mix solve <year> <day>"

  @shortdoc """
  Provides solution for <year> and <day>.
  """
  @impl Mix.Task
  def run(args) do
    case InputParser.parse(args) do
      {year, day} ->
        case AdventOfCode.solve(year, day) do
          {:ok, {part_1, part_2}} -> Mix.shell().info("Part 1: #{part_1}\tPart 2: #{part_2}")
          {:error, :not_yet_solved} -> Mix.shell().error("#{year}/#{day} is not solved yet")
          {:error, :invalid_args} -> Mix.shell().error("Invalid year/day: #{year}/#{day}")
        end

      _ ->
        Mix.shell().error("""
        [Usage] #{@usage}
        \tWHERE 2015 <= year <= #{AdventOfCode.get_latest_year()} and 1 <= day <= 25
        """)
    end
  end
end
