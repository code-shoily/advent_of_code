defmodule Mix.Tasks.YearStats do
  @moduledoc """
  Updates the README.md for `year` with solution statuses.

  Type `mix year_stats --year <year>` to update `year/README.md`.

  For example, `mix year_stats --year 2015` will generate `lib/2015/README.md`
  """

  alias AdventOfCode.Helpers.Summarizer

  use Mix.Task

  @usage "mix year_stats --year <year>"

  @shortdoc """
  Creates the per year README.md with updated solution status.
  """
  @impl Mix.Task
  def run(args) do
    year =
      case args do
        [year] ->
          String.to_integer(year)

        ["--year", year] ->
          String.to_integer(year)
      end

    readme_content = Summarizer.yearwise_readme(year)

    File.write!("lib/#{year}/README.md", readme_content)

    IO.puts("#{year}/README.md successfully updated!")
  rescue
    _ -> IO.puts(@usage)
  end
end
