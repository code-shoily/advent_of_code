defmodule Mix.Tasks.GenStats do
  @moduledoc """
  Updates the README.md for all years.
  
  Type `mix gen_stats` to update all `year/README.md`.
  
  For example, `mix gen_stats` will generate `lib/<year>/README.md` of all years.
  """

  alias AdventOfCode.Helpers.Summarizer

  use Mix.Task

  @year_range 2015..2022

  @usage "mix gen_stats"

  @shortdoc """
  Creates the per year README.md with updated solution status.
  """
  @impl Mix.Task
  def run(_) do
    for year <- @year_range do
      readme_content = Summarizer.yearwise_readme(year)

      File.write!("lib/#{year}/README.md", readme_content)

      Mix.shell().info("#{year}/README.md successfully updated!")
    end
  rescue
    _ -> Mix.shell().error(@usage)
  end
end
