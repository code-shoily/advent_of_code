defmodule Mix.Tasks.GenDifficulties do
  @moduledoc """
  Updates the difficulties.md for all years.

  Type `mix gen_difficulties` to update `difficulties.md` with latest difficulty summary.
  """

  alias AdventOfCode.Helpers.Summarizer

  use Mix.Task

  @usage "mix gen_difficulties"

  @filename "difficulties.md"

  @shortdoc """
  Creates or updates `difficulties.md` with latest tag data.
  """
  @impl Mix.Task
  def run(_) do
    content = Summarizer.difficulty_page()
    File.write!(@filename, content)
    Mix.shell().info("#{@filename} successfully updated!")
  rescue
    _ -> Mix.shell().error(@usage)
  end
end
