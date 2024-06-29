defmodule Mix.Tasks.GenDifficulties do
  @moduledoc """
  Updates the wiki/difficulties.md for all years.

  Type `mix gen_difficulties` to update `wiki/difficulties.md` with latest difficulty summary.
  """

  alias AdventOfCode.Helpers.Summarizer

  use Mix.Task

  @usage "mix gen_difficulties"

  @filename "wiki/difficulties.md"

  @shortdoc """
  Creates or updates `#{@filename}` with latest tag data.
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
