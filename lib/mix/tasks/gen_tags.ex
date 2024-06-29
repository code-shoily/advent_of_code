defmodule Mix.Tasks.GenTags do
  @moduledoc """
  Updates the wiki/tags.md for all years.

  Type `mix gen_tags` to update all `wiki/tags.md` with latest tag summary.
  """

  alias AdventOfCode.Helpers.Summarizer

  use Mix.Task

  @usage "mix gen_tags"

  @filename "wiki/tags.md"

  @shortdoc """
  Creates or updates `#{@filename}` with latest tag data.
  """
  @impl Mix.Task
  def run(_) do
    content = Summarizer.tag_page()
    File.write!(@filename, content)
    Mix.shell().info("#{@filename} successfully updated!")
  rescue
    _ -> Mix.shell().error(@usage)
  end
end
