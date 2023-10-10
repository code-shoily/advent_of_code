defmodule Mix.Tasks.GenReadme do
  @moduledoc """
  Updates the README.md for all years.

  Type `mix gen_readme` to update `README.md` with total stars gained for all years.

  For example, `mix gen_readme` will generate `README.md`.
  """

  alias AdventOfCode.Helpers.Summarizer

  use Mix.Task

  @usage "mix gen_readme"

  @shortdoc """
  Creates the per year README.md with updated solution status.
  """
  @impl Mix.Task
  def run(_) do
    read_me_static =
      :advent_of_code
      |> :code.priv_dir()
      |> Path.join("partials/readme_content.md")
      |> File.read!()

    content =
      """
      #{read_me_static}
      #{Summarizer.summarize()}
      """

    File.write!("README.md", content)
  rescue
    _ -> IO.puts(@usage)
  end
end
