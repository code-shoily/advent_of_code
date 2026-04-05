defmodule Mix.Tasks.Readme do
  @moduledoc """
  Regenerates README.md from priv/partials/readme_content.md and updates stats.
  """
  use Mix.Task

  @impl Mix.Task
  def run(_args) do
    Mix.shell().info("Regenerating README.md from partials...")
    File.cp!("priv/partials/readme_content.md", "README.md")
    
    Mix.shell().info("Updating stats in README.md...")
    System.cmd("python3", ["scripts/gen_wiki.py"])
    
    Mix.shell().info("README.md has been rewritten and stats updated.")
  end
end
