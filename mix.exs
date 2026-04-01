defmodule AdventOfCode.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent_of_code,
      version: "0.21.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      elixirc_options: [warnings_as_errors: true]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:eex, :logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:floki, "~> 0.36", only: [:dev]},
      {:httpoison, "~> 2.2", only: [:dev]},
      {:yog_ex, github: "code-shoily/yog_ex", branch: "main"}
    ]
  end

  defp aliases do
    [
      update_stats: ["cmd python3 scripts/gen_wiki.py"]
    ]
  end
end
