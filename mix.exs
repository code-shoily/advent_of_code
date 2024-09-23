defmodule AdventOfCode.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent_of_code,
      version: "0.21.0",
      elixir: "~> 1.17",
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
      {:credo, "~> 1.6", only: [:dev, :test], runtime: false},
      {:floki, "~> 0.36"},
      {:httpoison, "~> 2.2.0"},
      {:jason, "~> 1.4"},
      {:libgraph, "~> 0.16"},
      {:aja, "~> 0.7"},
      {:topo, "~> 1.0"},
      {:rustler, "~> 0.34.0"}
    ]
  end

  defp aliases do
    [
      update_stats: ["gen_stats", "gen_readme", "gen_tags", "gen_difficulties"]
    ]
  end
end
