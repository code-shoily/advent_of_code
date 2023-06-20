defmodule AdventOfCode.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent_of_code,
      version: "0.20.0",
      elixir: "~> 1.15.0",
      start_permanent: Mix.env() == :prod,
      deps: deps()
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
      {:floki, "~> 0.34.0"},
      {:httpoison, "~> 2.1.0"},
      {:jason, "~> 1.4"},
      {:libgraph, "~> 0.16"},
      {:aja, "~> 0.6.1"}
    ]
  end
end
