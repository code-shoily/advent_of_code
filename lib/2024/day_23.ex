defmodule AdventOfCode.Y2024.Day23 do
  @moduledoc """
  --- Day 23: LAN Party ---
  Problem Link: https://adventofcode.com/2024/day/23
  Difficulty: m
  Tags: graph clique bron-kerbosch maximum-clique LAN-party
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Property.Clique

  def input, do: InputReader.read_from_file(2024, 23)

  def run(input \\ input()) do
    graph = parse(input)

    p1 = solve_p1(graph)
    p2 = solve_p2(graph)

    {p1, p2}
  end

  defp solve_p1(graph) do
    Clique.k_cliques(graph, 3)
    |> Enum.count(fn clique ->
      clique |> Enum.any?(&String.starts_with?(&1, "t"))
    end)
  end

  defp solve_p2(graph) do
    Clique.max_clique(graph)
    |> MapSet.to_list()
    |> Enum.sort()
    |> Enum.join(",")
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.reduce(Yog.undirected(), fn line, graph ->
      [u, v] = String.split(line, "-")
      Yog.add_edge_ensure(graph, u, v, 1)
    end)
  end
end
