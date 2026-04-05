defmodule AdventOfCode.Y2018.Day25 do
  @moduledoc """
  --- Day 25: Four-Dimensional Adventure ---
  Problem Link: https://adventofcode.com/2018/day/25
  Difficulty: m
  Tags: connectivity constellation graph
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Connectivity

  def input, do: InputReader.read_from_file(2018, 25)

  def run(input \\ input()) do
    points = parse(input)

    {run_1(points), run_2(points)}
  end

  defp run_1(points) do
    # Points form "constellations" if their Manhattan distance is <= 3.
    # This is exactly finding connected components in a graph where
    # nodes are points and edges connect points with distance <= 3.
    graph = build_graph(points)

    Connectivity.connected_components(graph)
    |> length()
  end

  defp run_2(_points) do
    "Finished the 4D adventure! 🥳"
  end

  defp build_graph(points_with_index) do
    # Each points has an index for stable node ID.
    points =
      points_with_index
      |> Enum.with_index()

    # Create the undirected graph with nodes.
    initial_graph =
      Enum.reduce(points, Yog.undirected(), fn {p, idx}, acc ->
        Yog.add_node(acc, idx, p)
      end)

    # Consider all pairs of points and add edges if they are connected.
    # O(N^2) checks. N ~ 1250 => ~780k checks.
    # We only check each pair once.
    add_edges(points, initial_graph)
  end

  defp add_edges([], graph), do: graph

  defp add_edges([{p1, i1} | rest], graph) do
    new_graph =
      Enum.reduce(rest, graph, fn {p2, i2}, acc ->
        if manhattan_dist(p1, p2) <= 3 do
          # Graph is undirected, so order doesn't matter.
          # add_edge! ensures we don't handle error tuples inside the loop.
          Yog.add_edge!(acc, from: i1, to: i2, with: 1)
        else
          acc
        end
      end)

    add_edges(rest, new_graph)
  end

  defp manhattan_dist({x1, y1, z1, w1}, {x2, y2, z2, w2}) do
    abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2) + abs(w1 - w2)
  end

  def parse(data \\ input()) do
    for line <- Transformers.lines(data) do
      line
      |> String.split(",")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end
  end
end
