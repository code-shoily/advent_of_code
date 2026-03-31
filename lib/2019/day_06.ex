defmodule AdventOfCode.Y2019.Day06 do
  @moduledoc """
  --- Day 6: Universal Orbit Map ---
  Problem Link: https://adventofcode.com/2019/day/6
  Difficulty: xs
  Tags: graph routing
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Labeled
  alias Yog.Pathfinding.Dijkstra

  def input, do: InputReader.read_from_file(2019, 6)

  def run(input \\ input()) do
    lines = Transformers.lines(input)
    {run_1(lines), run_2(lines)}
  end

  def run_1(lines) do
    builder = build_graph(lines, Labeled.directed())
    {:ok, com_id} = Labeled.get_id(builder, "COM")
    graph = Labeled.to_graph(builder)

    # Recursive total orbit count: each node contributes its depth (distance from COM)
    count_total_orbits(graph, com_id, 0) - 0
  end

  def run_2(lines) do
    builder = build_graph(lines, Labeled.undirected())
    {:ok, you_id} = Labeled.get_id(builder, "YOU")
    {:ok, san_id} = Labeled.get_id(builder, "SAN")
    graph = Labeled.to_graph(builder)

    # Shortest path between YOU and SANTA.
    # Transfers needed = number of edges between the objects they ORBIT.
    # Path: YOU -> A -> ... -> B -> SAN has weight = (number of edges).
    # Transfers = (Path weight) - 2.
    {:ok, path} = Dijkstra.shortest_path(graph, you_id, san_id)
    path.weight - 2
  end

  defp build_graph(lines, initial_builder) do
    Enum.reduce(lines, initial_builder, fn line, builder ->
      [center, orbiter] = String.split(line, ")")
      Labeled.add_simple_edge(builder, center, orbiter)
    end)
  end

  defp count_total_orbits(graph, node_id, depth) do
    # Successors of the center are those that ORBIT it.
    successors = Yog.successors(graph, node_id)

    Enum.reduce(successors, depth, fn {child_id, _}, acc ->
      acc + count_total_orbits(graph, child_id, depth + 1)
    end)
  end
end
