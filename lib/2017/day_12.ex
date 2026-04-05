defmodule AdventOfCode.Y2017.Day12 do
  @moduledoc """
  --- Day 12: Digital Plumber ---
  Problem Link: https://adventofcode.com/2017/day/12
  Difficulty: s
  Tags: connectivity graph
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Connectivity
  alias Yog.Traversal

  def input, do: InputReader.read_from_file(2017, 12)

  def run(input \\ input()) do
    graph = parse(input)

    {run_1(graph), run_2(graph)}
  end

  defp run_1(graph) do
    # Size of the connected component containing node 0
    graph
    |> Traversal.walk(0, :breadth_first)
    |> Enum.count()
  end

  defp run_2(graph) do
    # Number of connected components in the graph
    graph
    |> Connectivity.connected_components()
    |> Enum.count()
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.reduce(Yog.undirected(), fn line, graph ->
      [node, connections] = line |> String.split(" <-> ")
      u = String.to_integer(node)
      dests = Transformers.int_words(connections, ", ")

      Enum.reduce(dests, graph, fn v, acc ->
        Yog.add_edge_ensure(acc, u, v, 1, nil)
      end)
    end)
  end
end
