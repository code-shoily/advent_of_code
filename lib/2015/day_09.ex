defmodule AdventOfCode.Y2015.Day09 do
  @moduledoc """
  --- Day 9: All in a Single Night ---
  Problem Link: https://adventofcode.com/2015/day/9
  Difficulty: s
  Tags: graph routing
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Labeled

  def input, do: InputReader.read_from_file(2015, 9)

  def run(input \\ input()) do
    builder = parse(input)
    graph = Labeled.to_graph(builder)
    node_ids = Yog.node_ids(graph)

    # Pre-generate adjacency map for fast weight lookup
    adj =
      Yog.all_edges(graph)
      |> Enum.flat_map(fn {u, v, w} -> [{{u, v}, w}, {{v, u}, w}] end)
      |> Map.new()

    # AOC Day 9 has a small number of cities (8), making permutations feasible.
    distances =
      node_ids
      |> permutations()
      |> Enum.map(&path_distance(&1, adj))
      |> Enum.reject(&is_nil/1)

    {Enum.min(distances), Enum.max(distances)}
  end

  def parse(data) do
    Transformers.lines(data)
    |> Enum.reduce(Labeled.undirected(), fn line, acc ->
      [_, from, to, dist] = Regex.run(~r/(\w+) to (\w+) = (\d+)/, line)
      Labeled.add_edge(acc, from, to, String.to_integer(dist))
    end)
  end

  # Calculate the total distance of a specific path (list of node IDs)
  defp path_distance([_], _), do: 0

  defp path_distance([u, v | rest], adj) do
    case Map.get(adj, {u, v}) do
      nil ->
        nil

      weight ->
        case path_distance([v | rest], adj) do
          nil -> nil
          rest_dist -> weight + rest_dist
        end
    end
  end

  # Standard permutations generator
  defp permutations([]), do: [[]]

  defp permutations(list) do
    for x <- list, rest <- permutations(list -- [x]), do: [x | rest]
  end
end
