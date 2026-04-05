defmodule AdventOfCode.Y2016.Day24 do
  @moduledoc """
  --- Day 24: Air Duct Spelunking ---
  Problem Link: https://adventofcode.com/2016/day/24
  Difficulty: m
  Tags: graph grid matrix pathfinding tsp
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Grid
  alias Yog.Pathfinding.Matrix

  def input, do: InputReader.read_from_file(2016, 24)

  def run(input \\ input()) do
    # Build the graph and identify POIs (0-7).
    {graph, pois} = parse(input)

    # Pre-calculate distances between all POIs using the Matrix API.
    # It intelligently selects Dijkstra or Floyd-Warshall.
    poi_ids = Map.values(pois)
    {:ok, dist_matrix} = Matrix.distance_matrix(graph, poi_ids)

    # Simplified label-based distance map.
    label_to_id = Map.new(pois, fn {label, id} -> {id, label} end)

    distances =
      Map.new(dist_matrix, fn {{u, v}, d} ->
        {{label_to_id[u], label_to_id[v]}, d}
      end)

    max_poi = Map.keys(pois) |> Enum.max()
    targets = Enum.to_list(1..max_poi)

    {solve_tsp(targets, distances, false), solve_tsp(targets, distances, true)}
  end

  def parse(data \\ input()) do
    # Grid builder creates an undirected graph and identifies walls.
    grid_data = Transformers.lines(data) |> Enum.map(&String.graphemes/1)
    grid = Grid.from_2d_list(grid_data, :undirected, Grid.avoiding("#"))
    graph = Grid.to_graph(grid)

    # Extract positions of numbered POIs.
    pois =
      Enum.reduce(graph.nodes, %{}, fn {id, data}, acc ->
        case Integer.parse(data) do
          {label, ""} -> Map.put(acc, label, id)
          _ -> acc
        end
      end)

    {graph, pois}
  end

  defp solve_tsp(targets, distances, return_to_start?) do
    targets
    |> permutations()
    |> Enum.map(fn p ->
      path = if return_to_start?, do: [0 | p] ++ [0], else: [0 | p]
      calculate_path_dist(path, distances)
    end)
    |> Enum.min()
  end

  defp calculate_path_dist(path, distances) do
    path
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [a, b] -> distances[{a, b}] end)
    |> Enum.sum()
  end

  defp permutations([]), do: [[]]

  defp permutations(list) do
    for x <- list, rest <- permutations(list -- [x]), do: [x | rest]
  end
end
