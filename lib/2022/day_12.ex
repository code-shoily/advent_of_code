defmodule AdventOfCode.Y2022.Day12 do
  @moduledoc """
  --- Day 12: Hill Climbing Algorithm ---
  Problem Link: https://adventofcode.com/2022/day/12
  Difficulty: m
  Tags: graph graph-traversal slow needs-improvement
  """
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 12)

  def run(input \\ input()) do
    {map, source, destination} = parse(input)
    lowest_points = Enum.map(Enum.filter(map, fn {_, v} -> v == ?a end), &elem(&1, 0))

    graph = to_digraph(map)
    solve_1 = Task.async(fn -> run_1(graph, source, destination) end)
    solve_2 = Task.async(fn -> run_2(graph, lowest_points, destination) end)

    {Task.await(solve_1, :infinity), Task.await(solve_2, :infinity)}
  end

  def parse(data \\ input()) do
    map =
      data
      |> Transformers.lines()
      |> Enum.map(&String.graphemes/1)
      |> Grid.grid2d(fn char -> :binary.first(char) end)

    source = elem(Enum.find(map, fn {_, v} -> v == ?S end), 0)
    destination = elem(Enum.find(map, fn {_, v} -> v == ?E end), 0)
    map = %{map | source => ?a, destination => ?z}

    {map, source, destination}
  end

  defp run_1(graph, source, destination),
    do: length(:digraph.get_short_path(graph, source, destination)) - 1

  defp run_2(graph, sources, destination) do
    source_map = sources |> Enum.with_index() |> Map.new(fn {k, _} -> {k, :empty} end)

    sources
    |> Enum.reduce(source_map, fn {_, _} = point, acc ->
      case source_map[point] do
        :empty ->
          get_and_update_shortest_path(graph, point, destination, acc)

        _ ->
          acc
      end
    end)
    |> Enum.min_by(fn {_, v} -> v end)
    |> elem(1)
  end

  defp to_digraph(grid) do
    graph = :digraph.new()

    Enum.reduce(grid, graph, fn {{x, y}, _}, _ ->
      [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
      |> Enum.filter(&Map.has_key?(grid, &1))
      |> Enum.map(&add_edge(&1, grid, x, y))
      |> Enum.reject(&is_nil/1)
      |> Enum.each(fn {v1, v2} ->
        :digraph.add_vertex(graph, v1)
        :digraph.add_vertex(graph, v2)
        :digraph.add_edge(graph, v1, v2)
      end)

      graph
    end)
  end

  defp get_and_update_shortest_path(graph, point, destination, paths) do
    case :digraph.get_short_path(graph, point, destination) do
      false ->
        paths

      path ->
        update_path(path, paths)
    end
  end

  defp update_path(path, state) do
    path
    |> Enum.with_index(1)
    |> Enum.reduce(state, fn {{_, _} = x, p}, acc ->
      case Map.get(acc, x) do
        :empty -> Map.put(acc, x, length(path) - p)
        _ -> acc
      end
    end)
  end

  defp add_edge(point, grid, x, y) do
    unless grid[point] - grid[{x, y}] > 1 do
      {{x, y}, point}
    end
  end
end
