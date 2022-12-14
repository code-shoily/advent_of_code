defmodule AdventOfCode.Y2022.Day12 do
  @moduledoc """
  --- Day 12: Hill Climbing Algorithm ---
  Problem Link: https://adventofcode.com/2022/day/12
  """
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

  defp run_1(graph, source, destination),
    do: length(:digraph.get_short_path(graph, source, destination)) - 1

  defp run_2(graph, sources, destination) do
    source_map = sources |> Enum.with_index() |> Map.new(fn {k, _} -> {k, :empty} end)

    sources
    |> Enum.reduce(source_map, fn {_, _} = point, acc ->
      case source_map[point] do
        :empty ->
          case :digraph.get_short_path(graph, point, destination) do
            false ->
              acc

            path ->
              len = length(path)

              path
              |> Enum.with_index(1)
              |> Enum.reduce(acc, fn {{_, _} = x, p}, acc2 ->
                case Map.get(acc2, x) do
                  :empty -> Map.put(acc2, x, len - p)
                  _ -> acc2
                end
              end)
          end

        _ ->
          acc
      end
    end)
    |> Enum.min_by(fn {_, v} -> v end)
    |> elem(1)
  end

  def parse(data \\ input()) do
    map =
      data
      |> Transformers.lines()
      |> Enum.map(&String.graphemes/1)
      |> Transformers.grid2d(fn char -> :binary.first(char) end)

    source = elem(Enum.find(map, fn {_, v} -> v == ?S end), 0)
    destination = elem(Enum.find(map, fn {_, v} -> v == ?E end), 0)
    map = %{map | source => ?a, destination => ?z}

    {map, source, destination}
  end

  def to_digraph(grid) do
    graph = :digraph.new()

    Enum.reduce(grid, graph, fn {{x, y}, _}, _ ->
      [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
      |> Enum.filter(&Map.has_key?(grid, &1))
      |> Enum.map(fn point ->
        unless grid[point] - grid[{x, y}] > 1 do
          {{x, y}, point}
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.each(fn {v1, v2} ->
        :digraph.add_vertex(graph, v1)
        :digraph.add_vertex(graph, v2)
        :digraph.add_edge(graph, v1, v2)
      end)
    end)

    graph
  end
end
