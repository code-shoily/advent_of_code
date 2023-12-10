defmodule AdventOfCode.Y2023.Day10 do
  @moduledoc """
  --- Day 10: Pipe Maze ---
  Problem Link: https://adventofcode.com/2023/day/10
  Difficulty: xl
  Tags: graph graph-traversal needs-improvement not-fast-enough
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias AdventOfCode.Algorithms.Grid
  alias Geo.Polygon

  import Topo, only: [contains?: 2]

  def input, do: InputReader.read_from_file(2023, 10)

  def run(input \\ input()) do
    input = parse(input)

    task_1 = Task.async(fn -> run_1(input) end)
    task_2 = Task.async(fn -> run_2(input) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  defp run_1({_, circuit}) do
    circuit |> Enum.count() |> div(2)
  end

  defp run_2({graph, circuit}) do
    graph
    |> Graph.vertices()
    |> Stream.reject(&(&1 in circuit))
    |> Stream.map(&contains?(%Polygon{coordinates: [circuit]}, &1))
    |> Enum.count(&Function.identity/1)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
    |> Grid.grid2d()
    |> then(fn grid ->
      {start, _} = Enum.find(grid, fn {_, tile} -> tile == "S" end)
      graph = to_graph(grid)
      circuit = circuit_nodes(graph, start)
      {graph, circuit}
    end)
  end

  defp to_graph(grid) do
    for {{x, y}, tile} <- grid, reduce: Graph.new() do
      acc ->
        case connected({x, y}, tile) do
          {n1, n2} -> acc |> Graph.add_edge({x, y}, n1) |> Graph.add_edge({x, y}, n2)
          nil -> acc
        end
    end
  end

  defp connected({x, y}, "|"), do: {{x - 1, y}, {x + 1, y}}
  defp connected({x, y}, "-"), do: {{x, y - 1}, {x, y + 1}}
  defp connected({x, y}, "L"), do: {{x - 1, y}, {x, y + 1}}
  defp connected({x, y}, "J"), do: {{x - 1, y}, {x, y - 1}}
  defp connected({x, y}, "7"), do: {{x, y - 1}, {x + 1, y}}
  defp connected({x, y}, "F"), do: {{x, y + 1}, {x + 1, y}}
  defp connected(_, _), do: nil

  defp circuit_nodes(graph, start) do
    graph
    |> Graph.in_neighbors(start)
    |> Enum.reduce(graph, &Graph.add_edge(&2, start, &1))
    |> Graph.reachable_neighbors([start])
  end
end
