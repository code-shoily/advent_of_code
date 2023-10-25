defmodule AdventOfCode.Y2021.Day15 do
  @moduledoc """
  --- Day 15: Chiton ---
  Problem Link: https://adventofcode.com/2021/day/15
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2021, 15)

  def run(input \\ input()) do
    input = parse(input)

    task_1 = Task.async(fn -> run_1(input) end)
    task_2 = Task.async(fn -> run_2(input) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def run_1(grid) do
    grid
    |> to_graph()
    |> Graph.dijkstra({0, 0}, get_destination(grid))
    |> tl()
    |> Enum.map(&Map.get(grid, &1))
    |> Enum.sum()
  end

  def run_2(grid) do
    grid = multiply(grid)

    grid
    |> to_graph()
    |> Graph.dijkstra({0, 0}, get_destination(grid))
    |> tl()
    |> Enum.map(&Map.get(grid, &1))
    |> Enum.sum()
  end

  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {column, x} -> {{x, y}, String.to_integer(column)} end)
    end)
    |> Map.new()
  end

  def to_graph(grid) do
    graph =
      Enum.reduce(grid, Graph.new(), fn {{x, y}, _}, graph ->
        nbs =
          [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
          |> Enum.filter(&Map.has_key?(grid, &1))
          |> Enum.map(fn point -> {{x, y}, point, [weight: Map.get(grid, point)]} end)

        Graph.add_edges(graph, nbs)
      end)

    invalid_edges =
      graph
      |> Graph.edges()
      |> Enum.filter(fn %Graph.Edge{v1: {x1, y1}, v2: {x2, y2}} ->
        abs(x1 - x2) > 1 and abs(y1 - y2) > 1
      end)

    Graph.delete_edges(graph, invalid_edges)
  end

  defp multiply(grid) do
    {max_x, max_y} = get_destination(grid)

    grid =
      grid
      |> Enum.reduce(grid, fn {{x, y}, risk}, grid ->
        Enum.reduce(1..4, grid, fn index, grid ->
          Map.put(grid, {x + index * (max_x + 1), y}, increment(risk + index))
        end)
      end)

    Enum.reduce(grid, grid, fn {{x, y}, risk}, grid ->
      Enum.reduce(1..4, grid, fn index, grid ->
        Map.put(grid, {x, y + index * (max_y + 1)}, increment(risk + index))
      end)
    end)
  end

  defp increment(weight), do: (weight > 9 && weight - 9) || weight
  defp get_destination(grid), do: grid |> Map.keys() |> Enum.max()
end
