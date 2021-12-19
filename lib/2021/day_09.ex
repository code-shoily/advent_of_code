defmodule AdventOfCode.Y2021.Day09 do
  @moduledoc """
  --- Day 9: Smoke Basin ---
  Problem Link: https://adventofcode.com/2021/day/9
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 9

  def run_1, do: input!() |> parse() |> risk_point()

  def run_2 do
    input!()
    |> parse()
    |> Map.filter(fn {_, w} -> w != 9 end)
    |> make_graph()
    |> basin_multiplier()
  end

  def parse(data) do
    data
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, x} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {value, y} -> {{x, y}, String.to_integer(value)} end)
    end)
    |> Map.new()
  end

  defp risk_point(map) do
    Enum.reduce(
      map,
      [],
      fn {{x, y}, weight}, lowest_list ->
        {x, y}
        |> adjacent_points()
        |> Enum.reduce(true, &(&2 && map[&1] > weight))
        |> then(&((&1 && [weight + 1 | lowest_list]) || lowest_list))
      end
    )
    |> Enum.sum()
  end

  defp make_graph(map) do
    Enum.reduce(
      map,
      Graph.add_vertices(%Graph{}, Map.keys(map)),
      fn {{x, y} = vertex, weight}, g_1 ->
        {x, y}
        |> adjacent_points()
        |> Enum.reduce(g_1, fn {new_x, new_y}, g_2 ->
          Graph.add_edge(g_2, vertex, {new_x, new_y}, weight: weight)
        end)
      end
    )
  end

  defp adjacent_points({x, y}) do
    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
    |> Enum.filter(fn {x, y} -> x >= 0 and y >= 0 and (x <= 99 and y <= 99) end)
  end

  defp basin_multiplier(graph) do
    graph
    |> Graph.strong_components()
    |> Enum.map(&length/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.reduce(&Kernel.*/2)
  end
end
