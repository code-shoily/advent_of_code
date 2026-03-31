defmodule AdventOfCode.Y2025.Day08 do
  @moduledoc """
  --- Day 8: Playground ---
  Problem Link: https://adventofcode.com/2025/day/8
  Difficulty: s
  Tags: union-find
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.DisjointSet

  def input, do: InputReader.read_from_file(2025, 8)

  def run(input \\ input()) do
    coords = parse(input)
    edges = build_sorted_edges(coords)

    {run_1(coords, edges), run_2(coords, edges)}
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end)
    |> List.to_tuple()
  end

  def build_sorted_edges(coords) do
    n = tuple_size(coords)

    for i <- 0..(n - 2),
        j <- (i + 1)..(n - 1) do
      {i, j, squared_dist(elem(coords, i), elem(coords, j))}
    end
    |> Enum.sort_by(fn {_, _, d} -> d end)
  end

  defp squared_dist({x1, y1, z1}, {x2, y2, z2}) do
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2
    dx * dx + dy * dy + dz * dz
  end

  def run_1(coords, edges \\ nil) do
    edges = edges || build_sorted_edges(coords)
    n = tuple_size(coords)
    dsu = Enum.reduce(0..(n - 1), DisjointSet.new(), fn i, acc -> DisjointSet.add(acc, i) end)

    edges
    |> Enum.take(1000)
    |> Enum.reduce(dsu, fn {i, j, _}, acc ->
      DisjointSet.union(acc, i, j)
    end)
    |> DisjointSet.to_lists()
    |> Enum.map(&length/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end

  def run_2(coords, edges \\ nil) do
    edges = edges || build_sorted_edges(coords)
    n = tuple_size(coords)
    dsu = Enum.reduce(0..(n - 1), DisjointSet.new(), fn i, acc -> DisjointSet.add(acc, i) end)

    find_last_edge(edges, dsu, n, coords)
  end

  defp find_last_edge([{i, j, _} | rest], dsu, num_circuits, coords) do
    {dsu1, root_i} = DisjointSet.find(dsu, i)
    {dsu2, root_j} = DisjointSet.find(dsu1, j)

    if root_i != root_j do
      if num_circuits == 2 do
        {x1, _, _} = elem(coords, i)
        {x2, _, _} = elem(coords, j)
        x1 * x2
      else
        dsu3 = DisjointSet.union(dsu2, i, j)
        find_last_edge(rest, dsu3, num_circuits - 1, coords)
      end
    else
      find_last_edge(rest, dsu2, num_circuits, coords)
    end
  end
end
