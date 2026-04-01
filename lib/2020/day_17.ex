defmodule AdventOfCode.Y2020.Day17 do
  @moduledoc """
  --- Day 17: Conway Cubes ---
  Problem Link: https://adventofcode.com/2020/day/17
  Difficulty: m
  Tags: simulation game-of-life infinite-grid
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 17)

  def run(input \\ input()) do
    initial = parse(input)
    {solve(initial, 3), solve(initial, 4)}
  end

  defp solve(initial, dims) do
    state =
      initial
      |> Enum.map(fn {x, y} ->
        if dims == 3, do: {x, y, 0}, else: {x, y, 0, 0}
      end)
      |> MapSet.new()

    Enum.reduce(1..6, state, fn _, acc -> step(acc, dims) end)
    |> MapSet.size()
  end

  defp step(active, dims) do
    neighbor_counts =
      active
      |> Enum.reduce(%{}, fn pos, counts ->
        get_neighbors(pos, dims)
        |> Enum.reduce(counts, fn n, acc -> Map.update(acc, n, 1, &(&1 + 1)) end)
      end)

    for {pos, count} <- neighbor_counts,
        count == 3 or (count == 2 and MapSet.member?(active, pos)),
        into: MapSet.new() do
      pos
    end
  end

  defp get_neighbors({x, y, z}, 3) do
    for dx <- -1..1, dy <- -1..1, dz <- -1..1, {dx, dy, dz} != {0, 0, 0} do
      {x + dx, y + dy, z + dz}
    end
  end

  defp get_neighbors({x, y, z, w}, 4) do
    for dx <- -1..1, dy <- -1..1, dz <- -1..1, dw <- -1..1, {dx, dy, dz, dw} != {0, 0, 0, 0} do
      {x + dx, y + dy, z + dz, w + dw}
    end
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> char == "#" end)
      |> Enum.map(fn {_, x} -> {x, y} end)
    end)
  end
end
