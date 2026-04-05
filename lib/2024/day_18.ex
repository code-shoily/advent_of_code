defmodule AdventOfCode.Y2024.Day18 do
  @moduledoc """
  --- Day 18: RAM Run ---
  Problem Link: https://adventofcode.com/2024/day/18
  Difficulty: m
  Tags: binary-search dijkstra graph grid
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Pathfinding.Dijkstra

  # Constants for the large grid
  @size 70
  @part1_bytes 1024

  def input, do: InputReader.read_from_file(2024, 18)

  def run(input \\ input()) do
    coords = parse(input)

    p1 = solve_p1(coords)
    p2 = solve_p2(coords)

    {p1, p2}
  end

  defp solve_p1(coords) do
    corrupted = coords |> Enum.take(@part1_bytes) |> MapSet.new()
    find_path(corrupted)
  end

  defp solve_p2(coords) do
    # Binary search for the first byte that blocks the path
    low = @part1_bytes
    high = length(coords) - 1

    idx = binary_search(coords, low, high)
    {x, y} = Enum.at(coords, idx)
    "#{x},#{y}"
  end

  defp binary_search(_coords, low, high) when low >= high, do: low

  defp binary_search(coords, low, high) do
    mid = div(low + high, 2)
    corrupted = coords |> Enum.take(mid + 1) |> MapSet.new()

    if find_path(corrupted) == :failed do
      # Path blocked at mid or before
      binary_search(coords, low, mid)
    else
      # Path still exists after mid
      binary_search(coords, mid + 1, high)
    end
  end

  defp find_path(corrupted) do
    case Dijkstra.implicit_dijkstra_by(
           from: {0, 0},
           successors_with_cost: fn {x, y} ->
             neighbors(x, y)
             |> Enum.filter(fn {nx, ny} ->
               nx >= 0 and nx <= @size and ny >= 0 and ny <= @size and
                 not MapSet.member?(corrupted, {nx, ny})
             end)
             |> Enum.map(fn pos -> {pos, 1} end)
           end,
           is_goal: fn pos -> pos == {@size, @size} end,
           visited_by: fn pos -> pos end
         ) do
      {:ok, dist} -> dist
      _ -> :failed
    end
  end

  defp neighbors(x, y), do: [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [x, y] = String.split(line, ",") |> Enum.map(&String.to_integer/1)
      {x, y}
    end)
  end
end
