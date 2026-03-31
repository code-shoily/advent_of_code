defmodule AdventOfCode.Y2020.Day24 do
  @moduledoc """
  --- Day 24: Lobby Layout ---
  Problem Link: https://adventofcode.com/2020/day/24
  Difficulty: m
  Tags: hexagon walk
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  require Integer

  def input, do: InputReader.read_from_file(2020, 24)

  def run(input \\ input()) do
    input = parse(input)
    black_tiles = run_1(input)

    {
      MapSet.size(black_tiles),
      run_2(black_tiles)
    }
  end

  def run_1(steps) do
    steps
    |> Enum.map(fn sides -> Enum.reduce(sides, {0, 0}, &walk/2) end)
    |> Enum.frequencies()
    |> Enum.filter(fn {_, count} -> rem(count, 2) == 1 end)
    |> MapSet.new(fn {tile, _} -> tile end)
  end

  def run_2(black_tiles) do
    1..100
    |> Enum.reduce(black_tiles, fn _day, tiles -> simulate(tiles) end)
    |> MapSet.size()
  end

  defp simulate(black_tiles) do
    all_to_check =
      black_tiles
      |> Enum.flat_map(fn pos -> [pos | hex_neighbors(pos)] end)
      |> MapSet.new()

    # 2. Apply rules
    all_to_check
    |> Enum.reduce(MapSet.new(), fn pos, acc ->
      black_neighbor_count =
        hex_neighbors(pos)
        |> Enum.count(&MapSet.member?(black_tiles, &1))

      is_black = MapSet.member?(black_tiles, pos)

      should_be_black =
        if is_black do
          black_neighbor_count == 1 or black_neighbor_count == 2
        else
          black_neighbor_count == 2
        end

      if should_be_black, do: MapSet.put(acc, pos), else: acc
    end)
  end

  defp hex_neighbors(pos) do
    ["e", "w", "ne", "nw", "se", "sw"] |> Enum.map(&walk(&1, pos))
  end

  @directions ~r/e|w|se|sw|ne|nw/
  def parse(input) do
    for line <- Transformers.lines(input) do
      @directions
      |> Regex.scan(line)
      |> List.flatten()
    end
  end

  defp walk("w", {x, y}), do: {x - 1, y}
  defp walk("e", {x, y}), do: {x + 1, y}
  defp walk("nw", {x, y}), do: {(Integer.is_even(y) && x) || x - 1, y - 1}
  defp walk("ne", {x, y}), do: {(Integer.is_even(y) && x + 1) || x, y - 1}
  defp walk("sw", {x, y}), do: {(Integer.is_even(y) && x) || x - 1, y + 1}
  defp walk("se", {x, y}), do: {(Integer.is_even(y) && x + 1) || x, y + 1}
end
