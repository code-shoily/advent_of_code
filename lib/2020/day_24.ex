defmodule AdventOfCode.Y2020.Day24 do
  @moduledoc """
  --- Day 24: Lobby Layout ---
  Problem Link: https://adventofcode.com/2020/day/24
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 24
  require Integer

  def run_1 do
    input()
    |> process()
    |> flip()
    |> black_tiles()
    |> length()
  end

  def run_2 do
    {:not_implemented, 2}
  end

  @directions ~r/e|w|se|sw|ne|nw/
  def process(input) do
    for line <- String.split(input, "\n") do
      @directions
      |> Regex.scan(line, capture: :first)
      |> List.flatten()
    end
  end

  defp walk("w", {x, y}), do: {x - 1, y}
  defp walk("e", {x, y}), do: {x + 1, y}
  defp walk("nw", {x, y}), do: {Integer.is_even(y) && x || x - 1, y - 1}
  defp walk("ne", {x, y}), do: {Integer.is_even(y) && x + 1 || x, y - 1}
  defp walk("sw", {x, y}), do: {Integer.is_even(y) && x || x - 1, y + 1}
  defp walk("se", {x, y}), do: {Integer.is_even(y) && x + 1 || x, y + 1}

  defp flip(tiles), do: Enum.map(tiles, fn sides -> Enum.reduce(sides, {0, 0}, &walk/2) end)

  defp black_tiles(tiles) do
    for {_, tile} <- Enum.frequencies(tiles), rem(tile, 2) == 1, do: tile
  end

  defp input do
    input!()
  end
end
