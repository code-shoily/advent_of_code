defmodule AdventOfCode.Y2020.Day24 do
  @moduledoc """
  --- Day 24: Lobby Layout ---
  Problem Link: https://adventofcode.com/2020/day/24
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  require Integer

  def input, do: InputReader.read_from_file(2020, 24)

  def run(input \\ input()) do
    input = parse(input)
    {run_1(input), {:todo, 2}}
  end

  def run_1(input) do
    input
    |> flip()
    |> black_tiles()
    |> length()
  end

  @directions ~r/e|w|se|sw|ne|nw/
  def parse(input) do
    for line <- Transformers.lines(input) do
      @directions
      |> Regex.scan(line, capture: :first)
      |> List.flatten()
    end
  end

  defp walk("w", {x, y}), do: {x - 1, y}
  defp walk("e", {x, y}), do: {x + 1, y}
  defp walk("nw", {x, y}), do: {(Integer.is_even(y) && x) || x - 1, y - 1}
  defp walk("ne", {x, y}), do: {(Integer.is_even(y) && x + 1) || x, y - 1}
  defp walk("sw", {x, y}), do: {(Integer.is_even(y) && x) || x - 1, y + 1}
  defp walk("se", {x, y}), do: {(Integer.is_even(y) && x + 1) || x, y + 1}

  defp flip(tiles), do: Enum.map(tiles, fn sides -> Enum.reduce(sides, {0, 0}, &walk/2) end)

  defp black_tiles(tiles) do
    for {_, tile} <- Enum.frequencies(tiles), rem(tile, 2) == 1, do: tile
  end
end
