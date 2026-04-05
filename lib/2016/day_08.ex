defmodule AdventOfCode.Y2016.Day08 do
  @moduledoc """
  --- Day 8: Two-Factor Authentication ---
  Problem Link: https://adventofcode.com/2016/day/8
  Difficulty: m
  Tags: matrix op-code visual-result
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @on "█"
  @off " "
  @width 50
  @height 6

  def input, do: InputReader.read_from_file(2016, 8)

  def run(input \\ input()) do
    final_grid =
      input
      |> parse()
      |> Enum.reduce(%{}, &operation/2)

    {map_size(final_grid), display(final_grid)}
  end

  defp parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(fn line ->
      case String.split(line, [" ", "=", "x"], trim: true) do
        ["rect", w, h] -> {:rect, to_i(w), to_i(h)}
        ["rotate", "row", "y", y, "by", by] -> {:row, to_i(y), to_i(by)}
        ["rotate", "column", x, "by", by] -> {:col, to_i(x), to_i(by)}
      end
    end)
  end

  defp operation({:rect, w, h}, grid) do
    for x <- 0..(w - 1), y <- 0..(h - 1), into: grid, do: {{x, y}, @on}
  end

  defp operation({:row, y, by}, grid) do
    keys = for x <- 0..(@width - 1), do: {x, y}

    for x <- 0..(@width - 1), grid[{x, y}] == @on, into: Map.drop(grid, keys) do
      {{rem(x + by, @width), y}, @on}
    end
  end

  defp operation({:col, x, by}, grid) do
    keys = for y <- 0..(@height - 1), do: {x, y}

    for y <- 0..(@height - 1), grid[{x, y}] == @on, into: Map.drop(grid, keys) do
      {{x, rem(y + by, @height)}, @on}
    end
  end

  defp display(grid) do
    # credo:disable-for-next-line
    IO.puts("")

    for y <- 0..(@height - 1) do
      for(x <- 0..(@width - 1), do: Map.get(grid, {x, y}, @off))
      |> Enum.join()
      # credo:disable-for-next-line
      |> IO.puts()
    end

    :ok
  end

  defp to_i(v), do: String.to_integer(v)
end
