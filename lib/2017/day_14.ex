defmodule AdventOfCode.Y2017.Day14 do
  @moduledoc """
  --- Day 14: Disk Defragmentation ---
  Problem Link: https://adventofcode.com/2017/day/14

  TODO: Look into strongly connected components to get the regions.
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 14

  alias AdventOfCode.Helpers.Transformers
  alias AdventOfCode.Y2017.Day10

  def run(input \\ input!()) do
    squares = input |> parse() |> squares()

    {run_1(squares), run_2(squares)}
  end

  def parse(data \\ input!()), do: hd(Transformers.lines(data))

  defp run_1(squares) do
    squares
    |> Enum.map(&Enum.count(&1, fn i -> i == "1" end))
    |> Enum.sum()
  end

  defp run_2(_squares) do
    {:todo, 2}
  end

  defp squares(input) do
    0..127
    |> Enum.map(fn key ->
      "#{input}-#{key}"
      |> Day10.compute_knot_hash()
      |> to_bits()
    end)
  end

  defp to_bits(hash) do
    hash
    |> String.graphemes()
    |> Enum.map_join(fn digit ->
      digit
      |> String.to_integer(16)
      |> Integer.to_string(2)
      |> String.pad_leading(4, "0")
    end)
    |> String.graphemes()
  end
end
