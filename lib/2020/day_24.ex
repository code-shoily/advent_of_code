defmodule AdventOfCode.Y2020.Day24 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/24
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 24

  def run_1, do: input!() |> process() |> walk() |> colorify() |> Enum.count(&(not &1))
  def process(input), do: Enum.map(String.split(input, "\n"), &parse/1)

  defp walk("w", {x, y}), do: {x - 1, y}
  defp walk("e", {x, y}), do: {x + 1, y}
  defp walk("nw", {x, y}), do: {x, y - 1}
  defp walk("ne", {x, y}), do: {x + 1, y - 1}
  defp walk("sw", {x, y}), do: {x - 1, y + 1}
  defp walk("se", {x, y}), do: {x, y + 1}
  defp walk(tiles), do: Enum.map(tiles, fn sides -> Enum.reduce(sides, {0, 0}, &walk/2) end)

  defp parse(s), do: ~r/e|w|se|sw|ne|nw/ |> Regex.scan(s, capture: :first) |> List.flatten()
  defp colorify(tiles), do: Enum.map(Enum.frequencies(tiles), &(rem(elem(&1, 1), 2) == 0))
end
