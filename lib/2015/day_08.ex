defmodule AdventOfCode.Y2015.Day08 do
  @moduledoc """
  --- Day 8: Matchsticks ---
  Problem Link: https://adventofcode.com/2015/day/8
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 8

  def run(input \\ input!()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    Enum.reduce(input, 0, fn line, acc ->
      acc + String.length(line) - actual_length(line)
    end)
  end

  defp run_2(input) do
    Enum.reduce(input, 0, fn line, acc ->
      acc + expanded_length(line) - String.length(line)
    end)
  end

  def parse(data), do: String.split(data, "\n", trim: true)

  @esc_regex ~r/(\\\\|\\\"|\\x[\da-f]{2})/
  @empty_regex ~r/\"/
  def actual_length(line) do
    line
    |> unescape(@esc_regex, "*")
    |> unescape(@empty_regex, "")
    |> String.length()
  end

  @expansion_regex ~r/(\\|\")/
  def expanded_length(line), do: 2 + String.length(unescape(line, @expansion_regex, "**"))
  defp unescape(line, regex, replacement), do: Regex.replace(regex, line, replacement)
end
