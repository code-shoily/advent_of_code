defmodule AdventOfCode.Y2015.Day08 do
  @moduledoc """
  --- Day 8: Matchsticks ---
  Problem Link: https://adventofcode.com/2015/day/8
  Difficulty: m
  Tags: string-encoding annoying
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 8)

  def run(input \\ input()) do
    input = Transformers.lines(input)

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
