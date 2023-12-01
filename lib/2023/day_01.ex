defmodule AdventOfCode.Y2023.Day01 do
  @moduledoc """
  --- Day 1: Trebuchet?! ---
  Problem Link: https://adventofcode.com/2023/day/1
  Difficulty: xs
  Tags: regex
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 1)

  def run(input \\ input()) do
    input
    |> Transformers.lines()
    |> then(
      &{solve(&1, ~r"\d"), solve(&1, ~r/(?=(one|two|three|four|five|six|seven|eight|nine|\d))/)}
    )
  end

  defp solve(input, regex) do
    for line <- input, reduce: 0 do
      acc ->
        regex
        |> Regex.scan(line)
        |> Enum.map(&List.last/1)
        |> then(&(10 * tr(List.first(&1)) + tr(List.last(&1))))
        |> Kernel.+(acc)
    end
  end

  defp tr(i) when i in ["one", "1"], do: 1
  defp tr(i) when i in ["two", "2"], do: 2
  defp tr(i) when i in ["three", "3"], do: 3
  defp tr(i) when i in ["four", "4"], do: 4
  defp tr(i) when i in ["five", "5"], do: 5
  defp tr(i) when i in ["six", "6"], do: 6
  defp tr(i) when i in ["seven", "7"], do: 7
  defp tr(i) when i in ["eight", "8"], do: 8
  defp tr(i) when i in ["nine", "9"], do: 9
end
