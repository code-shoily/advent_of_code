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
    input = Transformers.lines(input)

    {solve(input, ~r/\d/),
     solve(input, ~r/(?=(one|two|three|four|five|six|seven|eight|nine|\d))/)}
  end

  defp solve(input, regex), do: Enum.reduce(input, 0, fn x, acc -> acc + extract_by(x, regex) end)

  def extract_by(line, regex) do
    regex
    |> Regex.scan(line)
    |> Enum.map(&List.last/1)
    |> then(&(10 * tr(List.first(&1)) + tr(List.last(&1))))
  end

  def tr(i) when i in ["one", "1"], do: 1
  def tr(i) when i in ["two", "2"], do: 2
  def tr(i) when i in ["three", "3"], do: 3
  def tr(i) when i in ["four", "4"], do: 4
  def tr(i) when i in ["five", "5"], do: 5
  def tr(i) when i in ["six", "6"], do: 6
  def tr(i) when i in ["seven", "7"], do: 7
  def tr(i) when i in ["eight", "8"], do: 8
  def tr(i) when i in ["nine", "9"], do: 9
end
