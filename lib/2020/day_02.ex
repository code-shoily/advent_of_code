defmodule AdventOfCode.Y2020.Day02 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/2
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 2

  def run_1, do: input!() |> process() |> solve()
  def run_2, do: input!() |> process() |> solve_corrected()
  def run, do: {run_1(), run_2()}

  def process(input) do
    input
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
  end

  @pattern ~r/(\d+)-(\d+) (\S): (\S+)$/
  defp parse_line(line) do
    [_, lo, hi, char, pass] = Regex.run(@pattern, line)

    {String.to_integer(lo), String.to_integer(hi), char, pass}
  end

  defp solve(lines), do: Enum.count(lines, &valid?/1)
  defp solve_corrected(lines), do: Enum.count(lines, &valid_position?/1)

  defp valid?({lo, hi, char, pass}) do
    pass
    |> String.graphemes()
    |> Enum.frequencies()
    |> Map.get(char)
    |> (fn freq -> freq >= lo and freq <= hi end).()
  end

  defp valid_position?({lo, hi, char, pass}) do
    case {String.at(pass, lo - 1), String.at(pass, hi - 1)} do
      {a, a} -> false
      {a, b} when a == char or b == char -> true
      _ -> false
    end
  end
end
