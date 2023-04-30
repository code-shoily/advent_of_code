defmodule AdventOfCode.Y2020.Day02 do
  @moduledoc """
  --- Day 2: Password Philosophy ---
  Problem Link: https://adventofcode.com/2020/day/2
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 2)

  def run(input \\ input()) do
    input = parse(input)

    {solve(input), solve_corrected(input)}
  end

  def parse(input) do
    input
    |> Transformers.lines()
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
    |> then(fn freq -> freq >= lo and freq <= hi end)
  end

  defp valid_position?({lo, hi, char, pass}) do
    case {String.at(pass, lo - 1), String.at(pass, hi - 1)} do
      {a, a} -> false
      {a, b} when a == char or b == char -> true
      _ -> false
    end
  end
end
