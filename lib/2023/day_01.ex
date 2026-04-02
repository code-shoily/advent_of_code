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

  ~w/one two three four five six seven eight nine/
  |> Enum.with_index(1)
  |> Enum.each(fn {word, index} ->
    defp tr(unquote(word)), do: unquote(index)
  end)

  defp tr(numeric), do: String.to_integer(numeric)
end
