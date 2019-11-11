defmodule AdventOfCode.Y2015.Day5 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/5
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 5

  defp vowel?(s), do: s in ~w/a e i o u/

  def nice?(s), do: nice?(s, 0, 0, "")
  def nice?("", vowels, dups, _) when vowels >= 3 and dups > 0, do: true
  def nice?("", _, _, _), do: false

  def nice?(<<s::bytes-size(1)>> <> rest, vowels, dups, e) do
    cond do
      (e <> s) in ~w/ab cd pq xy/ -> false
      vowel?(s) and s == e -> nice?(rest, vowels + 1, dups + 1, s)
      vowel?(s) -> nice?(rest, vowels + 1, dups, s)
      s == e -> nice?(rest, vowels, dups + 1, s)
      true -> nice?(rest, vowels, dups, s)
    end
  end

  def run_1 do
    input!()
    |> String.split("\n")
    |> Enum.filter(&nice?/1)
    |> length()
  end

  def run_2 do
    0
  end

  @spec run :: %{
          problem_1: integer(),
          problem_2: integer()
        }
  def run do
    %{
      problem_1: run_1(),
      problem_2: run_2()
    }
  end
end
