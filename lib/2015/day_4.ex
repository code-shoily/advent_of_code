defmodule AdventOfCode.Y2015.Day4 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/4
  """
  defp h(s, number),
    do:
      :crypto.hash(:md5, s <> Integer.to_string(number))
      |> Base.encode16()

  defp satisfy(_, "0" <> "0" <> "0" <> "0" <> "0" <> _, n), do: n
  defp satisfy(s, _, n), do: lowest_number(s, n + 1)

  def lowest_number(s, number), do: satisfy(s, h(s, number), number)

  def run_1, do: "bgvyzdsv" |> lowest_number(1)

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
