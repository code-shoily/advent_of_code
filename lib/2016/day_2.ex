defmodule AdventOfCode.Y2016.Day2 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/2
  """
  use AdventOfCode.Data.InputReader, year: 2016, day: 2

  @initial_position 5

  defp valid?(number), do: number <= 0 or number > 9

  defp next(cur, "L") when rem(cur, 3) != 1, do: cur - 1
  defp next(cur, "R") when rem(cur, 3) != 0, do: cur + 1
  defp next(cur, "U"), do: cur - 3
  defp next(cur, "D"), do: cur + 3
  defp next(_, _), do: -1

  defp parse([], res), do: res
  defp parse([h | t], []), do: parse(t, [find(h, @initial_position)])
  defp parse([h | t], [x | _] = res), do: parse(t, [find(h, x) | res])

  defp find([], cur), do: cur

  defp find([h | t], cur) do
    next_key = next(cur, h)

    if valid?(next_key) do
      find(t, cur)
    else
      find(t, next_key)
    end
  end

  defp to_int(value) do
    case Integer.parse(value) do
      {res, ""} -> res
      _ -> :error
    end
  end

  def run_1 do
    input!()
    |> String.split("\n")
    |> Enum.map(&String.graphemes/1)
    |> parse([])
    |> Enum.reverse()
    |> Enum.join()
    |> to_int()
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
