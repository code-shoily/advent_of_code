defmodule AdventOfCode.Y2016.Day2 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/2
  """
  use AdventOfCode.Data.InputReader, year: 2016, day: 2

  @initial_position 5

  defp valid?(number), do: number <= 0 or number > 9

  defp next_1(cur, "L") when rem(cur, 3) != 1, do: cur - 1
  defp next_1(cur, "R") when rem(cur, 3) != 0, do: cur + 1
  defp next_1(cur, "U"), do: cur - 3
  defp next_1(cur, "D"), do: cur + 3
  defp next_1(_, _), do: -1

  defp parse([], res), do: res
  defp parse([h | t], []), do: parse(t, [find(h, @initial_position, &next_1/2)])
  defp parse([h | t], [x | _] = res), do: parse(t, [find(h, x, &next_1/2) | res])

  defp find([], cur, _), do: cur

  defp find([h | t], cur, next_fn) do
    next_key = next_fn.(cur, h)

    if valid?(next_key) do
      find(t, cur, next_fn)
    else
      find(t, next_key, next_fn)
    end
  end

  @spec run_1 :: :error | integer
  def run_1 do
    process_input()
    |> parse([])
    |> Enum.reverse()
    |> Enum.join()
    |> String.to_integer()
  end

  defp process_input do
    input!()
    |> String.split("\n")
    |> Enum.map(&String.graphemes/1)
  end

  # @matrix [
  #   [nil, nil, "1", nil, nil],
  #   [nil, "2", "3", "4", nil],
  #   ["5", "6", "7", "8", "9"],
  #   [nil, "10", "11", "12", nil],
  #   [nil, nil, "13", nil, nil]
  # ]
  @spec run_2 :: non_neg_integer
  def run_2 do
    input!()
    |> String.split("\n")
    |> length
  end

  def run, do: {run_1(), run_2()}
end
