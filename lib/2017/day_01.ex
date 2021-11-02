defmodule AdventOfCode.Y2017.Day01 do
  @moduledoc """
  --- Day 1: Inverse Captcha ---
  Problem Link: https://adventofcode.com/2017/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 1

  def run_1 do
    input!()
    |> process()
    |> matches_with_next()
    |> Enum.sum()
  end

  def run_2 do
    with data <- process(input!()),
         half <- data |> length() |> div(2) do
      data
      |> Enum.split(half)
      |> matching_pair_sum()
    end
  end

  def process(input) do
    input
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
  end

  defp matches_with_next([h | _] = lst) do
    lst
    |> Enum.chunk_every(2, 1, [h])
    |> Enum.map(fn pair ->
      case pair do
        [a, a] -> a
        _ -> 0
      end
    end)
  end

  defp matching_pair_sum({a, b}) do
    a
    |> Enum.zip(b)
    |> Enum.map(fn
      {a, a} -> a + a
      _ -> 0
    end)
    |> Enum.sum()
  end
end
