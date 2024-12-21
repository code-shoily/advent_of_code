defmodule AdventOfCode.Y2017.Day01 do
  @moduledoc """
  --- Day 1: Inverse Captcha ---
  Problem Link: https://adventofcode.com/2017/day/1
  Difficulty: xs
  Tags: sliding-window rust
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2017, 1)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input), do: Enum.sum(matches_with_next(input))

  defp run_2(input) do
    half = div(length(input), 2)
    matching_pair_sum(Enum.split(input, half))
  end

  def parse(input) do
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
    Enum.zip(a, b)
    |> Enum.sum_by(fn
      {a, a} -> a + a
      _ -> 0
    end)
  end
end
