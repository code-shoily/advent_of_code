defmodule AdventOfCode.Y2020.Day25 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/25
  Tweet: https://twitter.com/mafinar/status/1343609407201042436
  """
  @input [14_222_596, 4_057_428]
  def run, do: @input |> (fn [a, b] -> t(1, a, lp(1, b, 1)) end).()

  defp lp(x, k, s), do: ((r = t1(x, 7)) == k && s) || lp(r, k, s + 1)
  defp t(x, p, s), do: (s == 0 && x) || t(t1(x, p), p, s - 1)
  defp t1(x, p), do: rem(x * p, 20_201_227)
end
