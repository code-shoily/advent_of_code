defmodule AdventOfCode.Y2015.Day01 do
  @moduledoc """
  --- Day 1: Not Quite Lisp ---
  Problem Link: https://adventofcode.com/2015/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 1

  def run, do: {run_1(), run_2()}
  def run_1, do: input!() |> floor(0)
  def run_2, do: input!() |> to_basement(0, 0)

  @spec floor(String.t(), non_neg_integer) :: non_neg_integer
  defp floor("", cur), do: cur
  defp floor("(" <> rst, cur), do: floor(rst, cur + 1)
  defp floor(")" <> rst, cur), do: floor(rst, cur - 1)

  @spec to_basement(String.t(), non_neg_integer, integer) :: non_neg_integer
  defp to_basement(_, pos, -1), do: pos
  defp to_basement("(" <> rst, pos, cur), do: to_basement(rst, pos + 1, cur + 1)
  defp to_basement(")" <> rst, pos, cur), do: to_basement(rst, pos + 1, cur - 1)
end
