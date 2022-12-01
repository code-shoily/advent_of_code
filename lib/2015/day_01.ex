defmodule AdventOfCode.Y2015.Day01 do
  @moduledoc """
  --- Day 1: Not Quite Lisp ---
  Problem Link: https://adventofcode.com/2015/day/1
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2015, 1)

  def run, do: {floor(input(), 0), to_basement(input(), 0, 0)}

  defp floor("", cur), do: cur
  defp floor("(" <> rst, cur), do: floor(rst, cur + 1)
  defp floor(")" <> rst, cur), do: floor(rst, cur - 1)

  defp to_basement(_, pos, -1), do: pos
  defp to_basement("(" <> rst, pos, cur), do: to_basement(rst, pos + 1, cur + 1)
  defp to_basement(")" <> rst, pos, cur), do: to_basement(rst, pos + 1, cur - 1)
end
