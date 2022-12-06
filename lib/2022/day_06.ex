defmodule AdventOfCode.Y2022.Day06 do
  @moduledoc """
  --- Day 6: Tuning Trouble ---
  Problem Link: https://adventofcode.com/2022/day/6
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 6)

  def run(input \\ input()) do
    input = String.graphemes(input)
    {run_1(input, 0), run_2(input, 0)}
  end

  defp run_1([a, b, c, d | rest], v) do
    case uniq?([a, b, c, d]) do
      true -> v + 4
      _ -> run_1([b, c, d | rest], v + 1)
    end
  end

  defp run_2([a, b, c, d, e, f, g, h, i, j, k, l, m, n | rest], v) do
    case uniq?([a, b, c, d, e, f, g, h, i, j, k, l, m, n]) do
      true -> v + 14
      _ -> run_2([b, c, d, e, f, g, h, i, j, k, l, m, n | rest], v + 1)
    end
  end

  defp uniq?(lst), do: Enum.count(MapSet.new(lst)) == length(lst)
end
