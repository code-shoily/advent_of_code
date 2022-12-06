defmodule AdventOfCode.Y2022.Day06 do
  @moduledoc """
  --- Day 6: Tuning Trouble ---
  Problem Link: https://adventofcode.com/2022/day/6
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 6)
  def run(data \\ input()), do: {marker(data, 4), marker(data, 14)}
  defp uniq?(xs, len), do: len == Enum.count(MapSet.new(:binary.bin_to_list(xs)))

  defp marker(<<_::bytes-size(1)>> <> xs = data, len, v \\ 0),
    do: (uniq?(:binary.part(data, 0, len), len) && v + len) || marker(xs, len, v + 1)
end
