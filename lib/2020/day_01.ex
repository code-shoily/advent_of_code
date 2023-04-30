defmodule AdventOfCode.Y2020.Day01 do
  @moduledoc """
  --- Day 1: Report Repair ---
  Problem Link: https://adventofcode.com/2020/day/1
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 1)

  def run(input \\ input()) do
    input = Transformers.int_lines(input)
    {two_entries(input), three_entries(input)}
  end

  def two_entries(xs), do: two_entries(xs, MapSet.new(xs))

  def two_entries(xs, set) do
    Enum.reduce_while(xs, nil, fn x, _ ->
      halt_and_get(set, x, x)
    end)
  end

  def three_entries(xs), do: three_entries(xs, MapSet.new(xs))

  def three_entries([_ | ys] = xs, set) do
    Enum.reduce_while(xs, nil, fn x, _ ->
      Enum.reduce_while(ys, nil, fn y, _ ->
        halt_and_get(set, x + y, x * y)
      end)
      |> halt_and_get()
    end)
  end

  @year 2020
  defp halt_and_get(set, added, multiplied) do
    ((@year - added) in set &&
       {:halt, multiplied * (@year - added)}) ||
      {:cont, nil}
  end

  defp halt_and_get(nil), do: {:cont, nil}
  defp halt_and_get(result), do: {:halt, result}
end
