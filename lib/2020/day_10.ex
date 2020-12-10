defmodule AdventOfCode.Y2020.Day10 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/10
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 10

  def run_1, do: input!() |> process() |> rates() |> multiply_1_3()
  def run_2, do: {:not_implemented, 2}
  def run, do: {run_1(), run_2()}

  def process(input),
    do: input |> String.split("\n") |> Enum.map(&String.to_integer/1) |> MapSet.new()

  defp rates(data) do
    my_rate = Enum.max(data) + 3

    Stream.unfold(0, fn
      n when n > my_rate ->
        nil

      n ->
        range = MapSet.new((n + 1)..(n + 3))
        jolt = MapSet.intersection(data, range) |> Enum.to_list()
        if Enum.empty?(jolt), do: diffs(my_rate, n), else: diffs(Enum.min(jolt), n)
    end)
  end

  defp diffs(a, b) when a - b == 1, do: {{1, 0}, a}
  defp diffs(a, b) when a - b == 2, do: {{0, 0}, a}
  defp diffs(a, b) when a - b == 3, do: {{0, 1}, a}
  defp diffs(_, _), do: nil

  defp multiply_1_3(r),
    do: apply(&Kernel.*/2, Enum.reduce(r, [0, 0], fn {a, b}, [x, y] -> [x + a, y + b] end))
end
