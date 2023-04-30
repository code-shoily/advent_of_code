defmodule AdventOfCode.Y2021.Day06 do
  @moduledoc """
  --- Day 6: Lanternfish ---
  Problem Link: https://adventofcode.com/2021/day/6
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2021, 6)

  def run(input \\ input()) do
    input = parse(input)
    {Enum.sum(multiply(input, 80)), Enum.sum(multiply(input, 256))}
  end

  def parse(data) do
    data
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> Enum.frequencies()
  end

  def multiply(fishes, day) do
    (day == 0 && Map.values(fishes)) ||
      multiply(
        Map.pop(fishes, 0)
        |> then(
          &Map.merge(
            for({k, v} <- elem(&1, 1), into: %{}, do: {k - 1, v}),
            %{6 => elem(&1, 0) || 0, 8 => elem(&1, 0) || 0},
            fn _, a, b -> a + b end
          )
        ),
        day - 1
      )
  end
end
