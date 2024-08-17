defmodule AdventOfCode.Y2021.Day01 do
  @moduledoc """
  --- Day 1: Sonar Sweep ---
  Problem Link: https://adventofcode.com/2021/day/1
  Difficulty: xs
  Tags: sliding-window sequence rust
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2021, 1)

  def run(input \\ input()) do
    input = Transformers.int_lines(input)
    {depth_increase(input), depth_increase(sliding_window(input))}
  end

  defp depth_increase(measurements) do
    measurements
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.count(fn [a, b] -> b - a > 0 end)
  end

  defp sliding_window(measurements) do
    measurements
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.map(&Enum.sum/1)
  end
end
