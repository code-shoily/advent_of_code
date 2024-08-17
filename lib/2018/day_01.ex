defmodule AdventOfCode.Y2018.Day01 do
  @moduledoc """
  --- Day 1: Chronal Calibration ---
  Problem Link: https://adventofcode.com/2018/day/1
  Difficulty: s
  Tags: sequence set rust
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2018, 1)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    Enum.reduce(input, 0, &Kernel.+/2)
  end

  def run_2(input) do
    input
    |> Stream.cycle()
    |> Enum.reduce_while({%MapSet{}, 0}, fn cur, {history, prev} ->
      new_frequency = prev + cur

      if MapSet.member?(history, new_frequency) do
        {:halt, new_frequency}
      else
        {:cont, {MapSet.put(history, new_frequency), new_frequency}}
      end
    end)
  end

  @spec parse(String.t()) :: [String.t()]
  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(&String.to_integer/1)
  end
end
