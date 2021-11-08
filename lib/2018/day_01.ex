defmodule AdventOfCode.Y2018.Day01 do
  @moduledoc """
  --- Day 1: Chronal Calibration ---
  Problem Link: https://adventofcode.com/2018/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2018, day: 1

  @spec run_1() :: integer()
  def run_1, do: input!() |> parse() |> Enum.reduce(0, &Kernel.+/2)

  @spec run_2() :: non_neg_integer()
  def run_2 do
    input!()
    |> parse()
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
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
  end
end
