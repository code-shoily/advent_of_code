defmodule AdventOfCode.Y2018.Day1 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2018/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2018, day: 1

  @spec to_number_list(String.t()) :: [String.t()]
  def to_number_list(raw_input) do
    raw_input
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
  end

  @spec run_1() :: integer()
  def run_1, do: input!() |> to_number_list() |> Enum.reduce(0, &Kernel.+/2)

  @spec run_2() :: non_neg_integer()
  def run_2 do
    input!()
    |> to_number_list()
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

  def run, do: {run_1(), run_2()}
end
