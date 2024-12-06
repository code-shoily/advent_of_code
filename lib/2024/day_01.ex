defmodule AdventOfCode.Y2024.Day01 do
  @moduledoc """
  --- Day 1: Historian Hysteria ---
  Problem Link: https://adventofcode.com/2024/day/1
  Difficulty: xs
  Tags: list
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 1)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1({left_list, right_list, _}) do
    for {left, right} <- Enum.zip(left_list, right_list), reduce: 0 do
      acc -> acc + abs(left - right)
    end
  end

  defp run_2({left_list, _, tally}) do
    for id_value <- left_list, reduce: 0 do
      acc -> acc + id_value * Map.get(tally, id_value, 0)
    end
  end

  def parse(data \\ input()) do
    {left, right} =
      for line <- Transformers.lines(data), reduce: {[], []} do
        {left_list, right_list} ->
          [left, right] = String.split(line)
          {[String.to_integer(left) | left_list], [String.to_integer(right) | right_list]}
      end

    {Enum.sort(left), Enum.sort(right), Enum.frequencies(right)}
  end
end
