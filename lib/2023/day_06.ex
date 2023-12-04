defmodule AdventOfCode.Y2023.Day06 do
  @moduledoc """
  --- Day 6: Wait For It ---
  Problem Link: https://adventofcode.com/2023/day/6
  Difficulty: xs
  Tags: algebra
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 6)

  def run(input \\ input()) do
    input = parse(input)
    {run_1(input), run_2(input)}
  end

  defp run_1(input), do: Enum.reduce(input, 1, &(&2 * ways_to_win(&1)))
  defp run_2(input), do: input |> join_numbers() |> ways_to_win()

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&extract/1)
    |> then(fn [times, records] -> Enum.zip(times, records) end)
  end

  defp extract(data),
    do: ~r{\d+} |> Regex.scan(data) |> List.flatten() |> Enum.map(&String.to_integer/1)

  def ways_to_win({time, record}) do
    delta = :math.sqrt(time * time - 4 * record)
    floor((time + delta) / 2) - ceil((time - delta) / 2) + 1
  end

  defp join_numbers(data) do
    data
    |> Enum.unzip()
    |> Tuple.to_list()
    |> Enum.map(&(&1 |> Enum.join() |> String.to_integer()))
    |> List.to_tuple()
  end
end
