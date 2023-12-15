defmodule AdventOfCode.Y2019.Day05 do
  @moduledoc """
  --- Day 5: Sunny with a Chance of Asteroids ---
  Problem Link: https://adventofcode.com/2019/day/5
  Difficulty: xs
  Tags: op-code int-code
  """
  alias AdventOfCode.Helpers.InputReader
  alias AdventOfCode.Y2019.IntCode

  def input, do: InputReader.read_from_file(2019, 5)

  def run(input \\ input()) do
    input = parse(input)
    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    {:ok, pid} = input |> IntCode.start_link()
    %{output: output} = IntCode.run(pid)

    output
    |> Enum.reject(&(&1 == 0))
    |> case do
      [data] -> data
      [] -> -1
    end
  end

  def run_2(_) do
    nil
  end

  def parse(data), do: data |> String.split(",") |> Enum.map(&String.to_integer/1)
end
