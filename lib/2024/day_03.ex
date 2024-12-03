defmodule AdventOfCode.Y2024.Day03 do
  @moduledoc """
  --- Day 3: Mull It Over ---
  Problem Link: https://adventofcode.com/2024/day/3
  Difficulty:
  Tags:
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 3)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    input
    |> Enum.flat_map(fn line -> line |> tokenize() |> Enum.map(&mul/1) end)
    |> Enum.sum()
  end

  defp run_2(input) do
    input
    |> Enum.flat_map(&tokenize_with_state/1)
    |> Enum.reduce({true, 0}, fn instruction, {continue?, total} ->
      case {continue?, instruction} do
        {_, "do()"} -> {true, total}
        {_, "don't()"} -> {false, total}
        {true, instruction} -> {true, mul(instruction) + total}
        {false, _} -> {false, total}
      end
    end)
    |> elem(1)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
  end

  def tokenize_with_state(line) do
    ~r/(?>mul\(\d+,\d+\))|(?>do\(\))|(?>don't\(\))/
    |> Regex.scan(line)
    |> List.flatten()
  end

  def tokenize(line) do
    ~r/mul\(\d+,\d+\)/
    |> Regex.scan(line)
    |> List.flatten()
  end

  def mul(token) do
    token
    |> String.replace("mul(", "")
    |> String.replace(")", "")
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> Enum.reduce(&(&1 * &2))
  end
end
