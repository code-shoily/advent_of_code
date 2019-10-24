defmodule AdventOfCode.Y2015.Day7 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/7
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 7

  defp empty?(message) do
    String.trim(message) == ""
  end

  def tokenize([operator, operand]) do
    {operator, operand}
  end

  def tokenize([operand_1, operator, operand_2]) do
    {operator, operand_1, operand_2}
  end

  def tokenize(result), do: {"ASSIGN", result}

  def parse_line(instruction) do
    instruction
    |> String.split("->")
    |> Enum.map(&String.trim/1)
    |> (fn [lhs, rhs] ->
          {tokenize(String.split(lhs)), rhs}
        end).()
  end

  def parse(instructions) do
    instructions
    |> Enum.map(&parse_line/1)
  end

  def run do
    input!()
    |> String.split("\n")
    |> Enum.reject(&empty?/1)
    |> parse()
  end
end
