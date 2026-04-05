defmodule AdventOfCode.Y2024.Day07 do
  @moduledoc """
  --- Day 7: Bridge Repair ---
  Problem Link: https://adventofcode.com/2024/day/7
  Difficulty: s
  Tags: enumeration recursion
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 7)

  def run(input \\ input()) do
    equations = parse(input)

    p1 = run_1(equations)
    p2 = run_2(equations)

    {p1, p2}
  end

  defp run_1(equations) do
    solve(equations, [:add, :mul])
  end

  defp run_2(equations) do
    solve(equations, [:add, :mul, :cat])
  end

  defp solve(equations, ops) do
    equations
    |> Enum.filter(fn {target, [initial | rest]} ->
      valid?(target, initial, rest, ops)
    end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  defp valid?(target, target, [], _ops), do: true
  defp valid?(_target, _current, [], _ops), do: false
  defp valid?(target, current, [next | rest], ops) do
    Enum.any?(ops, fn op ->
      new_value = apply_op(op, current, next)
      new_value <= target and valid?(target, new_value, rest, ops)
    end)
  end

  defp apply_op(:add, a, b), do: a + b
  defp apply_op(:mul, a, b), do: a * b
  defp apply_op(:cat, a, b), do: String.to_integer("#{a}#{b}")

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [target, nums] = String.split(line, ": ")
      nums = nums |> String.split(" ") |> Enum.map(&String.to_integer/1)
      {String.to_integer(target), nums}
    end)
  end
end
