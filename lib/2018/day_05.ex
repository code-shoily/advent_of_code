defmodule AdventOfCode.Y2018.Day05 do
  @moduledoc """
  --- Day 5: Alchemical Reduction ---
  Problem Link: https://adventofcode.com/2018/day/5
  
  Thanks to `BenAlbin/Adent-Of-Code-2018` for the fastest solution, and helping me
  learn interesting way of solving the problem.
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2018, 5)

  def run(input \\ input()) do
    input = String.to_charlist(input)
    {run_1(input), run_2(input)}
  end

  def run_1(input), do: react(input)

  @units ?A..?Z
  def run_2(input) do
    Enum.min(
      @units
      |> Enum.map(fn unit ->
        input
        |> Enum.reject(&(&1 == unit or abs(&1 - unit) == 32))
        |> react()
      end)
    )
  end

  def react(polymer) do
    Enum.reduce(polymer, {0, []}, fn
      unit, {length, [top | stack]} when abs(top - unit) == 32 -> {length - 1, stack}
      unit, {length, stack} -> {length + 1, [unit | stack]}
    end)
    |> elem(0)
  end
end
