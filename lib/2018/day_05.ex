defmodule AdventOfCode.Y2018.Day05 do
  @moduledoc """
  --- Day 5: Alchemical Reduction ---
  Problem Link: https://adventofcode.com/2018/day/5

  Thanks to `BenAlbin/Adent-Of-Code-2018` for the fastest solution, and helping me
  learn interesting way of solving the problem.
  """
  use AdventOfCode.Helpers.InputReader, year: 2018, day: 5

  def run_1, do: input!() |> process() |> react()

  @units ?A..?Z
  def run_2 do
    with polymer <- process(input!()) do
      @units
      |> Enum.map(fn unit ->
        polymer
        |> Enum.reject(&(&1 == unit or abs(&1 - unit) == 32))
        |> react()
      end)
      |> Enum.min()
    end
  end

  def process(input), do: String.to_charlist(input)

  def react(polymer) do
    Enum.reduce(polymer, {0, []}, fn
      unit, {length, [top | stack]} when abs(top - unit) == 32 -> {length - 1, stack}
      unit, {length, stack} -> {length + 1, [unit | stack]}
    end)
    |> elem(0)
  end
end
