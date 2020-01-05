defmodule AdventOfCode.Y2018.Day5 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2018/day/5

  Thanks to `BenAlbin/Adent-Of-Code-2018` for the fastest solution, and helping me
  learn interesting way of solving the problem.
  """
  use AdventOfCode.Data.InputReader, year: 2018, day: 5

  def process(input), do: String.to_charlist(input)

  def run_1, do: input!() |> process() |> react()

  def react(polymer) do
    Enum.reduce(polymer, {0, []}, fn
      unit, {length, [top | stack]} when abs(top - unit) == 32 -> {length - 1, stack}
      unit, {length, stack} -> {length + 1, [unit | stack]}
    end)
    |> elem(0)
  end

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

  def run, do: {run_1(), run_2()}
end
