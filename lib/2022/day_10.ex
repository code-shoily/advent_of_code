defmodule AdventOfCode.Y2022.Day10 do
  @moduledoc """
  --- Day 10: Cathode-Ray Tube ---
  Problem Link: https://adventofcode.com/2022/day/10
  Difficulty: m
  Tags: visual-result op-code
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 10)

  def run(input \\ input()) do
    cycles = simulate(parse(input))

    part1 =
      [20, 60, 100, 140, 180, 220]
      |> Enum.map(fn c -> c * cycles[c] end)
      |> Enum.sum()

    part2 = draw(cycles)

    {part1, part2}
  end

  defp parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(fn
      "noop" -> :noop
      "addx " <> val -> {:addx, String.to_integer(val)}
    end)
  end

  defp simulate(instructions) do
    {cycles, _, _} =
      Enum.reduce(instructions, {%{}, 1, 1}, fn
        :noop, {acc, cycle, x} ->
          {Map.put(acc, cycle, x), cycle + 1, x}

        {:addx, v}, {acc, cycle, x} ->
          acc = acc |> Map.put(cycle, x) |> Map.put(cycle + 1, x)
          {acc, cycle + 2, x + v}
      end)

    cycles
  end

  defp draw(cycles) do
    for r <- 0..5 do
      for c <- 0..39 do
        cycle = r * 40 + c + 1
        x = cycles[cycle]

        if c in (x - 1)..(x + 1) do
          IO.write("█")
        else
          IO.write("▒")
        end
      end

      IO.puts("")
    end

    :ok
  end
end
