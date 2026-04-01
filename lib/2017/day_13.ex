defmodule AdventOfCode.Y2017.Day13 do
  @moduledoc """
  --- Day 13: Packet Scanners ---
  Problem Link: https://adventofcode.com/2017/day/13
  Difficulty: s
  Tags: sequence optimization
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 13)

  def run(input \\ input()) do
    # Optimization: Store scanners as a list of {pos, period} tuples
    scanners =
      input
      |> parse()
      |> Enum.map(fn {pos, range} -> {pos, 2 * (range - 1), range} end)

    # Scanners with smaller periods are more likely to catch us early,
    # so sorting them can speed up Enum.any? short-circuiting in Part 2.
    sorted_scanners = Enum.sort_by(scanners, fn {_, period, _} -> period end)

    task_1 = Task.async(fn -> solve_1(scanners) end)
    task_2 = Task.async(fn -> solve_2(sorted_scanners) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Map.new(fn line ->
      [pos, range] = String.split(line, ": ") |> Enum.map(&String.to_integer/1)
      {pos, range}
    end)
  end

  defp solve_1(scanners) do
    scanners
    |> Enum.filter(fn {pos, period, _} -> rem(pos, period) == 0 end)
    |> Enum.map(fn {pos, _, range} -> pos * range end)
    |> Enum.sum()
  end

  defp solve_2(scanners) do
    Stream.iterate(0, &(&1 + 1))
    |> Enum.find(fn delay ->
      # Short-circuiting any check makes this much faster than iterating all layers
      !Enum.any?(scanners, fn {pos, period, _} -> rem(delay + pos, period) == 0 end)
    end)
  end
end
