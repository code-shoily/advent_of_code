defmodule AdventOfCode.Y2017.Day13 do
  @moduledoc """
  --- Day 13: Packet Scanners ---
  Problem Link: https://adventofcode.com/2017/day/13
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 13

  alias AdventOfCode.Helpers.Transformers

  def run(input \\ input!()) do
    input = parse(input)
    limit = input |> Map.keys() |> Enum.max()

    {run_1(input, limit), run_2(input, limit)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Map.new(fn line ->
      line
      |> String.split(": ")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end)
  end

  defp run_1(input, limit, delay \\ 0) do
    input
    |> sneak(limit, delay)
    |> severity(input)
  end

  defp run_2(input, limit, delay \\ 0) do
    case sneak(input, limit, delay) do
      [] -> delay
      _ -> run_2(input, limit, delay + 1)
    end
  end

  defp sneak(input, limit, delay) do
    Enum.reduce(0..limit, [], fn pos, detections ->
      case Map.get(input, pos) do
        nil -> detections
        range -> (detect?(pos, range, delay) && [pos | detections]) || detections
      end
    end)
  end

  defp detect?(pico_second, range, delay) do
    rem(pico_second + delay, 2 * (range - 1)) == 0
  end

  defp severity(detections, input) do
    input
    |> Map.take(detections)
    |> Enum.map(fn {k, v} -> k * v end)
    |> Enum.sum()
  end
end
