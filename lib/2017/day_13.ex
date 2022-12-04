defmodule AdventOfCode.Y2017.Day13 do
  @moduledoc """
  --- Day 13: Packet Scanners ---
  Problem Link: https://adventofcode.com/2017/day/13
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 13)

  def run(input \\ input()) do
    input = parse(input)
    limit = Enum.max(Map.keys(input))

    task_1 = Task.async(fn -> run_1(input, limit) end)
    task_2 = Task.async(fn -> run_2(input, limit) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
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
