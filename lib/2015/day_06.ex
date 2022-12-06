defmodule AdventOfCode.Y2015.Day06 do
  @moduledoc """
  --- Day 6: Probably a Fire Hazard ---
  Problem Link: https://adventofcode.com/2015/day/6
  NOTE: This is slow!
  """
  alias Aja.Vector
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 6)

  def run(input \\ input()) do
    input = parse(input)
    grid = make_grid(1000)

    task_1 = Task.async(fn -> run_1(input, grid) end)
    task_2 = Task.async(fn -> run_2(input, grid) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(&parse_input/1)
  end

  defp run_1(input, grid) do
    input
    |> Enum.reduce(grid, &apply_1(&1, &2))
    |> total_brightness()
  end

  defp run_2(input, grid) do
    input
    |> Enum.reduce(grid, &apply_2(&1, &2))
    |> total_brightness()
  end

  def make_grid(dim) do
    0 |> Vector.duplicate(dim) |> Vector.duplicate(dim)
  end

  @regex ~r"""
  (?<cmd>toggle|turn\son|turn\soff)\s
  (?<x1>\d+),(?<y1>\d+)
  \s\S+\s
  (?<x2>\d+),(?<y2>\d+)
  """x
  defp parse_input(line) do
    @regex
    |> Regex.named_captures(line)
    |> format()
  end

  defp format(%{"cmd" => cmd, "x1" => x1, "x2" => x2, "y1" => y1, "y2" => y2}) do
    {
      cmd |> String.replace(" ", "_") |> String.to_atom(),
      {String.to_integer(x1), String.to_integer(y1)},
      {String.to_integer(x2), String.to_integer(y2)}
    }
  end

  defp apply_1({cmd, {x1, y1}, {x2, y2}}, grid) do
    for x <- x1..x2, y <- y1..y2 do
      [x, y]
    end
    |> Aja.Enum.reduce(grid, fn [x, y], acc ->
      case cmd do
        :turn_on -> put_in(acc, [x, y], 1)
        :turn_off -> put_in(acc, [x, y], 0)
        :toggle -> update_in(acc, [x, y], fn v -> (v == 0 && 1) || 0 end)
      end
    end)
  end

  defp total_brightness(grid) do
    Aja.Enum.reduce(grid, 0, fn row, acc ->
      acc + Aja.Enum.sum(Aja.Enum.filter(row, &(&1 > 0)))
    end)
  end

  defp apply_2({cmd, {x1, y1}, {x2, y2}}, grid) do
    for x <- x1..x2, y <- y1..y2 do
      [x, y]
    end
    |> Aja.Enum.reduce(grid, fn [x, y], acc ->
      case cmd do
        :turn_on -> update_in(acc, [x, y], fn v -> v + 1 end)
        :turn_off -> update_in(acc, [x, y], fn v -> max(0, v - 1) end)
        :toggle -> update_in(acc, [x, y], fn v -> v + 2 end)
      end
    end)
  end
end
