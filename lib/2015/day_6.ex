defmodule AdventOfCode.Y2015.Day6 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/6
  """
  # ! Faster implementation of this.
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 6

  def make_grid(dim) do
    0..(dim - 1)
    |> Stream.map(fn x ->
      {x, 0..(dim - 1) |> Stream.map(&{&1, 0}) |> Enum.into(%{})}
    end)
    |> Enum.into(%{})
  end

  def process(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse/1)
  end

  @regex ~r"""
  (?<cmd>toggle|turn\son|turn\soff)\s
  (?<x1>\d+),(?<y1>\d+)
  \s\S+\s
  (?<x2>\d+),(?<y2>\d+)
  """x
  defp parse(line) do
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

  def apply_1(:turn_on, coord, src), do: update_in(src, coord, fn _ -> 1 end)
  def apply_1(:turn_off, coord, src), do: update_in(src, coord, fn _ -> 0 end)
  def apply_1(:toggle, coord, src), do: update_in(src, coord, fn v -> (v == 0 && 1) || 0 end)

  def apply_1({cmd, {x1, y1}, {x2, y2}}, src) do
    x1..x2
    |> Stream.flat_map(fn x ->
      y1..y2
      |> Stream.map(fn y ->
        [x, y]
      end)
    end)
    |> Enum.reduce(src, fn x, acc -> apply_1(cmd, x, acc) end)
  end

  def total_brightness(grid) do
    grid
    |> Stream.flat_map(fn {_, line} ->
      Stream.map(line, fn {_, value} -> value end)
    end)
    |> Enum.sum()
  end

  def run_1 do
    grid = make_grid(1000)

    input!()
    |> process()
    |> Enum.reduce(grid, &apply_1(&1, &2))
    |> total_brightness()
  end

  def apply_2(:turn_on, coord, src), do: update_in(src, coord, fn v -> v + 1 end)

  def apply_2(:turn_off, coord, src),
    do: update_in(src, coord, fn v -> (v - 1 < 0 && 0) || v - 1 end)

  def apply_2(:toggle, coord, src), do: update_in(src, coord, fn v -> v + 2 end)

  def apply_2({cmd, {x1, y1}, {x2, y2}}, src) do
    x1..x2
    |> Stream.flat_map(fn x ->
      y1..y2
      |> Stream.map(fn y ->
        [x, y]
      end)
    end)
    |> Enum.reduce(src, fn x, acc -> apply_2(cmd, x, acc) end)
  end

  def run_2 do
    grid = make_grid(1000)

    input!()
    |> process()
    |> Enum.reduce(grid, &apply_2(&1, &2))
    |> total_brightness()
  end

  def run, do: {run_1(), run_2()}
end
