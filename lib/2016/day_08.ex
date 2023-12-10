defmodule AdventOfCode.Y2016.Day08 do
  @moduledoc """
  --- Day 8: Two-Factor Authentication ---
  Problem Link: https://adventofcode.com/2016/day/8
  Difficulty: m
  Tags: visual-result matrix op-code
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @on "█"
  @off "."
  @width 50
  @height 6

  def input, do: InputReader.read_from_file(2016, 8)

  def run(input \\ input()) do
    input = parse(input)

    process = Enum.reduce(input, empty_grid(), &operation/2)

    {Enum.count(process, fn {_, v} -> v == @on end), display(process)}
  end

  def to_dims(d) do
    String.split(d, "x")
    |> Enum.map(&to_i/1)
    |> List.to_tuple()
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      case Transformers.words(line) do
        ["rect", d] -> {:rect, to_dims(d)}
        [_, _, "y=" <> y, _, by] -> {:row, to_i(y), to_i(by)}
        [_, _, "x=" <> x, _, by] -> {:column, to_i(x), to_i(by)}
      end
    end)
  end

  defp empty_grid do
    for w <- 0..(@width - 1), h <- 0..(@height - 1), into: %{} do
      {{w, h}, @off}
    end
  end

  defp operation({:rect, {w, h}}, grid) do
    for x <- 0..(w - 1), y <- 0..(h - 1), reduce: grid do
      acc -> %{acc | {x, y} => @on}
    end
  end

  defp operation({:row, at, by}, grid) do
    grid
    |> line_by_row(at)
    |> Map.new(fn {{x, y}, _} ->
      {{rem(x + by, @width), y}, grid[{x, y}]}
    end)
    |> then(&Map.merge(grid, &1))
  end

  defp operation({:column, at, by}, grid) do
    grid
    |> line_by_column(at)
    |> Map.new(fn {{x, y}, _} ->
      {{x, rem(y + by, @height)}, grid[{x, y}]}
    end)
    |> then(&Map.merge(grid, &1))
  end

  defp display(grid) do
    for h <- 0..(@height - 1) do
      for w <- 0..(@width - 1) do
        IO.write(grid[{w, h}])
      end

      IO.puts("")
    end

    IO.puts("")

    :ok
  end

  defp line_by(grid, n, xy), do: Map.filter(grid, fn {k, _} -> elem(k, xy) == n end)
  defp line_by_column(grid, at), do: line_by(grid, at, 0)
  defp line_by_row(grid, at), do: line_by(grid, at, 1)

  defp to_i(v), do: String.to_integer(v)
end
