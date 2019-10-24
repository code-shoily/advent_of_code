defmodule AdventOfCode.Y2015.Day6 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/6
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 6

  def init(size) do
    for i <- 0..size, into: %{} do
      {i, for(j <- 0..size, into: %{}, do: {j, false})}
    end
  end

  def make_grid([x1, y1], [x2, y2]) do
    for i <- x1..x2,
        j <- y1..y2,
        do: [i, j]
  end

  def execute({state, [[x1, y1], [x2, y2]]}, grid) when is_boolean(state) do
    [x1, y1]
    |> make_grid([x2, y2])
    |> Enum.reduce(grid, fn x, acc -> put_in(acc, x, state) end)
  end

  def execute({:toggle, [[x1, y1], [x2, y2]]}, grid) do
    [x1, y1]
    |> make_grid([x2, y2])
    |> Enum.reduce(grid, fn x, acc -> update_in(acc, x, fn v -> !v end) end)
  end

  def coordinates(data) do
    data
    |> String.trim()
    |> String.split(" ")
    |> Enum.filter(&String.contains?(&1, ","))
    |> Enum.map(&String.split(&1, ","))
    |> Enum.map(fn [x, y] ->
      [String.to_integer(x), String.to_integer(y)]
    end)
  end

  def parse_instruction("turn on" <> rest), do: {true, coordinates(rest)}
  def parse_instruction("turn off" <> rest), do: {false, coordinates(rest)}
  def parse_instruction("toggle" <> rest), do: {:toggle, coordinates(rest)}

  def parse_line(line) do
    line
    |> String.replace("through", "")
    |> parse_instruction()
  end

  def light_on_count(grid) do
    grid
    |> Enum.flat_map(fn {_, val} ->
      Enum.map(val, fn {_, val} -> val end)
    end)
    |> Enum.filter(& &1)
    |> length()
  end

  def run do
    grid = init(999)

    input!()
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> Enum.reduce(grid, fn x, acc -> execute(x, acc) end)
    |> light_on_count()
  end
end
