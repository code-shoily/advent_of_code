defmodule AdventOfCode.Y2015.Day18 do
  @moduledoc """
  --- Day 18: Like a GIF For Your Yard ---
  Problem Link: https://adventofcode.com/2015/day/18
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 18

  alias AdventOfCode.Helpers.Transformers

  @repetitions 1..100
  @last_index 99
  @corners [{0, 0}, {0, @last_index}, {@last_index, 0}, {@last_index, @last_index}]

  def run(input \\ input!()) do
    grid = parse(input)

    {run_1(grid), run_2(grid)}
  end

  defp run_1(grid), do: steps(grid, &new_state/3)

  defp run_2(grid) do
    grid
    |> update_corners()
    |> steps(&new_state_with_faulty_bulbs/3)
  end

  def parse(data \\ input!()) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
    |> Transformers.grid2d()
  end

  defp steps(grid, state_computer) do
    @repetitions
    |> Enum.reduce(grid, fn _, acc ->
      iterate(acc, state_computer)
    end)
    |> Map.values()
    |> Enum.filter(&(&1 == "#"))
    |> Enum.count()
  end

  defp iterate(grid, state_computer) do
    Map.new(grid, fn {{x, y}, state} ->
      state_computer.(grid, {x, y}, state)
    end)
  end

  defp new_state(grid, {x, y}, state) do
    total_ons =
      grid
      |> get_neighbors({x, y})
      |> Enum.filter(&(&1 == "#"))
      |> Enum.count()

    state =
      case state do
        "#" -> (total_ons in [2, 3] && "#") || "."
        "." -> (total_ons == 3 && "#") || "."
      end

    {{x, y}, state}
  end

  defp get_neighbors(grid, {x, y}) do
    [
      {x + 1, y},
      {x - 1, y},
      {x, y + 1},
      {x, y - 1},
      {x + 1, y + 1},
      {x + 1, y - 1},
      {x - 1, y - 1},
      {x - 1, y + 1}
    ]
    |> Enum.map(fn coords -> grid[coords] end)
    |> Enum.reject(&is_nil/1)
  end

  defp update_corners(grid) do
    always_on =
      @corners
      |> Map.new(fn corner -> {corner, "#"} end)

    Map.merge(grid, always_on)
  end

  defp new_state_with_faulty_bulbs(grid, {x, y}, state) do
    case new_state(grid, {x, y}, state) do
      {corner, _} when corner in @corners -> {corner, "#"}
      state -> state
    end
  end
end
