defmodule AdventOfCode.Y2015.Day18 do
  @moduledoc """
  --- Day 18: Like a GIF For Your Yard ---
  Problem Link: https://adventofcode.com/2015/day/18
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @input InputReader.read_from_file(2015, 18)
  @repetitions 1..100
  @last_index 99
  @corners [{0, 0}, {0, @last_index}, {@last_index, 0}, {@last_index, @last_index}]

  def run(input \\ @input) do
    grid = parse(input)

    solution_1 = Task.async(fn -> steps(grid, &state/3) end)

    solution_2 =
      Task.async(fn -> grid |> update_corners() |> steps(&state_with_faulty_corners/3) end)

    {
      Task.await(solution_1, 10_000),
      Task.await(solution_2, 10_000)
    }
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
    |> Transformers.grid2d()
  end

  defp steps(grid, tx) do
    @repetitions
    |> Enum.reduce(grid, fn _, acc -> iterate(acc, tx) end)
    |> Enum.count(fn {_, light} -> light == "#" end)
  end

  defp iterate(grid, tx) do
    Map.new(grid, fn {{x, y}, state} ->
      tx.(grid, {x, y}, state)
    end)
  end

  defp state(grid, {x, y}, state) do
    ons = grid |> get_neighbors({x, y}) |> Enum.count(&(&1 == "#"))

    state =
      case state do
        "#" -> (ons in [2, 3] && "#") || "."
        "." -> (ons == 3 && "#") || "."
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
    Map.merge(grid, Map.new(@corners, &{&1, "#"}))
  end

  defp state_with_faulty_corners(grid, {x, y}, state) do
    case state(grid, {x, y}, state) do
      {corner, _} when corner in @corners -> {corner, "#"}
      state -> state
    end
  end
end
