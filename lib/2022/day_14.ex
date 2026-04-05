defmodule AdventOfCode.Y2022.Day14 do
  @moduledoc """
  --- Day 14: Regolith Reservoir ---
  Problem Link: https://adventofcode.com/2022/day/14
  Difficulty: l
  Tags: flood-fill grid implicit-graph
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Traversal.Implicit

  @source {500, 0}

  def input, do: InputReader.read_from_file(2022, 14)

  def run(input \\ input()) do
    rocks = parse(input)
    max_y = rocks |> Enum.map(&elem(&1, 1)) |> Enum.max()

    {solve_1(rocks, max_y), solve_2(rocks, max_y + 2)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.reduce(MapSet.new(), fn line, acc ->
      line
      |> String.split(" -> ")
      |> Enum.map(fn pair ->
        pair |> String.split(",") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      end)
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.reduce(acc, fn [{x1, y1}, {x2, y2}], acc_inner ->
        {min_x, max_x} = Enum.min_max([x1, x2])
        {min_y, max_y} = Enum.min_max([y1, y2])

        for x <- min_x..max_x, y <- min_y..max_y, into: acc_inner do
          {x, y}
        end
      end)
    end)
  end

  def solve_1(rocks, max_y) do
    simulate_1(rocks, max_y)
  end

  def solve_2(rocks, floor) do
    # Part 2: Reachable air nodes above the floor.
    # Every reachable spot will eventually be filled with sand.
    Implicit.implicit_fold(
      from: @source,
      using: :breadth_first,
      initial: 0,
      successors_of: fn {x, y} ->
        if y + 1 < floor do
          [{x, y + 1}, {x - 1, y + 1}, {x + 1, y + 1}]
          |> Enum.filter(&(!MapSet.member?(rocks, &1)))
        else
          []
        end
      end,
      with: fn acc, _node, _meta -> {:continue, acc + 1} end
    )
  end

  defp simulate_1(rocks, max_y) do
    Stream.unfold(rocks, fn state ->
      case pour_one(state, max_y, @source) do
        {:rest, new_state} -> {1, new_state}
        :abyss -> nil
      end
    end)
    |> Enum.sum()
  end

  defp pour_one(state, max_y, {x, y}) do
    if y >= max_y do
      :abyss
    else
      down = {x, y + 1}
      left = {x - 1, y + 1}
      right = {x + 1, y + 1}

      case Enum.find([down, left, right], &(!MapSet.member?(state, &1))) do
        nil -> {:rest, MapSet.put(state, {x, y})}
        next -> pour_one(state, max_y, next)
      end
    end
  end
end
