defmodule AdventOfCode.Y2020.Day11 do
  @moduledoc """
  --- Day 11: Seating System ---
  Problem Link: https://adventofcode.com/2020/day/11
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 11)

  def run(input \\ input()) do
    input = parse(input)
    task_1 = Task.async(fn -> run_1(input) end)
    task_2 = Task.async(fn -> run_2(input) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def run_1(input), do: seats(input, &adjacents/3, 4)
  def run_2(input), do: seats(input, &adjacents_view/3, 5)

  defp seats(data, strategy, limit) do
    data
    |> evolve(strategy, fn adj -> Enum.all?(adj, &(&1 != "#")) end, fn adj ->
      Enum.count(adj, &(&1 == "#")) >= limit
    end)
    |> Enum.filter(fn {_, seat} -> seat == "#" end)
    |> length()
  end

  def parse(input) do
    input = Transformers.lines(input)
    size = length(input)

    grid =
      for {row, y} <- Enum.with_index(input),
          {seat, x} <- Enum.with_index(String.graphemes(row)),
          do: {{x, y}, seat}

    {Map.new(grid), size, size}
  end

  def update_changes(adjacents, seat, changes, x, y, occupy?, free?) do
    cond do
      seat == "L" && occupy?.(adjacents) -> [{x, y, "#"} | changes]
      seat == "#" && free?.(adjacents) -> [{x, y, "L"} | changes]
      true -> changes
    end
  end

  def get_changes(grid, rows, cols, adjacents_fn, occupy?, free?) do
    for(x <- 0..(cols - 1), y <- 0..(rows - 1), do: {x, y})
    |> Enum.reduce([], fn {x, y}, changes ->
      case Map.get(grid, {x, y}) do
        "." ->
          changes

        seat ->
          grid
          |> adjacents_fn.(x, y)
          |> update_changes(seat, changes, x, y, occupy?, free?)
      end
    end)
  end

  def evolve({grid, rows, cols}, adjacents_fn, occupy?, free?) do
    Enum.reduce_while(Stream.cycle([0]), grid, fn _, grid ->
      changes = get_changes(grid, rows, cols, adjacents_fn, occupy?, free?)

      (Enum.empty?(changes) && {:halt, grid}) ||
        {:cont,
         Enum.reduce(changes, grid, fn {x, y, new_val}, grid ->
           Map.put(grid, {x, y}, new_val)
         end)}
    end)
  end

  def adjacents(grid, x, y),
    do:
      Enum.map(
        [
          {x + 1, y},
          {x + 1, y - 1},
          {x + 1, y + 1},
          {x, y + 1},
          {x, y - 1},
          {x - 1, y},
          {x - 1, y - 1},
          {x - 1, y + 1}
        ],
        fn {x, y} -> Map.get(grid, {x, y}) end
      )

  def adjacents_view(grid, x, y) do
    [{1, 1}, {1, 0}, {1, -1}, {0, 1}, {0, -1}, {-1, 1}, {-1, 0}, {-1, -1}]
    |> Enum.reduce([], fn {shift_x, shift_y}, acc ->
      Enum.reduce_while(Stream.cycle([0]), {acc, x, y}, fn _, {acc, x, y} ->
        reducing_cond(grid, {x + shift_x, y + shift_y}, acc)
      end)
    end)
  end

  defp reducing_cond(grid, {new_x, new_y}, acc) do
    case Map.get(grid, {new_x, new_y}) do
      "." -> {:cont, {acc, new_x, new_y}}
      nil -> {:halt, acc}
      seat -> {:halt, [seat | acc]}
    end
  end
end
