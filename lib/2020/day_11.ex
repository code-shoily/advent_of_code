defmodule AdventOfCode.Y2020.Day11 do
  @moduledoc """
  --- Day 11: Seating System ---
  Problem Link: https://adventofcode.com/2020/day/11
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 11

  def run_1, do: input!() |> parse() |> seats(&adjacents/3, 4)
  def run_2, do: input!() |> parse() |> seats(&adjacents_view/3, 5)

  defp seats(data, strategy, limit) do
    data
    |> evolve(strategy, fn adj -> Enum.all?(adj, &(&1 != "#")) end, fn adj ->
      Enum.count(adj, &(&1 == "#")) >= limit
    end)
    |> Enum.filter(fn {_, seat} -> seat == "#" end)
    |> length()
  end

  def parse(input) do
    input = String.split(input, "\n")
    size = length(input)

    grid =
      for {row, y} <- Enum.with_index(input),
          {seat, x} <- Enum.with_index(String.graphemes(row)),
          do: {{x, y}, seat}

    {Map.new(grid), size, size}
  end

  def evolve({grid, rows, cols}, adjacents_fn, occupy?, free?) do
    Enum.reduce_while(Stream.cycle([0]), grid, fn _, grid ->
      changes =
        for(x <- 0..(cols - 1), y <- 0..(rows - 1), do: {x, y})
        |> Enum.reduce([], fn {x, y}, changes ->
          case Map.get(grid, {x, y}) do
            "." ->
              changes

            seat ->
              adjacents = adjacents_fn.(grid, x, y)

              cond do
                seat == "L" && occupy?.(adjacents) -> [{x, y, "#"} | changes]
                seat == "#" && free?.(adjacents) -> [{x, y, "L"} | changes]
                true -> changes
              end
          end
        end)

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
        {new_x, new_y} = {x + shift_x, y + shift_y}

        case Map.get(grid, {new_x, new_y}) do
          "." -> {:cont, {acc, new_x, new_y}}
          nil -> {:halt, acc}
          seat -> {:halt, [seat | acc]}
        end
      end)
    end)
  end
end
