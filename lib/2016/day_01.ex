defmodule AdventOfCode.Y2016.Day01 do
  @moduledoc """
  --- Day 1: No Time for a Taxicab ---
  Problem Link: https://adventofcode.com/2016/day/1
  Difficulty: s
  Tags: grid measurement set rust
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2016, 1)

  def run(input \\ input()) do
    input = Transformers.words(input, ",")

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    input
    |> move()
    |> Enum.reverse()
    |> hd()
    |> blocks()
  end

  defp run_2(input) do
    input
    |> move()
    |> merge_points()
    |> first_duplicate(MapSet.new())
    |> blocks()
  end

  defp move(directions), do: move(:origin, directions, [0, 0], [])

  defp move(_, [], _, positions), do: positions

  defp move(:origin, ["L" <> steps | rest], [x, y], positions) do
    new_position = [x - String.to_integer(steps), y]
    move(:west, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:origin, ["R" <> steps | rest], [x, y], positions) do
    new_position = [x + String.to_integer(steps), y]
    move(:east, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:north, ["L" <> steps | rest], [x, y], positions) do
    new_position = [x - String.to_integer(steps), y]
    move(:west, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:north, ["R" <> steps | rest], [x, y], positions) do
    new_position = [x + String.to_integer(steps), y]
    move(:east, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:south, ["L" <> steps | rest], [x, y], positions) do
    new_position = [x + String.to_integer(steps), y]
    move(:east, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:south, ["R" <> steps | rest], [x, y], positions) do
    new_position = [x - String.to_integer(steps), y]
    move(:west, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:west, ["L" <> steps | rest], [x, y], positions) do
    new_position = [x, y - String.to_integer(steps)]
    move(:south, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:west, ["R" <> steps | rest], [x, y], positions) do
    new_position = [x, y + String.to_integer(steps)]
    move(:north, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:east, ["L" <> steps | rest], [x, y], positions) do
    new_position = [x, y + String.to_integer(steps)]
    move(:north, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp move(:east, ["R" <> steps | rest], [x, y], positions) do
    new_position = [x, y - String.to_integer(steps)]
    move(:south, rest, new_position, positions ++ positions_between([x, y], new_position))
  end

  defp positions_between([x1, y], [x2, y]), do: x1..x2 |> Enum.map(&[&1, y])
  defp positions_between([x, y1], [x, y2]), do: y1..y2 |> Enum.map(&[x, &1])

  defp merge_points(positions), do: positions |> Enum.chunk_by(& &1) |> Enum.map(&hd(&1))

  defp blocks([x, y]), do: abs(x) + abs(y)

  defp first_duplicate([position | rest], history),
    do:
      (MapSet.member?(history, position) && position) ||
        first_duplicate(rest, MapSet.put(history, position))
end
