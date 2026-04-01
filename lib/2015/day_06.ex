defmodule AdventOfCode.Y2015.Day06 do
  @moduledoc """
  --- Day 6: Probably a Fire Hazard ---
  Problem Link: https://adventofcode.com/2015/day/6
  Difficulty: m
  Tags: grid vector reduction
  """
  import Bitwise
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2015, 6)

  def run(input \\ input()) do
    parsed_input = parse(input)

    task_1 = Task.async(fn -> part_1(parsed_input) end)
    task_2 = Task.async(fn -> part_2(parsed_input) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def parse(input \\ input()) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_input/1)
  end

  # Part 1 implementation using bitwise operations on rows (represented as 1000-bit integers)
  defp part_1(instructions) do
    # Initial grid: 1000 rows of all zeros (represented as 0)
    grid = Tuple.duplicate(0, 1000)

    instructions
    |> Enum.reduce(grid, fn {cmd, x1..x2//_, y1..y2//_}, acc ->
      mask = ((1 <<< (x2 - x1 + 1)) - 1) <<< x1

      Enum.reduce(y1..y2, acc, fn y, rows ->
        row = elem(rows, y)

        new_row =
          case cmd do
            "turn on" -> row ||| mask
            "turn off" -> row &&& bnot(mask)
            "toggle" -> bxor(row, mask)
          end

        put_elem(rows, y, new_row)
      end)
    end)
    |> Tuple.to_list()
    |> Enum.reduce(0, fn row, acc -> acc + popcount(row) end)
  end

  # Part 2 implementation using :counters for mutable-like performance
  defp part_2(instructions) do
    c = :counters.new(1_000_000, [])

    Enum.each(instructions, fn {cmd, x1..x2//_, y1..y2//_} ->
      case cmd do
        "turn on" ->
          for y <- y1..y2 do
            offset = y * 1000 + 1
            for x <- x1..x2, do: :counters.add(c, offset + x, 1)
          end

        "turn off" ->
          for y <- y1..y2 do
            offset = y * 1000 + 1

            for x <- x1..x2 do
              idx = offset + x
              if :counters.get(c, idx) > 0, do: :counters.add(c, idx, -1)
            end
          end

        "toggle" ->
          for y <- y1..y2 do
            offset = y * 1000 + 1
            for x <- x1..x2, do: :counters.add(c, offset + x, 2)
          end
      end
    end)

    # Sum all values
    Enum.reduce(1..1_000_000, 0, fn i, acc -> acc + :counters.get(c, i) end)
  end

  # High performance popcount for large integers
  defp popcount(0), do: 0

  defp popcount(n) do
    n
    |> :binary.encode_unsigned()
    |> count_binary_ones(0)
  end

  defp count_binary_ones(<<>>, acc), do: acc

  defp count_binary_ones(<<b, rest::binary>>, acc) do
    count_binary_ones(
      rest,
      acc +
        for <<(bit::1 <- <<b::8>>)>>, reduce: 0 do
          a -> a + bit
        end
    )
  end

  @regex ~r"(?<cmd>toggle|turn\son|turn\soff)\s(?<x1>\d+),(?<y1>\d+)\s\S+\s(?<x2>\d+),(?<y2>\d+)"
  defp parse_input(line) do
    %{"cmd" => cmd, "x1" => x1, "x2" => x2, "y1" => y1, "y2" => y2} =
      Regex.named_captures(@regex, line)

    {cmd, String.to_integer(x1)..String.to_integer(x2),
     String.to_integer(y1)..String.to_integer(y2)}
  end
end
