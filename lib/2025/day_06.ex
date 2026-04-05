defmodule AdventOfCode.Y2025.Day06 do
  @moduledoc """
  --- Day 6: Trash Compactor ---
  Problem Link: https://adventofcode.com/2025/day/6
  Difficulty: m
  Tags: grid math parsing
  """

  def input, do: File.read!("priv/input_files/2025_6.txt")

  def run(input \\ input()) do
    grid =
      input
      |> String.split("\n")
      |> Enum.reject(&(&1 == ""))

    width = grid |> Enum.map(&String.length/1) |> Enum.max()
    grid = Enum.map(grid, fn line -> String.pad_trailing(line, width) end)

    blocks = identify_blocks(grid, width)

    {solve_part1(grid, blocks), solve_part2(grid, blocks)}
  end

  defp identify_blocks(grid, width) do
    non_space_cols =
      Enum.reduce(0..(width - 1), [], fn j, acc ->
        if Enum.all?(grid, fn row -> String.at(row, j) == " " end) do
          acc
        else
          [j | acc]
        end
      end)
      |> Enum.reverse()

    non_space_cols
    |> Enum.chunk_while(
      [],
      fn
        j, [] -> {:cont, [j]}
        j, [last | _] = chunk when j == last + 1 -> {:cont, [j | chunk]}
        j, chunk -> {:cont, Enum.reverse(chunk), [j]}
      end,
      fn
        [] -> {:cont, []}
        chunk -> {:cont, Enum.reverse(chunk), []}
      end
    )
    |> Enum.map(fn chunk -> {List.first(chunk), length(chunk)} end)
  end

  defp solve_part1(grid, blocks) do
    height = length(grid)

    blocks
    |> Enum.map(fn {start, len} ->
      numbers =
        for row_idx <- 0..(height - 2) do
          grid
          |> Enum.at(row_idx)
          |> String.slice(start, len)
          |> String.replace(" ", "")
        end
        |> Enum.reject(&(&1 == ""))
        |> Enum.map(&String.to_integer/1)

      op = get_op(grid, {start, len})
      apply_op(numbers, op)
    end)
    |> Enum.sum()
  end

  defp solve_part2(grid, blocks) do
    height = length(grid)

    blocks
    |> Enum.reverse()
    |> Enum.map(fn {start, len} ->
      numbers =
        for j <- (start + len - 1)..start//-1 do
          digits =
            for row_idx <- 0..(height - 2) do
              String.at(Enum.at(grid, row_idx), j)
            end
            |> Enum.join()
            |> String.replace(" ", "")

          if digits == "", do: nil, else: String.to_integer(digits)
        end
        |> Enum.reject(&is_nil/1)

      op = get_op(grid, {start, len})
      apply_op(numbers, op)
    end)
    |> Enum.sum()
  end

  defp get_op(grid, {start, len}) do
    grid
    |> List.last()
    |> String.slice(start, len)
    |> String.trim()
  end

  defp apply_op([n | rest], "+") do
    Enum.reduce(rest, n, &Kernel.+/2)
  end

  defp apply_op([n | rest], "*") do
    Enum.reduce(rest, n, &Kernel.*/2)
  end

  def parse(data \\ input()) do
    data
    |> String.split("\n", trim: true)
  end
end
