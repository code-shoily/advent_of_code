defmodule AdventOfCode.Y2023.Day14 do
  @moduledoc """
  --- Day 14: Parabolic Reflector Dish ---
  Problem Link: https://adventofcode.com/2023/day/14
  Difficulty: l
  Tags: grid grid-rotation modular-arithmetic memoization
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 14)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input), do: input |> tilt() |> get_load()

  @cycle 1_000_000_000
  def run_2(input) do
    1..@cycle
    |> Enum.reduce_while({input, %{}}, fn idx, {dish, cache} ->
      new_dish = roll(dish)
      hash = :erlang.phash2(new_dish)

      case cache[hash] do
        nil ->
          {:cont, {new_dish, Map.put(cache, hash, idx)}}

        existing_idx ->
          diff = idx - existing_idx
          {:halt, {new_dish, @cycle - div(@cycle - existing_idx, diff) * diff - existing_idx - 1}}
      end
    end)
    |> then(fn {dish, n} -> Enum.reduce(0..n, dish, fn _, acc -> roll(acc) end) end)
    |> get_load()
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
    |> turn()
  end

  defp roll(dish), do: Enum.reduce(1..4, dish, fn _, acc -> turn(tilt(acc)) end)

  defp tilt(dish) do
    Enum.map(dish, fn row ->
      row
      |> Enum.chunk_by(&(&1 == "#"))
      |> Enum.map(&Enum.sort/1)
      |> Enum.flat_map(& &1)
    end)
  end

  defp turn(dish) do
    dish
    |> Transformers.transpose()
    |> Enum.map(&Enum.reverse/1)
  end

  defp get_load(tilted_dish) do
    tilted_dish
    |> Enum.flat_map(fn row -> Enum.with_index(row, 1) end)
    |> Enum.reduce(0, fn {value, idx}, acc -> acc + ((value == "O" && idx) || 0) end)
  end
end
