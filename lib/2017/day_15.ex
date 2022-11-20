defmodule AdventOfCode.Y2017.Day15 do
  @moduledoc """
  --- Day 15: Dueling Generators ---
  Problem Link: https://adventofcode.com/2017/day/15
  FIXME: Number being Marsenne Prime, can't help but think there's a faster way to this.
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 15

  alias AdventOfCode.Helpers.Transformers

  import Bitwise

  @multipliers %{"A" => 16_807, "B" => 48_271}

  def run(input \\ input!()) do
    input = parse(input)

    part_1 = Task.async(fn -> final_count_1(input) end)
    part_2 = Task.async(fn -> final_count_2(input) end)

    {
      Task.await(part_1, :infinity),
      Task.await(part_2, :infinity)
    }
  end

  @regex ~r"Generator (A|B) starts with (\d+)"
  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [name, start] = Regex.run(@regex, line, capture: :all_but_first)

      {String.to_integer(start), @multipliers[name]}
    end)
  end

  defp final_count_1([{a, multiplier_a}, {b, multiplier_b}]) do
    get_final_count(
      40_000_000,
      [generator(a, multiplier_a), generator(b, multiplier_b)]
    )
  end

  defp final_count_2([{a, multiplier_a}, {b, multiplier_b}]) do
    get_final_count(5_000_000, [
      generator(a, multiplier_a, multiple_of(4)),
      generator(b, multiplier_b, multiple_of(8))
    ])
  end

  defp generator(initial, multiplier, filter \\ & &1) do
    initial
    |> next(multiplier)
    |> Stream.iterate(&next(&1, multiplier))
    |> filter.()
  end

  defp get_final_count(limit, [a, b]) do
    [a, b]
    |> Stream.zip()
    |> Enum.take(limit)
    |> Enum.count(&same_lowest_bits?/1)
  end

  @mask (1 <<< 16) - 1
  defp same_lowest_bits?({a, b}), do: bxor(a &&& @mask, b &&& @mask) == 0

  defp multiple_of(amount), do: &Stream.filter(&1, fn val -> rem(val, amount) == 0 end)
  defp next(prev, multiplier), do: rem(prev * multiplier, 2_147_483_647)
end
