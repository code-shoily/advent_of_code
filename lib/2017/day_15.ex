defmodule AdventOfCode.Y2017.Day15 do
  @moduledoc """
  --- Day 15: Dueling Generators ---
  Problem Link: https://adventofcode.com/2017/day/15
  Difficulty: l
  Tags: number-theory bitwise optimization
  """
  import Bitwise

  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 15)

  @mod 2_147_483_647

  def run(input \\ input()) do
    [a, b] = parse(input)

    # Use Task.async for parallelism
    part_1 = Task.async(fn -> solve_1(a, b, 0, 0) end)
    part_2 = Task.async(fn -> solve_2(a, b, 0, 0) end)

    {Task.await(part_1, :infinity), Task.await(part_2, :infinity)}
  end

  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [_, val] = Regex.run(~r/starts with (\d+)/, line)
      String.to_integer(val)
    end)
  end

  defp solve_1(_, _, 40_000_000, result), do: result

  defp solve_1(a, b, count, result) do
    na = next(a, 16_807)
    nb = next(b, 48_271)

    if (na &&& 0xFFFF) == (nb &&& 0xFFFF) do
      solve_1(na, nb, count + 1, result + 1)
    else
      solve_1(na, nb, count + 1, result)
    end
  end

  defp solve_2(_, _, 5_000_000, result), do: result

  defp solve_2(a, b, count, result) do
    na = next_filtered(a, 16_807, 3)
    nb = next_filtered(b, 48_271, 7)

    if (na &&& 0xFFFF) == (nb &&& 0xFFFF) do
      solve_2(na, nb, count + 1, result + 1)
    else
      solve_2(na, nb, count + 1, result)
    end
  end

  @compile {:inline, next: 2}
  defp next(val, mult) do
    prod = val * mult
    # Mersenne Prime (2^31 - 1) optimization
    res = (prod >>> 31) + (prod &&& @mod)
    if res >= @mod, do: res - @mod, else: res
  end

  defp next_filtered(val, mult, mask) do
    n = next(val, mult)
    # Using bitwise & mask for multiple_of(4) -> &&& 3 == 0, multiple_of(8) -> &&& 7 == 0
    if (n &&& mask) == 0, do: n, else: next_filtered(n, mult, mask)
  end
end
