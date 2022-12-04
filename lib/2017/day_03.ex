defmodule AdventOfCode.Y2017.Day03 do
  @moduledoc """
  --- Day 3: Spiral Memory ---
  Problem Link: https://adventofcode.com/2017/day/3
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 3)

  def run(input \\ input()) do
    input = List.first(Transformers.int_lines(input))
    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    {x, y} = position(input)
    round(abs(x) + abs(y))
  end

  defp run_2(input), do: allocations(input, 2, %{{0, 0} => 1}, 1)

  def position(idx) do
    r = div(1 + trunc(:math.sqrt(idx - 1)), 2)
    d = 2 * r - 1
    i = idx - d ** 2 - 1

    case {i < d, i < 2 * d + 2, i < 3 * d + 2} do
      {true, _, _} -> {r, i - r + 1}
      {_, true, _} -> {r - i + d, r}
      {_, _, true} -> {-r, r - i - 1 + 2 * d + 2}
      _ -> {i - r - 3 * d - 2, -r}
    end
  end

  defp allocations(idx, _i, _cache, result) when result > idx, do: result

  defp allocations(idx, i, cache, _result) do
    {x, y} = position(i)

    result =
      Enum.sum(
        Enum.map(
          -3..5,
          &Map.get(cache, {x + Integer.mod(&1, 3) - 1, y + Integer.floor_div(&1, 3)}, 0)
        )
      )

    allocations(idx, i + 1, Map.put(cache, {x, y}, result), result)
  end
end
