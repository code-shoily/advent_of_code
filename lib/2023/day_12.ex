defmodule AdventOfCode.Y2023.Day12 do
  @moduledoc """
  --- Day 12: Hot Springs ---
  Problem Link: https://adventofcode.com/2023/day/12
  Difficulty: xl
  Tags: memoization vector
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Aja.Vector

  def input, do: InputReader.read_from_file(2023, 12)

  def run(input \\ input()) do
    input = parse(input)

    {total_arrangements(input, fn {spring, groups} ->
       arrangements({as_vector(spring), groups})
     end),
     total_arrangements(input, fn {spring, groups} ->
       arrangements(
         {[spring] |> List.duplicate(5) |> Enum.join("?") |> as_vector(),
          groups |> Vector.duplicate(5) |> Vector.flat_map(& &1)}
       )
     end)}
  end

  defp total_arrangements(input, fun) do
    for {:ok, count} <- Task.async_stream(input, fun), reduce: 0 do
      acc -> acc + count
    end
  end

  def parse(data \\ input()) do
    for line <- Transformers.lines(data) do
      [springs, data] = String.split(line, " ")
      {springs, Vector.new(Transformers.int_words(data, ","))}
    end
  end

  defp memoize(input, counts), do: Process.put(input, counts)
  defp memoized({_, _, _, _, _} = input), do: memoized(input, Process.get(input))
  defp memoized(input, nil), do: tap(arrangements(input), &memoize(input, &1))
  defp memoized(_, counts), do: counts
  defp as_vector(springs), do: Vector.new(String.graphemes(springs <> "."))
  defp arrangements({springs, groups}), do: arrangements({springs, groups, 0, 0, 0})

  defp arrangements({springs, groups, current, current_count, processed} = input) do
    case {Vector.at(springs, current), Vector.size(springs), Vector.size(groups)} do
      {_, ^current, ^processed} -> tap(1, &memoize(input, &1))
      {_, ^current, _} -> tap(0, &memoize(input, &1))
      {"#", _, _} -> memoized({springs, groups, current + 1, current_count + 1, processed})
      {".", _, _} -> arrangements(input, :operational)
      {_, _, ^processed} -> arrangements(input, :operational)
      _ -> arrangements(input, :unknown)
    end
  end

  defp arrangements({springs, groups, current, current_count, processed} = input, :operational) do
    if processed < Vector.size(groups) and current_count == Vector.at(groups, processed) do
      memoized({springs, groups, current + 1, 0, processed + 1})
    else
      (current_count == 0 && memoized({springs, groups, current + 1, 0, processed})) ||
        tap(0, &memoize(input, &1))
    end
  end

  defp arrangements({springs, groups, current, current_count, processed} = input, :unknown) do
    groups_processed = Vector.at(groups, processed)

    case current_count do
      ^groups_processed -> memoized({springs, groups, current + 1, 0, processed + 1})
      0 -> memoized({springs, groups, current + 1, 0, processed})
      _ -> 0
    end
    |> Kernel.+(memoized({springs, groups, current + 1, current_count + 1, processed}))
    |> tap(fn counts -> memoize(input, counts) end)
  end
end
