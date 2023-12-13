defmodule AdventOfCode.Y2023.Day12 do
  @moduledoc """
  --- Day 12: Hot Springs ---
  Problem Link: https://adventofcode.com/2023/day/12
  Difficulty: xl
  Tags: memoization vector slow
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Aja.Vector

  def input, do: InputReader.read_from_file(2023, 12)

  def run(input \\ input()) do
    input = parse(input)

    task_1 = Task.async(fn -> run_1(input) end)
    task_2 = Task.async(fn -> run_2(input) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  defp run_1(input) do
    for {spring, groups} <- input, reduce: 0 do
      acc ->
        acc + arrangements({as_vector(spring), groups})
    end
  end

  defp run_2(input) do
    for {spring, groups} <- input, reduce: 0 do
      acc ->
        acc +
          arrangements(
            {[spring] |> List.duplicate(5) |> Enum.join("?") |> as_vector(),
             groups |> Vector.duplicate(5) |> Vector.flat_map(& &1)}
          )
    end
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [springs, data] = String.split(line, " ")
      {springs, data |> Transformers.int_words(",") |> Vector.new()}
    end)
  end

  defp as_vector(springs) do
    springs
    |> Kernel.<>(".")
    |> String.codepoints()
    |> Vector.new()
  end

  @damaged "#"
  @operational "."
  defp arrangements({springs, groups}), do: arrangements({springs, groups, 0, 0, 0})

  defp arrangements({springs, groups, current, current_count, processed} = input) do
    cond do
      current == Vector.size(springs) ->
        input |> memoize(if Vector.size(groups) == processed, do: 1, else: 0)

      Vector.at(springs, current) == @damaged ->
        memoized({springs, groups, current + 1, current_count + 1, processed})

      Vector.at(springs, current) == @operational or Vector.size(groups) == processed ->
        input |> arrangements(:operational_or_done)

      true ->
        input |> arrangements(:unknown)
    end
  end

  defp arrangements(
         {springs, groups, current, current_count, processed} = input,
         :operational_or_done
       ) do
    if processed < Vector.size(groups) and current_count == Vector.at(groups, processed) do
      memoized({springs, groups, current + 1, 0, processed + 1})
    else
      (current_count == 0 &&
         memoized({springs, groups, current + 1, 0, processed})) ||
        memoize(input, 0)
    end
  end

  defp arrangements({springs, groups, current, current_count, processed} = input, :unknown) do
    damaged =
      memoized({springs, groups, current + 1, current_count + 1, processed})

    groups_processed = Vector.at(groups, processed)

    counts =
      case current_count do
        ^groups_processed ->
          damaged + memoized({springs, groups, current + 1, 0, processed + 1})

        0 ->
          damaged + memoized({springs, groups, current + 1, 0, processed})

        _ ->
          damaged
      end

    memoize(input, counts)
  end

  defp memoize(input, counts) do
    input
    |> Process.put(counts)
    |> then(fn _ -> counts end)
  end

  defp memoized(input) do
    case Process.get(input) do
      nil ->
        input
        |> arrangements()
        |> then(fn counts -> memoize(input, counts) end)

      data ->
        data
    end
  end
end
