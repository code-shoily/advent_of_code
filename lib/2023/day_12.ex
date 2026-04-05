defmodule AdventOfCode.Y2023.Day12 do
  @moduledoc """
  --- Day 12: Hot Springs ---
  Problem Link: https://adventofcode.com/2023/day/12
  Difficulty: xl
  Tags: dynamic-programming
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 12)

  def run(input \\ input()) do
    parsed = parse(input)

    res1 = compute(parsed, 1)
    res2 = compute(parsed, 5)

    {res1, res2}
  end

  defp compute(parsed, multiplier) do
    parsed
    |> Task.async_stream(
      fn {springs, groups} ->
        unfolded_springs = List.duplicate(springs, multiplier) |> Enum.join("?")
        unfolded_groups = List.duplicate(groups, multiplier) |> List.flatten() |> List.to_tuple()

        Process.get_keys() |> Enum.each(&Process.delete/1)
        do_count(unfolded_springs, unfolded_groups, 0, 0, 0)
      end,
      timeout: :infinity
    )
    |> Enum.reduce(0, fn {:ok, count}, acc -> acc + count end)
  end

  def parse(data) do
    for line <- Transformers.lines(data) do
      [springs, groups] = String.split(line, " ")
      {springs, Transformers.int_words(groups, ",")}
    end
  end

  defp do_count(springs, groups, s_idx, g_idx, current_count) do
    key = {s_idx, g_idx, current_count}

    case Process.get(key) do
      nil ->
        res = calc(springs, groups, s_idx, g_idx, current_count)
        Process.put(key, res)
        res

      val ->
        val
    end
  end

  defp calc(springs, groups, s_idx, g_idx, current_count) do
    if s_idx == byte_size(springs) do
      cond do
        g_idx == tuple_size(groups) and current_count == 0 -> 1
        g_idx == tuple_size(groups) - 1 and current_count == elem(groups, g_idx) -> 1
        true -> 0
      end
    else
      char = :binary.at(springs, s_idx)

      case char do
        ?# ->
          handle_hash(springs, groups, s_idx, g_idx, current_count)

        ?. ->
          handle_dot(springs, groups, s_idx, g_idx, current_count)

        ?? ->
          handle_dot(springs, groups, s_idx, g_idx, current_count) +
            handle_hash(springs, groups, s_idx, g_idx, current_count)
      end
    end
  end

  defp handle_hash(springs, groups, s_idx, g_idx, current_count) do
    if g_idx < tuple_size(groups) and current_count < elem(groups, g_idx) do
      do_count(springs, groups, s_idx + 1, g_idx, current_count + 1)
    else
      0
    end
  end

  defp handle_dot(springs, groups, s_idx, g_idx, current_count) do
    case current_count do
      0 ->
        do_count(springs, groups, s_idx + 1, g_idx, 0)

      n ->
        if g_idx < tuple_size(groups) and n == elem(groups, g_idx) do
          do_count(springs, groups, s_idx + 1, g_idx + 1, 0)
        else
          0
        end
    end
  end
end
