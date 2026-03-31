defmodule AdventOfCode.Y2020.Day23 do
  @moduledoc """
  --- Day 23: Crab Cups ---
  Problem Link: https://adventofcode.com/2020/day/23
  Difficulty: l
  Tags: circular-linked-list slow
  """
  def input, do: "467528193"

  def run(input \\ input()) do
    cups = parse(input)

    {
      solve(cups, 100, 9),
      solve(cups, 10_000_000, 1_000_000)
    }
  end

  def parse(input \\ input()) do
    input |> String.graphemes() |> Enum.map(&String.to_integer/1)
  end

  # Part 1 and Part 2 use the same logic, but different constraints
  defp solve(initial_cups, moves, total_cups) do
    # 1. Build the circular linked list using :counters (O(1) mutable array)
    arr = :counters.new(total_cups + 1, [:write_concurrency])

    # Fill initial cups
    initial_len = length(initial_cups)
    max_initial = Enum.max(initial_cups)

    initial_cups
    |> Enum.chunk_every(2, 1, [])
    |> Enum.each(fn
      [curr, nxt] ->
        :counters.put(arr, curr, nxt)

      [last] ->
        if total_cups > initial_len do
          :counters.put(arr, last, max_initial + 1)
        else
          :counters.put(arr, last, hd(initial_cups))
        end
    end)

    # Fill the rest for Part 2
    if total_cups > initial_len do
      Enum.each((max_initial + 1)..total_cups, fn i ->
        if i == total_cups do
          :counters.put(arr, i, hd(initial_cups))
        else
          :counters.put(arr, i, i + 1)
        end
      end)
    end

    first_cup = hd(initial_cups)

    play(arr, first_cup, moves, total_cups)

    if total_cups == 9 do
      # Part 1: Labels after cup 1
      read_after(arr, 1, 8)
    else
      # Part 2: Product of two cups after cup 1
      c1 = :counters.get(arr, 1)
      c2 = :counters.get(arr, c1)
      c1 * c2
    end
  end

  defp play(_arr, _current, 0, _), do: :ok

  defp play(arr, current, moves, max) do
    # 1. Pick up three cups immediately clockwise of current
    c1 = :counters.get(arr, current)
    c2 = :counters.get(arr, c1)
    c3 = :counters.get(arr, c2)
    after_picked = :counters.get(arr, c3)

    # 2. Select destination cup
    dest = select_destination(current - 1, c1, c2, c3, max)

    # 3. Update pointers to "insert" cups
    after_dest = :counters.get(arr, dest)

    :counters.put(arr, current, after_picked)
    :counters.put(arr, dest, c1)
    :counters.put(arr, c3, after_dest)

    play(arr, after_picked, moves - 1, max)
  end

  defp select_destination(0, c1, c2, c3, max), do: select_destination(max, c1, c2, c3, max)

  defp select_destination(dest, c1, c2, c3, max) do
    if dest == c1 or dest == c2 or dest == c3 do
      select_destination(dest - 1, c1, c2, c3, max)
    else
      dest
    end
  end

  defp read_after(arr, start, count) do
    Enum.reduce(1..count, {[], start}, fn _, {acc, curr} ->
      nxt = :counters.get(arr, curr)
      {[nxt | acc], nxt}
    end)
    |> elem(0)
    |> Enum.reverse()
    |> Enum.join()
  end
end
