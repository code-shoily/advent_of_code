defmodule AdventOfCode.Y2020.Day23 do
  @moduledoc """
  --- Day 23: Crab Cups ---
  Problem Link: https://adventofcode.com/2020/day/23
  Difficulty: l
  Tags: circular-linked-list atomics
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

  defp solve(initial_cups, moves, total_cups) do
    arr = :atomics.new(total_cups + 1, [])
    setup_pointers(arr, initial_cups, total_cups)

    [first | _] = initial_cups
    play(arr, first, moves, total_cups)

    if total_cups == 9 do
      read_after(arr, 1, 8)
    else
      c1 = :atomics.get(arr, 1)
      c2 = :atomics.get(arr, c1)
      c1 * c2
    end
  end

  defp setup_pointers(arr, initial, total) do
    max_init = Enum.max(initial)
    [first | _] = initial
    last_init = List.last(initial)

    initial
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.each(fn [u, v] -> :atomics.put(arr, u, v) end)

    if total > length(initial) do
      :atomics.put(arr, last_init, max_init + 1)

      if total > max_init + 1 do
        Enum.each((max_init + 1)..(total - 1), fn i -> :atomics.put(arr, i, i + 1) end)
      end

      :atomics.put(arr, total, first)
    else
      :atomics.put(arr, last_init, first)
    end
  end

  defp play(_arr, _current, 0, _max), do: :ok

  defp play(arr, current, moves, max) do
    c1 = :atomics.get(arr, current)
    c2 = :atomics.get(arr, c1)
    c3 = :atomics.get(arr, c2)
    after_c3 = :atomics.get(arr, c3)

    d1 = if current == 1, do: max, else: current - 1

    dest =
      if d1 == c1 or d1 == c2 or d1 == c3 do
        d2 = if d1 == 1, do: max, else: d1 - 1

        if d2 == c1 or d2 == c2 or d2 == c3 do
          d3 = if d2 == 1, do: max, else: d2 - 1

          if d3 == c1 or d3 == c2 or d3 == c3,
            do: if(d3 == 1, do: max, else: d3 - 1),
            else: d3
        else
          d2
        end
      else
        d1
      end

    after_dest = :atomics.get(arr, dest)

    :atomics.put(arr, current, after_c3)
    :atomics.put(arr, dest, c1)
    :atomics.put(arr, c3, after_dest)

    play(arr, after_c3, moves - 1, max)
  end

  defp read_after(arr, current, count) do
    Enum.reduce(1..count, {[], current}, fn _, {acc, curr} ->
      nxt = :atomics.get(arr, curr)
      {[nxt | acc], nxt}
    end)
    |> elem(0)
    |> Enum.reverse()
    |> Enum.join()
  end
end
