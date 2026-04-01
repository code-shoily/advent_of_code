defmodule AdventOfCode.Y2015.Day20 do
  @moduledoc """
  --- Day 20: Infinite Elves and Infinite Houses ---
  Problem Link: https://adventofcode.com/2015/day/20
  Difficulty: l
  Tags: slow infinite-sequence sequence sieve optimization
  """

  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2015, 20)

  def run(input \\ input()) do
    target = String.trim(input) |> String.to_integer()

    task_1 = Task.async(fn -> solve_1(target) end)
    task_2 = Task.async(fn -> solve_2(target) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  # Part 1: Each elf i delivers 10*i gifts to every i-th house.
  # First house with >= target gifts.
  def solve_1(target) do
    # Upper bound: house `target/10` is guaranteed to get `target` gifts from elf `target/10`.
    limit = div(target, 10)
    arr = :atomics.new(limit, [{:signed, false}])

    # Sieve of Eratosthenes-style accumulation
    # O(N log N) total operations
    Enum.each(1..limit, fn elf ->
      val = elf * 10
      # Start from elf, go to limit by elf steps
      for house <- elf..limit//elf do
        :atomics.add(arr, house, val)
      end
    end)

    Enum.find(1..limit, fn h -> :atomics.get(arr, h) >= target end)
  end

  # Part 2: Each elf i delivers 11*i gifts to their first 50 houses.
  def solve_2(target) do
    limit = div(target, 11) + 1
    arr = :atomics.new(limit, [{:signed, false}])

    Enum.each(1..limit, fn elf ->
      val = elf * 11
      # Elf i delivers to i, 2i, ..., 50i
      # We take the first 50 houses but must not exceed limit
      steps = min(50, div(limit, elf))

      if steps > 0 do
        for s <- 1..steps do
          house = s * elf
          if house <= limit, do: :atomics.add(arr, house, val)
        end
      end
    end)

    Enum.find(1..limit, fn h -> :atomics.get(arr, h) >= target end)
  end
end
