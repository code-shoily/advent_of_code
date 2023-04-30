defmodule AdventOfCode.Y2019.Day04 do
  @moduledoc """
  --- Day 4: Secure Container ---
  Problem Link: https://adventofcode.com/2019/day/4
  """
  def input, do: 245_182..790_572

  def run(input \\ input()), do: {run_1(input), run_2(input)}

  def run_1(range) do
    Enum.count(for n <- range, n |> adjacent() and n |> increasing(), do: n)
  end

  def run_2(range) do
    Enum.count(for n <- range, n |> adjacent_2() and n |> increasing(), do: n)
  end

  def adjacent(number) do
    number
    |> Integer.digits()
    |> Enum.dedup()
    |> Kernel.!=(Integer.digits(number))
  end

  def increasing(number) do
    number
    |> Integer.digits()
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [a, b] -> a <= b end)
    |> Enum.all?()
  end

  def adjacent_2(number) do
    number
    |> Integer.digits()
    |> Enum.chunk_by(& &1)
    |> Enum.filter(fn dups -> Enum.count(dups) == 2 end)
    |> Enum.any?()
  end
end
