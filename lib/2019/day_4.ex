defmodule AdventOfCode.Y2019.Day4 do
  @moduledoc """
  Problem description: Problem description: https://adventofcode.com/2019/day/4
  """
  @range 245_182..790_572

  def run, do: {run_1(), run_2()}

  def run_1 do
    Enum.count(for n <- @range, n |> adjacent() and n |> increasing(), do: n)
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

  def run_2 do
    Enum.count(for n <- @range, n |> adjacent_2() and n |> increasing(), do: n)
  end

  def adjacent_2(number) do
    number
    |> Integer.digits()
    |> Enum.chunk_by(& &1)
    |> Enum.filter(fn dups -> Enum.count(dups) == 2 end)
    |> Enum.any?()
  end
end
