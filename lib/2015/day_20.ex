defmodule AdventOfCode.Y2015.Day20 do
  @moduledoc """
  --- Day 20: Infinite Elves and Infinite Houses ---
  Problem Link: https://adventofcode.com/2015/day/20
  """
  alias AdventOfCode.Algorithms.Arithmetics
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2015, 20)

  def run(input \\ input()) do
    input = String.to_integer(input)

    part_1 = Task.async(fn -> run_1(input) end)
    part_2 = Task.async(fn -> run_2(input) end)

    {Task.await(part_1, 10_000), Task.await(part_2, 10_000)}
  end

  defp run_1(input) do
    house_with_gifts(input, &number_of_gifts/1)
  end

  defp run_2(input) do
    house_with_gifts(input, &number_of_gifts_below_50/1)
  end

  def house_with_gifts(limit, fun) do
    Stream.iterate(1, &(&1 + 1))
    |> Stream.map(fun)
    |> Enum.take_while(fn {_, gifts} -> gifts < limit end)
    |> Enum.max_by(fn {house, _} -> house end)
    |> then(fn {house, _} -> house + 1 end)
  end

  defp number_of_gifts(house) do
    gifts =
      house
      |> Arithmetics.divisors()
      |> Enum.map(&(&1 * 10))
      |> Enum.sum()

    {house, gifts}
  end

  defp number_of_gifts_below_50(house) do
    gifts =
      house
      |> Arithmetics.divisors()
      |> Enum.filter(fn divisor -> div(house, divisor) < 50 end)
      |> Enum.map(&(&1 * 11))
      |> Enum.sum()

    {house, gifts}
  end
end
