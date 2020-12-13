defmodule AdventOfCode.Y2020.Day13 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/13
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 13

  import AdventOfCode.Helpers.ChineseRemainder

  def run_1, do: input!() |> process_1() |> earliest_bus() |> result()
  def run_2, do: input!() |> process_2() |> compute()

  def process_1(input \\ input!()) do
    [time, ids] = String.split(input, "\n", trim: true)

    {String.to_integer(time),
     ids |> String.split(",") |> Enum.reject(&(&1 == "x")) |> Enum.map(&String.to_integer/1)}
  end

  def process_2(input \\ input!()) do
    input
    |> String.split("\n", trim: true)
    |> Enum.at(-1)
    |> String.split(",")
    |> Enum.with_index()
    |> Enum.reject(fn {x, _} -> x == "x" end)
    |> Enum.map(fn {x, y} -> {String.to_integer(x), y} end)
  end

  def compute(list) do
    list
    |> Enum.map(fn {v, idx} -> {v, v - idx} end)
    |> chinese_remainder()
  end

  defp next_departure(id, time) do
    next_departure = (div(time, id) + 1) * id
    {id, next_departure, next_departure - time}
  end

  defp earliest_bus({time, ids}) do
    Enum.min_by(Enum.map(ids, &next_departure(&1, time)), &elem(&1, 2))
  end

  defp result({id, _, diff}), do: id * diff
end
