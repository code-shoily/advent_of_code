defmodule AdventOfCode.Y2020.Day13 do
  @moduledoc """
  --- Day 13: Shuttle Search ---
  Problem Link: https://adventofcode.com/2020/day/13
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 13

  alias ExAlgo.Number.ChineseRemainder

  def run_1, do: input!() |> parse_1() |> earliest_bus() |> elem(3)
  def run_2, do: input!() |> parse_2() |> ChineseRemainder.compute()

  def parse_1(input) do
    [time, ids] = String.split(input, "\n", trim: true)

    {String.to_integer(time),
     ids
     |> String.split(",")
     |> Enum.reject(&(&1 == "x"))
     |> Enum.map(&String.to_integer/1)}
  end

  def parse_2(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.at(-1)
    |> String.split(",")
    |> Enum.with_index()
    |> Enum.reject(fn {x, _} -> x == "x" end)
    |> Enum.map(fn {v, idx} -> {String.to_integer(v), String.to_integer(v) - idx} end)
  end

  defp next_departure(id, time) do
    next_departure = (div(time, id) + 1) * id
    {id, next_departure, next_departure - time, id * (next_departure - time)}
  end

  defp earliest_bus({time, ids}) do
    ids
    |> Enum.map(&next_departure(&1, time))
    |> Enum.min_by(&elem(&1, 2))
  end
end
