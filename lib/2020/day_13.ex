defmodule AdventOfCode.Y2020.Day13 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/13
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 13

  def run_1, do: input!() |> process() |> earliest_bus() |> result()
  def run_2, do: {:not_implemented, 2}

  def process(input \\ input!()) do
    [time, ids] = String.split(input, "\n", trim: true)

    {String.to_integer(time),
     ids |> String.split(",") |> Enum.reject(&(&1 == "x")) |> Enum.map(&String.to_integer/1)}
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
