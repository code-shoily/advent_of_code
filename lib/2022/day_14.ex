defmodule AdventOfCode.Y2022.Day14 do
  @moduledoc """
  --- Day 14: Regolith Reservoir ---
  Problem Link: https://adventofcode.com/2022/day/14
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @source {500, 0}

  def input(), do: InputReader.read_from_file(2022, 14)

  def run(input \\ input()) do
    input = parse(input)

    run_1 = Task.async(fn -> Enum.count(endless_sands(input)) end)
    run_2 = Task.async(fn -> Enum.count(floored_sands(input)) end)

    {Task.await(run_1, :infinity), Task.await(run_2, :infinity)}
  end

  def parse(path) do
    path
    |> Transformers.lines()
    |> Enum.flat_map(fn line ->
      line
      |> String.split(" -> ")
      |> Enum.map(fn coords ->
        coords
        |> Transformers.words(",")
        |> Enum.map(&String.to_integer/1)
      end)
      |> then(fn [_ | tail] = coords -> Enum.zip([coords, tail]) end)
      |> Enum.flat_map(fn
        {[a, x], [a, y]} -> Enum.map(x..y, &{a, &1})
        {[x, b], [y, b]} -> Enum.map(x..y, &{&1, b})
      end)
    end)
    |> MapSet.new()
  end

  defp endless_sands(input) do
    floor = Enum.max_by(input, &elem(&1, 1)) |> elem(1)
    Stream.unfold(input, &fall(&1, floor, @source))
  end

  defp floored_sands(input) do
    floor = Enum.max_by(input, &elem(&1, 1)) |> elem(1) |> Kernel.+(2)
    state = (-floor - 1)..(floor + 1) |> Stream.map(&{500 + &1, floor}) |> Enum.into(input)

    Stream.unfold(state, &fall(&1, floor, @source))
  end

  defp fall(state, floor, {x, y} = sand) do
    case MapSet.member?(state, @source) do
      false ->
        down = {x, y + 1}
        left = {x - 1, y + 1}
        right = {x + 1, y + 1}

        case Enum.find([down, left, right], &(!MapSet.member?(state, &1))) do
          nil -> {state, MapSet.put(state, sand)}
          {_, y} when y > floor -> nil
          air -> fall(state, floor, air)
        end

      true ->
        nil
    end
  end
end
