defmodule AdventOfCode.Y2020.Day14 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/14
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 14

  def run_1, do: input!() |> process() |> initialize() |> result()
  def run_2, do: {:not_implemented, 2}
  def process(input), do: input |> String.split("\n") |> Enum.map(&parse/1)

  defp parse(line) do
    case line do
      "mask = " <> rest -> rest
      "mem" <> rest -> tl(Regex.run(~r/\[(\d+)\] = (.+)/, rest))
    end
  end

  defp initialize(commands) do
    Enum.reduce(commands, {%{}, nil}, fn
      x, {mem, _} when is_binary(x) -> {mem, x}
      [addr, val], {mem, mask} -> {update(mem, addr, {val, mask}), mask}
    end)
  end

  defp update(mem, addr, {val, mask}) do
    binstr(val)
    |> String.graphemes()
    |> Enum.zip(String.graphemes(mask))
    |> Enum.map_join(fn {a, b} -> (b == "X" && a) || b end)
    |> String.to_integer(2)
    |> (fn bits -> Map.put(mem, addr, bits) end).()
  end

  defp binstr(a), do: String.pad_leading(Integer.to_string(String.to_integer(a), 2), 36, "0")
  defp result({mem, _}), do: Enum.sum(Map.values(mem))
end
