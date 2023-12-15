defmodule AdventOfCode.Y2023.Day15 do
  @moduledoc """
  --- Day 15: Lens Library ---
  Problem Link: https://adventofcode.com/2023/day/15
  Difficulty: s
  Tags: hash ordered-map
  """
  alias AdventOfCode.Helpers.InputReader
  alias Aja.OrdMap

  def input, do: InputReader.read_from_file(2023, 15)

  def run(input \\ input()) do
    input = parse_1(input)
    {run_1(input), run_2(input)}
  end

  defp run_1(input), do: Enum.reduce(input, 0, &(&2 + hash(&1)))

  defp run_2(input) do
    input
    |> parse_2()
    |> Enum.reduce(%{}, fn
      {op, b, len}, acc ->
        hash = hash(b)

        case op do
          :add -> Map.update(acc, hash, OrdMap.new(%{b => len}), &OrdMap.put(&1, b, len))
          :remove -> Map.update(acc, hash, OrdMap.new(%{}), &OrdMap.drop(&1, [b]))
        end
    end)
    |> total_focus()
  end

  defp total_focus(map) do
    Enum.reduce(map, 0, fn {box, boxes}, acc ->
      acc +
        (boxes
         |> OrdMap.to_list()
         |> Enum.with_index(1)
         |> Enum.reduce(0, &(&2 + row_focus(&1, box))))
    end)
  end

  defp row_focus({{_, len}, slot}, box), do: (box + 1) * slot * len

  def parse_1(input \\ input()), do: String.split(input, ",", trim: true)

  def parse_2(commands) do
    Enum.map(commands, fn cmd ->
      case Regex.run(~r{([a-zA-Z]+)(=|-)([0-9]*)}, cmd) do
        [_, label, "=", len] -> {:add, label, String.to_integer(len)}
        [_, label, "-", ""] -> {:remove, label, 0}
      end
    end)
  end

  def hash(lst) do
    for ch <- String.to_charlist(lst), reduce: 0 do
      acc -> rem((acc + ch) * 17, 256)
    end
  end
end
