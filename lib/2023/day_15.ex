defmodule AdventOfCode.Y2023.Day15 do
  @moduledoc """
  --- Day 15: Lens Library ---
  Problem Link: https://adventofcode.com/2023/day/15
  Difficulty: s
  Tags: hash sequence
  """
  alias AdventOfCode.Helpers.InputReader

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
      {:add, label, len}, boxes ->
        h = hash(label)

        Map.update(boxes, h, [{label, len}], fn lenses ->
          if List.keymember?(lenses, label, 0) do
            List.keyreplace(lenses, label, 0, {label, len})
          else
            lenses ++ [{label, len}]
          end
        end)

      {:remove, label}, boxes ->
        h = hash(label)
        Map.update(boxes, h, [], &List.keydelete(&1, label, 0))
    end)
    |> total_focus()
  end

  defp total_focus(boxes) do
    Enum.reduce(boxes, 0, fn {box_idx, lenses}, acc ->
      acc +
        Enum.reduce(Enum.with_index(lenses, 1), 0, fn {{_, len}, slot}, inner_acc ->
          inner_acc + (box_idx + 1) * slot * len
        end)
    end)
  end

  defp parse_1(input), do: String.split(input, ",", trim: true) |> Enum.map(&String.trim/1)

  defp parse_2(commands) do
    Enum.map(commands, fn cmd ->
      case Regex.run(~r{([a-zA-Z]+)(=|-)([0-9]*)}, cmd) do
        [_, label, "=", len] -> {:add, label, String.to_integer(len)}
        [_, label, "-", ""] -> {:remove, label}
      end
    end)
  end

  def hash(lst) do
    for ch <- String.to_charlist(lst), reduce: 0 do
      acc -> rem((acc + ch) * 17, 256)
    end
  end
end
