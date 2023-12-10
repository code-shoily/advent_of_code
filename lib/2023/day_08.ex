defmodule AdventOfCode.Y2023.Day08 do
  @moduledoc """
  --- Day 8: Haunted Wasteland ---
  Problem Link: https://adventofcode.com/2023/day/8
  Difficulty: s
  Tags: arithmetic sequence
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 8)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1({seq, nodes}), do: steps(seq, nodes, "AAA", fn dst -> dst == "ZZZ" end)

  defp run_2({seq, nodes}) do
    for {src, _} <- nodes, String.ends_with?(src, "A"), reduce: 1 do
      lcm ->
        n = steps(seq, nodes, src, fn n -> String.ends_with?(n, "Z") end)
        div(n * lcm, Integer.gcd(n, lcm))
    end
  end

  def parse(data \\ input()) do
    [instructions | nodes] = Transformers.lines(data)

    {String.graphemes(instructions), parse_nodes(nodes)}
  end

  defp parse_nodes(nodes) do
    nodes
    |> Enum.map(fn node ->
      Regex.run(~r{(...) = \((...), (...)\)}, node, capture: :all_but_first)
    end)
    |> Enum.group_by(fn [node, _, _] -> node end, fn [_, left, right] -> {left, right} end)
    |> Map.new(fn {node, [lr]} -> {node, lr} end)
  end

  def steps(seq, nodes, src, p?) do
    do_steps = fn
      [], node, len, rec -> rec.(seq, node, len, rec)
      ["R" | xs], {_, r}, len, rec -> (p?.(r) && len + 1) || rec.(xs, nodes[r], len + 1, rec)
      ["L" | xs], {l, _}, len, rec -> (p?.(l) && len + 1) || rec.(xs, nodes[l], len + 1, rec)
    end

    do_steps.(seq, nodes[src], 0, do_steps)
  end
end
