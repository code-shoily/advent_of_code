defmodule AdventOfCode.Y2015.Day07 do
  @moduledoc """
  --- Day 7: Some Assembly Required ---
  Problem Link: https://adventofcode.com/2015/day/7
  Difficulty: m
  Tags: graph dag logic-gates bitwise
  """
  import Bitwise
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 7)

  def run(input \\ input()) do
    wires = parse(input)

    values_1 = solve_circuit(wires)
    solution_1 = values_1["a"]

    wires_2 = Map.put(wires, "b", {:assign, [solution_1]})
    values_2 = solve_circuit(wires_2)
    solution_2 = values_2["a"]

    {solution_1, solution_2}
  end

  defp solve_circuit(wires) do
    edges =
      for {target, {_op, args}} <- wires,
          arg <- args,
          is_binary(arg),
          do: {arg, target, 1}

    graph =
      Map.keys(wires)
      |> Enum.reduce(Yog.directed(), fn wire, g -> Yog.add_node(g, wire, nil) end)
      |> Yog.add_edges!(edges)

    {:ok, sorted} = Yog.Traversal.topological_sort(graph)

    Enum.reduce(sorted, %{}, fn wire, values ->
      {op, args} = wires[wire]

      arg_vals =
        Enum.map(args, fn
          arg when is_integer(arg) -> arg
          arg when is_binary(arg) -> Map.get(values, arg)
        end)

      Map.put(values, wire, compute(op, arg_vals))
    end)
  end

  defp compute(:assign, [x]), do: x
  defp compute(:not, [x]), do: bnot(x) &&& 0xFFFF
  defp compute(:and, [x, y]), do: x &&& y &&& 0xFFFF
  defp compute(:or, [x, y]), do: (x ||| y) &&& 0xFFFF
  defp compute(:lshift, [x, y]), do: x <<< y &&& 0xFFFF
  defp compute(:rshift, [x, y]), do: x >>> y &&& 0xFFFF

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Map.new(fn line ->
      [lhs, rhs] = String.split(line, " -> ")
      {rhs, parse_op(lhs)}
    end)
  end

  defp parse_op(s) do
    case String.split(s) do
      ["NOT", x] -> {:not, [val(x)]}
      [x, "AND", y] -> {:and, [val(x), val(y)]}
      [x, "OR", y] -> {:or, [val(x), val(y)]}
      [x, "LSHIFT", y] -> {:lshift, [val(x), val(y)]}
      [x, "RSHIFT", y] -> {:rshift, [val(x), val(y)]}
      [x] -> {:assign, [val(x)]}
    end
  end

  defp val(s) do
    case Integer.parse(s) do
      {n, ""} -> n
      _ -> s
    end
  end
end
