defmodule AdventOfCode.Y2022.Day21 do
  @moduledoc """
  --- Day 21: Monkey Math ---
  Problem Link: https://adventofcode.com/2022/day/21
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 21)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def parse(data \\ input()) do
    for line <- Transformers.lines(data), into: Map.new() do
      parse_monkey(line)
    end
  end

  defp run_1(input) do
    yell(input, "root")
  end

  defp run_2(input) do
    input = Map.delete(input, "humn")
    {_, [m1, m2]} = Map.get(input, "root")
    inv(yell(input, m1), yell(input, m2))
  end

  defp parse_monkey(line) do
    [monkey, raw_yell] = String.split(line, ": ")

    {monkey,
     case String.split(raw_yell, " ") do
       [first, op, second] -> {op, [first, second]}
       [num] -> String.to_integer(num)
     end}
  end

  defp yell(tasks, monkey) do
    case tasks[monkey] do
      num when is_integer(num) ->
        num

      {op, [m1, m2]} ->
        case {yell(tasks, m1), yell(tasks, m2)} do
          {a, b} when is_integer(a) and is_integer(b) -> eval(op, [a, b])
          {a, b} -> {op, [a, b]}
        end

      nil ->
        nil
    end
  end

  defp inv(a, b) when is_integer(b), do: inv(b, a)
  defp inv(n, {"+", [a, b]}) when is_integer(a), do: inv(n - a, b)
  defp inv(n, {"+", [a, b]}), do: inv(n - b, a)
  defp inv(n, {"-", [a, b]}) when is_integer(a), do: inv(a - n, b)
  defp inv(n, {"-", [a, b]}), do: inv(b + n, a)
  defp inv(n, {"*", [a, b]}) when is_integer(a), do: inv(div(n, a), b)
  defp inv(n, {"*", [a, b]}), do: inv(div(n, b), a)
  defp inv(n, {"/", [a, b]}) when is_integer(a), do: inv(div(a, n), b)
  defp inv(n, {"/", [a, b]}), do: inv(b * n, a)
  defp inv(n, nil), do: n

  defp eval("+", [a, b]), do: a + b
  defp eval("-", [a, b]), do: a - b
  defp eval("*", [a, b]), do: a * b
  defp eval("/", [a, b]), do: div(a, b)
end
