defmodule AdventOfCode.Y2016.Day02 do
  @moduledoc """
  --- Day 2: Bathroom Security ---
  Problem Link: https://adventofcode.com/2016/day/2
  Difficulty: xs
  Tags: grid rust
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2016, 2)
  @initial_position_1 5
  @initial_position_2 [{"5", 2, 0}]

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    input
    |> parse_rule([], @initial_position_1)
    |> Enum.reverse()
    |> Enum.join()
    |> String.to_integer()
  end

  def run_2(input) do
    data = to_matrix_map()

    input
    |> Enum.reduce(@initial_position_2, fn cmd, [{_, x, y} | _] = acc ->
      [run_cmds(data, cmd, x, y) | acc]
    end)
    |> Enum.map_join(fn {v, _, _} -> v end)
    |> String.split_at(-1)
    |> elem(0)
    |> String.reverse()
  end

  defp parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
  end

  defp valid?(number), do: number <= 0 or number > 9

  defp next_1(cur, "L") when rem(cur, 3) != 1, do: cur - 1
  defp next_1(cur, "R") when rem(cur, 3) != 0, do: cur + 1
  defp next_1(cur, "U"), do: cur - 3
  defp next_1(cur, "D"), do: cur + 3
  defp next_1(_, _), do: -1

  defp parse_rule([], res, _), do: res

  defp parse_rule([h | t], [], position),
    do: parse_rule(t, [find(h, position, &next_1/2)], position)

  defp parse_rule([h | t], [x | _] = res, position),
    do: parse_rule(t, [find(h, x, &next_1/2) | res], position)

  defp find([], cur, _), do: cur

  defp find([h | t], cur, next_fn) do
    next_key = next_fn.(cur, h)

    if valid?(next_key) do
      find(t, cur, next_fn)
    else
      find(t, next_key, next_fn)
    end
  end

  @matrix [
    [nil, nil, "1", nil, nil],
    [nil, "2", "3", "4", nil],
    ["5", "6", "7", "8", "9"],
    [nil, "A", "B", "C", nil],
    [nil, nil, "D", nil, nil]
  ]

  def to_matrix_map(data \\ @matrix) do
    data
    |> Enum.with_index()
    |> Map.new(fn {val, idx} ->
      case val do
        val when is_list(val) -> {idx, to_matrix_map(val)}
        _ -> {idx, val}
      end
    end)
  end

  def run_cmds(data, [], x, y) do
    {data[x][y], x, y}
  end

  def run_cmds(data, ["D" | rest], x, y) do
    (data[x + 1][y] == nil && run_cmds(data, rest, x, y)) || run_cmds(data, rest, x + 1, y)
  end

  def run_cmds(data, ["U" | rest], x, y) do
    (data[x - 1][y] == nil && run_cmds(data, rest, x, y)) || run_cmds(data, rest, x - 1, y)
  end

  def run_cmds(data, ["L" | rest], x, y) do
    (data[x][y - 1] == nil && run_cmds(data, rest, x, y)) || run_cmds(data, rest, x, y - 1)
  end

  def run_cmds(data, ["R" | rest], x, y) do
    (data[x][y + 1] == nil && run_cmds(data, rest, x, y)) || run_cmds(data, rest, x, y + 1)
  end
end
