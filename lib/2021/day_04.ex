defmodule AdventOfCode.Y2021.Day04 do
  @moduledoc """
  --- Day 4: Giant Squid ---
  Problem Link: https://adventofcode.com/2021/day/4
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2021, 4)

  def run(input \\ input()) do
    input = parse(input)
    {play_1(input), play_2(input)}
  end

  def parse(data) do
    [order_data | board_data] = String.split(data, "\n\n", trim: true)
    {get_orders(order_data), get_boards(board_data)}
  end

  defp get_orders(data), do: data |> String.split(",") |> Enum.map(&String.to_integer/1)
  defp get_boards(data), do: data |> Enum.with_index() |> Enum.map(&get_board/1)

  defp get_board({data, idx}) do
    data
    |> String.split("\n")
    |> Enum.flat_map(fn line ->
      line
      |> String.split(" ", trim: true)
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.with_index()
    |> Enum.map(fn {key, idx} -> {key, {div(idx, 5), rem(idx, 5)}} end)
    |> Enum.into(%{})
    |> then(&board_state(&1, idx))
  end

  defp board_state(board, idx), do: {idx, board, [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]}

  def play_1({orders, board_states}) do
    orders
    |> Enum.reduce_while(board_states, fn order, acc ->
      acc = Enum.map(acc, &mark_board(&1, order))

      case get_winner(acc) do
        nil -> {:cont, acc}
        board -> {:halt, {score(board), order}}
      end
    end)
    |> Tuple.product()
  end

  def score(board), do: board |> Map.keys() |> Enum.sum()

  defp mark_board({idx, board, rows, cols} = state, order) do
    case Map.pop(board, order) do
      {{row, col}, board} ->
        {idx, board, List.update_at(rows, row, &(&1 + 1)), List.update_at(cols, col, &(&1 + 1))}

      {nil, _} ->
        state
    end
  end

  defp get_winner(board_states) do
    Enum.reduce_while(board_states, nil, fn {_, board, rows, cols}, _ ->
      5 in rows or (5 in cols && {:halt, board}) || {:cont, nil}
    end)
  end

  def play_2({orders, board_states}) do
    orders
    |> Enum.reduce({board_states, []}, fn order, {acc, winners} ->
      acc = Enum.map(acc, &mark_board(&1, order))

      {acc,
       [Enum.map(get_winners(acc), fn {idx, board} -> {idx, score(board), order} end) | winners]}
    end)
    |> elem(1)
    |> Enum.reverse()
    |> Enum.drop_while(&Enum.empty?/1)
    |> Enum.flat_map(&Function.identity/1)
    |> last_winner(Enum.count(board_states), MapSet.new())
    |> Tuple.product()
  end

  defp last_winner([{idx, score, order} | rest], size, map_set) do
    set = MapSet.put(map_set, idx)
    (Enum.count(set) == size && {score, order}) || last_winner(rest, size, set)
  end

  defp get_winners(board_states) do
    Enum.reduce(board_states, [], fn {idx, board, rows, cols}, acc ->
      ((5 in rows or 5 in cols) && [{idx, board} | acc]) || acc
    end)
  end
end
