defmodule AdventOfCode.Y2025.Day10 do
  @moduledoc """
  --- Day 10: Factory ---
  Problem Link: https://adventofcode.com/2025/day/10
  Difficulty: l
  Tags: bitwise dynamic-programming graph pathfinding
  """
  import Bitwise
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Pathfinding.AStar

  def input, do: InputReader.read_from_file(2025, 10)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    input
    |> Enum.map(&solve_machine_1/1)
    |> Enum.sum()
  end

  def run_2(input) do
    input
    |> Enum.map(fn m ->
      Process.put(:memo_p2, %{})
      res = solve_machine_2(m)
      Process.delete(:memo_p2)
      res
    end)
    |> Enum.sum()
  end

  defp solve_machine_1(m) do
    states = get_button_states(m.buttons_lists)
    target_mask = m.indicator

    states
    |> Map.get(target_mask, [])
    |> Enum.map(fn {_, presses} -> presses end)
    |> case do
      [] ->
        search_machine_1_yog(m)

      list ->
        Enum.min(list)
    end
  end

  defp search_machine_1_yog(%{indicator: target_mask, buttons_mask: buttons_mask}) do
    successors = fn mask ->
      Enum.map(buttons_mask, fn btn -> {bxor(mask, btn), 1} end)
    end

    heuristic = fn mask ->
      diff = bxor(mask, target_mask)
      count_set_bits(diff)
    end

    is_goal = fn mask -> mask == target_mask end

    case AStar.implicit_a_star(0, successors, is_goal, heuristic) do
      {:ok, cost} -> cost
      :error -> 0
    end
  end

  defp count_set_bits(v) do
    if v == 0 do
      0
    else
      1 + count_set_bits(v &&& v - 1)
    end
  end

  defp solve_machine_2(%{joltage: target_v, buttons_lists: buttons_lists}) do
    states = get_button_states(buttons_lists)
    solve_recursive(target_v, states)
  end

  defp solve_recursive(v, states) do
    if Enum.all?(v, &(&1 == 0)) do
      0
    else
      memo = Process.get(:memo_p2)

      if Map.has_key?(memo, v) do
        Map.get(memo, v)
      else
        res = compute_recursive(v, states)
        Process.put(:memo_p2, Map.put(memo, v, res))
        res
      end
    end
  end

  defp compute_recursive(v, states) do
    target_parity =
      v
      |> Enum.with_index()
      |> Enum.reduce(0, fn {val, idx}, acc ->
        if rem(val, 2) == 1, do: acc + (1 <<< idx), else: acc
      end)

    subsets = Map.get(states, target_parity, [])

    subsets
    |> Enum.flat_map(fn {inc_v, presses} ->
      next_v =
        Enum.zip_with(v, inc_v, fn v_val, inc_val ->
          v_val - inc_val
        end)

      if Enum.all?(next_v, &(&1 >= 0)) do
        next_v_div = Enum.map(next_v, &div(&1, 2))
        [presses + 2 * solve_recursive(next_v_div, states)]
      else
        []
      end
    end)
    |> case do
      # Infinity
      [] -> 1_000_000_000
      list -> Enum.min(list)
    end
  end

  defp get_button_states(buttons_lists) do
    m = length(buttons_lists)

    num_counters =
      if buttons_lists == [] do
        0
      else
        buttons_lists |> List.flatten() |> Enum.max() |> Kernel.+(1)
      end

    0..((1 <<< m) - 1)
    |> Enum.reduce(%{}, fn i, acc ->
      {parity, inc_v, presses} =
        Enum.reduce(0..(m - 1), {0, List.duplicate(0, num_counters), 0}, fn j, {p, iv, pr} ->
          if (i &&& 1 <<< j) != 0 do
            btn = Enum.at(buttons_lists, j)
            # Update parity mask
            p2 = Enum.reduce(btn, p, fn bit, p_acc -> bxor(p_acc, 1 <<< bit) end)
            # Update increment vector
            iv2 =
              Enum.reduce(btn, iv, fn bit, iv_acc -> List.update_at(iv_acc, bit, &(&1 + 1)) end)

            {p2, iv2, pr + 1}
          else
            {p, iv, pr}
          end
        end)

      Map.update(acc, parity, [{inc_v, presses}], &[{inc_v, presses} | &1])
    end)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    [[_, indicator_str]] = Regex.scan(~r/\[([.#]+)\]/, line)
    buttons_matches = Regex.scan(~r/\(([\d,]+)\)/, line)
    [[_, joltage_str]] = Regex.scan(~r/\{([\d,]+)\}/, line)

    indicator_list =
      indicator_str
      |> String.graphemes()
      |> Enum.map(fn
        "." -> 0
        "#" -> 1
      end)

    indicator_mask =
      indicator_list
      |> Enum.with_index()
      |> Enum.reduce(0, fn {val, idx}, acc ->
        if val == 1, do: acc + (1 <<< idx), else: acc
      end)

    buttons_lists =
      buttons_matches
      |> Enum.map(fn [_, m] ->
        m |> String.split(",") |> Enum.map(&String.to_integer/1)
      end)

    buttons_mask =
      buttons_lists
      |> Enum.map(fn indices ->
        Enum.reduce(indices, 0, fn idx, acc -> acc + (1 <<< idx) end)
      end)

    joltage =
      joltage_str
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    %{
      num_lights: length(indicator_list),
      indicator: indicator_mask,
      buttons_lists: buttons_lists,
      buttons_mask: buttons_mask,
      joltage: joltage
    }
  end
end
