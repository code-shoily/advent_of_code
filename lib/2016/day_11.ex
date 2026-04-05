defmodule AdventOfCode.Y2016.Day11 do
  @moduledoc """
  --- Day 11: Radioisotope Thermoelectric Generators ---
  Problem Link: https://adventofcode.com/2016/day/11
  Difficulty: l
  Tags: graph parsing simulation state-space
  """
  alias Yog.Traversal.Implicit

  def run(_input \\ "") do
    # Manual parse based on 2016_11.txt
    # Floor 1: strontium(S) G,M, plutonium(P) G,M
    # Floor 2: thulium(T) G, ruthenium(R) G,M, curium(C) G,M
    # Floor 3: thulium(T) M
    # Floor 4: -
    # State: {elevator_floor, sorted_pairs} where pair = {m_floor, g_floor}
    p1_initial = {1, sort_pairs([{1, 1}, {1, 1}, {3, 2}, {2, 2}, {2, 2}])}

    # Part 2: add Elerium and Dilithium pairs on floor 1
    p2_initial = {1, sort_pairs([{1, 1}, {1, 1} | elem(p1_initial, 1)])}

    {solve(p1_initial), solve(p2_initial)}
  end

  defp solve(initial_state) do
    Implicit.implicit_fold(
      from: initial_state,
      using: :breadth_first,
      initial: nil,
      successors_of: fn state -> generate_successors(state) end,
      with: fn
        _acc, {4, pairs}, meta ->
          if Enum.all?(pairs, fn {m, g} -> m == 4 and g == 4 end) do
            {:halt, meta.depth}
          else
            {:continue, nil}
          end

        _acc, _state, _meta ->
          {:continue, nil}
      end
    )
  end

  defp generate_successors({floor, pairs}) do
    # Pruning: Don't go back down to empty floors
    lowest_occupied =
      Enum.find(1..4, fn f ->
        Enum.any?(pairs, fn {m, g} -> m == f or g == f end)
      end) || 4

    next_floors = for f <- [floor - 1, floor + 1], f in 1..4, f >= lowest_occupied, do: f

    items =
      for {pair, idx} <- Enum.with_index(pairs),
          type <- [:m, :g],
          item_floor(pair, type) == floor,
          do: {idx, type}

    one_item = for item <- items, do: [item]
    two_items = for i1 <- items, i2 <- items, i1 < i2, do: [i1, i2]

    for nf <- next_floors,
        load <- one_item ++ two_items,
        new_pairs = move(pairs, nf, load),
        valid?(new_pairs),
        do: {nf, sort_pairs(new_pairs)}
  end

  defp item_floor({m, _g}, :m), do: m
  defp item_floor({_m, g}, :g), do: g

  defp move(pairs, next_floor, items_to_move) do
    Enum.reduce(items_to_move, pairs, fn {idx, type}, acc ->
      List.update_at(acc, idx, fn {m, g} ->
        if type == :m, do: {next_floor, g}, else: {m, next_floor}
      end)
    end)
  end

  defp sort_pairs(pairs), do: Enum.sort(pairs)

  defp valid?(pairs) do
    Enum.all?(1..4, fn floor ->
      has_generator = Enum.any?(pairs, fn {_m, g} -> g == floor end)

      if has_generator do
        Enum.all?(pairs, fn {m, g} ->
          if m == floor, do: g == floor, else: true
        end)
      else
        true
      end
    end)
  end
end
