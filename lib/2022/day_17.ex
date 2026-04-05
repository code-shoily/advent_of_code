defmodule AdventOfCode.Y2022.Day17 do
  @moduledoc """
  --- Day 17: Pyroclastic Flow ---
  Problem Link: https://adventofcode.com/2022/day/17
  Difficulty: l
  Tags: cycle-detection simulation tetris
  """
  alias AdventOfCode.Helpers.InputReader

  @rocks [
    # -
    [{0, 0}, {1, 0}, {2, 0}, {3, 0}],
    # +
    [{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}],
    # L
    [{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}],
    # |
    [{0, 0}, {0, 1}, {0, 2}, {0, 3}],
    # #
    [{0, 0}, {1, 0}, {0, 1}, {1, 1}]
  ]
  @rock_count 5

  def input, do: InputReader.read_from_file(2022, 17)

  def run(input \\ input()) do
    jet_pattern = input |> String.trim() |> String.graphemes() |> List.to_tuple()
    jet_count = tuple_size(jet_pattern)

    {solve(jet_pattern, jet_count, 2022), solve(jet_pattern, jet_count, 1_000_000_000_000)}
  end

  defp solve(jets, jet_count, total_rocks) do
    simulate(0, 0, MapSet.new(), 0, %{}, total_rocks, jets, jet_count, 0)
  end

  defp simulate(
         rock_count,
         jet_idx,
         occupied,
         max_y,
         cache,
         total_rocks,
         jets,
         jet_count,
         added_height
       ) do
    if rock_count == total_rocks do
      max_y + added_height
    else
      state_key = {rem(rock_count, @rock_count), jet_idx, get_profile(occupied, max_y)}

      case cache[state_key] do
        {prev_rock_count, prev_max_y} when added_height == 0 ->
          rocks_in_cycle = rock_count - prev_rock_count
          height_in_cycle = max_y - prev_max_y
          remaining_rocks = total_rocks - rock_count
          cycles_to_skip = div(remaining_rocks, rocks_in_cycle)

          simulate(
            rock_count + cycles_to_skip * rocks_in_cycle,
            jet_idx,
            occupied,
            max_y,
            %{},
            total_rocks,
            jets,
            jet_count,
            cycles_to_skip * height_in_cycle
          )

        _ ->
          rock = Enum.at(@rocks, rem(rock_count, @rock_count))

          {new_occupied, new_jet_idx, new_max_y} =
            fall_rock(rock, jet_idx, occupied, max_y, jets, jet_count)

          new_cache =
            if added_height == 0, do: Map.put(cache, state_key, {rock_count, max_y}), else: cache

          simulate(
            rock_count + 1,
            new_jet_idx,
            new_occupied,
            new_max_y,
            new_cache,
            total_rocks,
            jets,
            jet_count,
            added_height
          )
      end
    end
  end

  defp fall_rock(rock, jet_idx, occupied, max_y, jets, jet_count) do
    start_pos = {2, max_y + 4}
    move_loop(rock, start_pos, jet_idx, occupied, jets, jet_count, max_y)
  end

  defp move_loop(rock, {rx, ry}, jet_idx, occupied, jets, jet_count, max_y) do
    jet = elem(jets, jet_idx)
    next_jet_idx = rem(jet_idx + 1, jet_count)
    dx = if jet == ">", do: 1, else: -1

    px = rx + dx
    px = if can_move?(rock, {px, ry}, occupied), do: px, else: rx

    py = ry - 1

    if can_move?(rock, {px, py}, occupied) do
      move_loop(rock, {px, py}, next_jet_idx, occupied, jets, jet_count, max_y)
    else
      new_occupied =
        Enum.reduce(rock, occupied, fn {dx, dy}, acc ->
          MapSet.put(acc, {px + dx, ry + dy})
        end)

      new_max_y = Enum.reduce(rock, max_y, fn {_, dy}, acc -> max(acc, ry + dy) end)

      pruned_occupied = purge_occupied(new_occupied, new_max_y)

      {pruned_occupied, next_jet_idx, new_max_y}
    end
  end

  defp can_move?(rock, {rx, ry}, occupied) do
    Enum.all?(rock, fn {dx, dy} ->
      x = rx + dx
      y = ry + dy
      x >= 0 and x <= 6 and y >= 1 and not MapSet.member?(occupied, {x, y})
    end)
  end

  defp get_profile(occupied, max_y) do
    for x <- 0..6 do
      peak =
        Enum.reduce(occupied, 0, fn {px, py}, acc ->
          if px == x, do: max(acc, py), else: acc
        end)

      peak - max_y
    end
  end

  defp purge_occupied(occupied, max_y) do
    Enum.filter(occupied, fn {_, y} -> y > max_y - 50 end) |> MapSet.new()
  end

  def parse(data \\ input()) do
    data |> String.trim()
  end
end
