defmodule AdventOfCode.Y2021.Day21 do
  @moduledoc """
  --- Day 21: Dirac Dice ---
  Problem Link: https://adventofcode.com/2021/day/21
  Difficulty: xl
  Tags: dynamic-programming
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2021, 21)

  @spec run(binary()) :: {any(), any()}
  def run(input \\ input()) do
    input = parse(input)
    task_1 = Task.async(fn -> solve_1(input) end)
    task_2 = Task.async(fn -> solve_2(input) end)

    {
      Task.await(task_1, :infinity),
      Task.await(task_2, :infinity)
    }
  end

  def solve_1({{p1, _}, {p2, _}}) do
    roll({{p1, 0}, {p2, 0}}, 1, 0, 0)
  end

  def solve_2(input) do
    # input: {{p1_pos, 0}, {p2_pos, 0}}
    {wins1, wins2} = play_multiversal(input, %{}) |> elem(0)
    max(wins1, wins2)
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      Regex.run(~r/\d+$/, line) |> List.first() |> String.to_integer()
    end)
    |> then(fn [p1, p2] ->
      {{p1, 0}, {p2, 0}}
    end)
  end

  # Part 1: Deterministic Dice
  defp roll({{_, s1}, {_, s2}}, _dice, rolls, _turn) when s1 >= 1000, do: s2 * rolls
  defp roll({{_, s1}, {_, s2}}, _dice, rolls, _turn) when s2 >= 1000, do: s1 * rolls

  defp roll({{p1, s1}, {p2, s2}}, dice, rolls, turn) do
    move = rem(dice - 1, 100) + 1 + rem(dice, 100) + 1 + rem(dice + 1, 100) + 1

    if rem(turn, 2) == 0 do
      {pos, score} = next_move(move, {p1, s1})
      roll({{pos, score}, {p2, s2}}, rem(dice + 2, 100) + 1, rolls + 3, turn + 1)
    else
      {pos, score} = next_move(move, {p2, s2})
      roll({{p1, s1}, {pos, score}}, rem(dice + 2, 100) + 1, rolls + 3, turn + 1)
    end
  end

  # Part 2: Dirac Dice (1, 2, 3) -> Possible totals (3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)
  @dice_rolls [{3, 1}, {4, 3}, {5, 6}, {6, 7}, {7, 6}, {8, 3}, {9, 1}]

  defp play_multiversal({{_, _}, {_, p2}}, cache) when p2 >= 21 do
    # Player 2 (passive) won
    {{0, 1}, cache}
  end

  defp play_multiversal(state, cache) do
    case Map.get(cache, state) do
      nil ->
        {p1, p2} = state

        {wins, new_cache} =
          Enum.reduce(@dice_rolls, {{0, 0}, cache}, fn {total, universes}, {{w1, w2}, c} ->
            {swapped_wins, updated_cache} = play_multiversal({p2, next_move(total, p1)}, c)
            {swapped_w2, swapped_w1} = swapped_wins
            {{w1 + swapped_w1 * universes, w2 + swapped_w2 * universes}, updated_cache}
          end)

        {wins, Map.put(new_cache, state, wins)}

      value ->
        {value, cache}
    end
  end

  defp next_move(total, {player, point}) do
    new_position = rem(player + total - 1, 10) + 1
    {new_position, point + new_position}
  end
end
