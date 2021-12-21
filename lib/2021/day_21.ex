defmodule AdventOfCode.Y2021.Day21 do
  @moduledoc """
  --- Day 21: Dirac Dice ---
  Problem Link: https://adventofcode.com/2021/day/21
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 21

  require Integer

  def run_1, do: input!() |> parse() |> roll(1, 0) |> Tuple.product()

  def run_2 do
    {:ok, pid} = Agent.start_link(fn -> %{} end)
    initial_state = input!() |> parse()

    play_multiversal(pid).(initial_state)
    |> then(fn {player_1, player_2} ->
      (player_1 > player_2 && player_1) || player_2
    end)
  end

  def parse(data \\ input!()) do
    data
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split(":")
      |> List.last()
      |> String.trim()
      |> String.to_integer()
    end)
    |> List.to_tuple()
    |> then(fn {player_1, player_2} ->
      {{player_1, 0}, {player_2, 0}}
    end)
  end

  defp roll({{_, point_1}, {_, point_2}}, _, iter) when point_1 >= 1000, do: {iter, point_2}
  defp roll({{_, point_1}, {_, point_2}}, _, iter) when point_2 >= 1000, do: {iter, point_1}

  defp roll({{player_1, point_1}, {player_2, point_2}}, first, iter) do
    total = first + first + 1 + first + 2
    next = first + 3

    if Integer.is_odd(first) do
      {next_position, next_point} = next_move(total, {player_1, point_1})

      roll(
        {{next_position, next_point}, {player_2, point_2}},
        next,
        iter + 3
      )
    else
      {next_position, next_point} = next_move(total, {player_2, point_2})

      roll(
        {{player_1, point_1}, {next_position, next_point}},
        next,
        iter + 3
      )
    end
  end

  @dice_rolls [{3, 1}, {4, 3}, {5, 6}, {6, 7}, {7, 6}, {8, 3}, {9, 1}]
  defp play_multiversal({{_, _}, {_, point_2}}) when point_2 >= 21, do: {0, 1}

  defp play_multiversal({{player_1, point_1}, {player_2, point_2}}) do
    @dice_rolls
    |> Enum.reduce({0, 0}, fn {total, universes}, {wins_1, wins_2} ->
      {win_2, win_1} =
        play_multiversal({
          {player_2, point_2},
          next_move(total, {player_1, point_1})
        })

      {wins_1 + universes * win_1, wins_2 + universes * win_2}
    end)
  end

  defp play_multiversal(agent) when is_pid(agent) do
    fn players ->
      case Agent.get(agent, &Map.get(&1, players)) do
        nil ->
          value = play_multiversal(players)

          Agent.get_and_update(
            agent,
            fn state -> {value, Map.put(state, players, value)} end
          )

        value ->
          value
      end
    end
  end

  def next_move(total, {player, point}) do
    new_position =
      case rem(player + total, 10) do
        0 -> 10
        value -> value
      end

    {new_position, point + new_position}
  end
end
