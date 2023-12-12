defmodule AdventOfCode.Y2021.Day21 do
  @moduledoc """
  --- Day 21: Dirac Dice ---
  Problem Link: https://adventofcode.com/2021/day/21
  Difficulty: xl
  Tags: slow needs-improvement
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  require Integer

  def input, do: InputReader.read_from_file(2021, 21)

  def run(input \\ input()) do
    input = parse(input)
    task_1 = Task.async(fn -> run_1(input) end)
    task_2 = Task.async(fn -> run_2(input) end)

    {
      Task.await(task_1, :infinity),
      Task.await(task_2, :infinity)
    }
  end

  def run_1(input), do: input |> roll(1, 0) |> Tuple.product()

  def run_2(input) do
    {:ok, pid} = Agent.start_link(fn -> %{} end)
    initial_state = input

    play_multiversal(pid).(initial_state)
    |> then(fn {player_1, player_2} ->
      (player_1 > player_2 && player_1) || player_2
    end)
  end

  def parse(data) do
    data
    |> Transformers.lines()
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
          players
          |> play_multiversal()
          |> update_agent(agent, players)

        value ->
          value
      end
    end
  end

  defp update_agent(value, agent, players) do
    Agent.get_and_update(
      agent,
      fn state -> {value, Map.put(state, players, value)} end
    )
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
