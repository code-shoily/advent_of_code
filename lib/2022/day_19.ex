defmodule AdventOfCode.Y2022.Day19 do
  @moduledoc """
  --- Day 19: Not Enough Minerals ---
  Problem Link: https://adventofcode.com/2022/day/19
  Difficulty: xl
  Tags: optimization search concurrent slow
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 19)

  def run(input \\ input()) do
    blueprints = parse(input)

    run_1_task = Task.async(fn -> solve_1(blueprints) end)
    run_2_task = Task.async(fn -> solve_2(blueprints) end)

    {Task.await(run_1_task, :infinity), Task.await(run_2_task, :infinity)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      Regex.scan(~r/\d+/, line)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  def solve_1(blueprints) do
    blueprints
    |> Task.async_stream(
      fn [id | costs] ->
        id * max_geodes(costs, 24)
      end,
      timeout: :infinity
    )
    |> Enum.map(fn {:ok, val} -> val end)
    |> Enum.sum()
  end

  def solve_2(blueprints) do
    blueprints
    |> Enum.take(3)
    |> Task.async_stream(
      fn [_id | costs] ->
        max_geodes(costs, 32)
      end,
      timeout: :infinity
    )
    |> Enum.map(fn {:ok, val} -> val end)
    |> Enum.product()
  end

  defp max_geodes(costs, time) do
    # costs: [ore_robot_ore, clay_robot_ore, obs_robot_ore, obs_robot_clay, geode_robot_ore, geode_robot_obs]
    [r1o, r2o, r3o, r3c, r4o, r4s] = costs

    # Max robots needed for each resource
    max_ore = Enum.max([r1o, r2o, r3o, r4o])
    max_clay = r3c
    max_obs = r4s

    memo = %{}
    {result, _} = dfs(time, {1, 0, 0, 0}, {0, 0, 0, 0}, costs, {max_ore, max_clay, max_obs}, memo)
    result
  end

  defp dfs(0, _robots, {_o, _c, _s, g}, _costs, _max_robots, memo), do: {g, memo}

  defp dfs(time, robots, resources, costs, max_robots, memo) do
    {mo, mc, ms} = max_robots
    {r1, r2, r3, r4} = robots
    {o, c, s, g} = resources

    # Pruning: Capping resources
    # If we have more than (time * max_needed - robots * (time-1)), we can't spend it all.
    o = min(o, time * mo - r1 * (time - 1))
    c = min(c, time * mc - r2 * (time - 1))
    s = min(s, time * ms - r3 * (time - 1))

    state = {time, {r1, r2, r3, r4}, {o, c, s, g}}

    case Map.fetch(memo, state) do
      {:ok, result} ->
        {result, memo}

      :error ->
        [r1o, r2o, r3o, r3c, r4o, r4s] = costs

        # Decisions:
        {res, memo} =
          if o >= r4o and s >= r4s do
            # 1. Build Geode robot (highest priority)
            dfs(
              time - 1,
              {r1, r2, r3, r4 + 1},
              {o + r1 - r4o, c + r2, s + r3 - r4s, g + r4},
              costs,
              max_robots,
              memo
            )
          else
            # Obsidian robot
            {res3, memo} =
              if r3 < ms and o >= r3o and c >= r3c do
                dfs(
                  time - 1,
                  {r1, r2, r3 + 1, r4},
                  {o + r1 - r3o, c + r2 - r3c, s + r3, g + r4},
                  costs,
                  max_robots,
                  memo
                )
              else
                {0, memo}
              end

            # Clay robot
            {res2, memo} =
              if r2 < mc and o >= r2o do
                dfs(
                  time - 1,
                  {r1, r2 + 1, r3, r4},
                  {o + r1 - r2o, c + r2, s + r3, g + r4},
                  costs,
                  max_robots,
                  memo
                )
              else
                {0, memo}
              end

            # Ore robot
            {res1, memo} =
              if r1 < mo and o >= r1o do
                dfs(
                  time - 1,
                  {r1 + 1, r2, r3, r4},
                  {o + r1 - r1o, c + r2, s + r3, g + r4},
                  costs,
                  max_robots,
                  memo
                )
              else
                {0, memo}
              end

            # Wait
            {res0, memo} =
              dfs(time - 1, robots, {o + r1, c + r2, s + r3, g + r4}, costs, max_robots, memo)

            {Enum.max([res3, res2, res1, res0]), memo}
          end

        {res, Map.put(memo, state, res)}
    end
  end
end
